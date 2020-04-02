import util.Random

object ControlFunction
{
    val rnd = new Random()
    val RotationDelay = 16

    def forMaster(bot: Bot) {

        val currDir = bfsWithAvoid(bot)

                // can we spawn a mini-bot? We don't do it more often than every 10 cycles.
        val lastSpawnTime = bot.inputAsIntOrElse("lastSpawnTime", 0)
        if((bot.time - lastSpawnTime) > RotationDelay ) {
            // yes, we can (try to) spawn a mini-bot
            if(bot.energy > 100) {
                bot
                .spawn(bot.view.relPosFromIndex(currDir), "mood" -> "Minion")
                .set("lastSpawnTime" -> bot.time)
                .say("Off you go!")
                .status("Circling...")
            } else {
                bot.status("Low Energy")
            }
        } else {
            bot.status("Waiting...")
        }

        val (directionValue, nearestEnemyMaster, nearestEnemySlave) = analyzeViewAsMaster(bot.view)

        val dontFireAggressiveMissileUntil = bot.inputAsIntOrElse("dontFireAggressiveMissileUntil", -1)
        val dontFireDefensiveMissileUntil = bot.inputAsIntOrElse("dontFireDefensiveMissileUntil", -1)

        if(dontFireAggressiveMissileUntil < bot.time && bot.energy > 100) { // fire attack missile?
            nearestEnemyMaster match {
                case None =>            // no-on nearby
                case Some(relPos) =>    // a master is nearby
                    val unitDelta = relPos.signum
                    val remainder = relPos - unitDelta // we place slave nearer target, so subtract that from overall delta
                    bot.spawn(unitDelta, "mood" -> "Aggressive", "target" -> remainder)
                    bot.set("dontFireAggressiveMissileUntil" -> (bot.time + relPos.stepCount + 1))
            }
        }
        else
        if(dontFireDefensiveMissileUntil < bot.time && bot.energy > 100) { // fire defensive missile?
            nearestEnemySlave match {
                case None =>            // no-on nearby
                case Some(relPos) =>    // an enemy slave is nearby
                    if(relPos.stepCount < 8) {
                        // this one's getting too close!
                        val unitDelta = relPos.signum
                        val remainder = relPos - unitDelta // we place slave nearer target, so subtract that from overall delta
                        bot.spawn(unitDelta, "mood" -> "Defensive", "target" -> remainder)
                        bot.set("dontFireDefensiveMissileUntil" -> (bot.time + relPos.stepCount + 1))
                    }
            }
        }
    }

    def threeBlocks(bot: Bot, currDir: Int): Int = {
        if(currDir == -1)
            return -1
        var arr1 = bot.view.twoNeighbours(currDir)
        var arr = Array(currDir, bot.view.absIndexFromRelIndex(bot.view.indexFromRelPos(arr1._1)), bot.view.absIndexFromRelIndex(bot.view.indexFromRelPos(arr1._2)))
        arr = arr.filter(p => bot.view.notEnemy(p))
        if(!arr.isEmpty)
            if(currDir == arr(0)) return arr(0) else return arr(rnd.nextInt(arr.length))
        return -1
    }

    // Try to avoid enemies, walls, own minibots while searching for good food
    def bfsWithAvoid(bot: Bot): Int = {

        var currDir = bfsImproved(bot)
        currDir = threeBlocks(bot, currDir)
        var lastDir = bot.inputAsIntOrElse("lastDirection", -1)

        if(currDir == -1) {

            if (lastDir != -1) { 
                lastDir = threeBlocks(bot, lastDir)
            }

            if (lastDir == -1) {
                val arr = bot.view.notEnemiesAround(bot.view.indexFromAbsPos(bot.view.center))
                lastDir = arr(0)
                bot.set("lastDirection" -> lastDir)
            }
            bot.move(bot.view.relPosFromIndex(lastDir))
            return lastDir
        }
        else {
            bot.move(bot.view.relPosFromIndex(currDir))
            lastDir = currDir
            bot.set("lastDirection" -> lastDir)
            return currDir
        }
    }

    // Find Shortest Direction
    def bfsImproved(bot: Bot) : Int = {
        var cells = bot.view.cells
        var visited = bot.view.notEnemies
        var previous = new Array[Int](bot.view.size * bot.view.size)
        var q1 = bot.view.neighbours(bot.view.indexFromRelPos(XY(0,0)))
        var answer = -1

        visited(bot.view.indexFromRelPos(XY(0,0))) = false
        previous(bot.view.indexFromRelPos(XY(0,0))) = -1
        for(i <- 0 until q1.length) {
            previous(q1(i)) = bot.view.indexFromAbsPos(bot.view.center)
            visited(q1(i)) = false

            if(cells(q1(i)) == 'B' || cells(q1(i)) == 'P') {
                 answer = q1(i)
                 return answer
            }
        }

        if (answer == -1) {
            var break = true
            while(!q1.isEmpty && break) {
                var q2 = bot.view.neighbours(q1(0))
                q2 = q2.filter(p => visited(p))

                for(i <- 0 until q2.length) {
                    previous(q2(i)) = q1(0)
                    visited(q2(i)) = false

                    if(cells(q2(i)) == 'B' || cells(q2(i)) == 'P') {
                        answer = q2(i)
                        break = false
                    }
                }
                q1 = q1.zipWithIndex.filter(_._2 != 0).map(_._1)
                q1 = q1 ++ q2
            }

            if(answer != -1) {
                var dir = directionBfs(previous, answer, previous(answer))
                return dir
            }
        }
        return -1
    }

    def directionBfs(previousArray: Array[Int], previous: Int, current: Int) : Int = {
        if(previousArray(current) != -1) {
            val previous1 = current
            val current1 = previousArray(current)
            return directionBfs(previousArray, previous1, current1)
        }
        else { 
            return previous
        }
    }

    def forSlave(bot: MiniBot) {
        bot.inputOrElse("mood", "Lurking") match {
            //case "Minion" => bfsWithAvoid(bot)
            case "Minion" => 
                if(bot.energy > 1000) {
                    bot.view.offsetToNearest('M') match {
                        case Some(delta: XY) => bot.move(delta.signum)
                        case None => bfsWithAvoid(bot)
                    } 
                }
                else 
                    bfsWithAvoid(bot)
            case "Aggressive" => reactAsAggressiveMissile(bot)
            case "Defensive" => reactAsDefensiveMissile(bot)
            case s: String => bot.log("unknown mood: " + s)
        }
    }


    def reactAsAggressiveMissile(bot: MiniBot) {
        bot.view.offsetToNearest('m') match {
            case Some(delta: XY) =>
                // another master is visible at the given relative position (i.e. position delta)

                // close enough to blow it up?
                if(delta.length <= 2) {
                    // yes -- blow it up!
                    bot.explode(4)

                } else {
                    // no -- move closer!
                    bot.move(delta.signum)
                    bot.set("rx" -> delta.x, "ry" -> delta.y)
                }
            case None =>
                // no target visible -- follow our targeting strategy
                val target = bot.inputAsXYOrElse("target", XY.Zero)

                // did we arrive at the target?
                if(target.isNonZero) {
                    // no -- keep going
                    val unitDelta = target.signum // e.g. CellPos(-8,6) => CellPos(-1,1)
                    bot.move(unitDelta)

                    // compute the remaining delta and encode it into a new 'target' property
                    val remainder = target - unitDelta // e.g. = CellPos(-7,5)
                    bot.set("target" -> remainder)
                } else {
                    // yes -- but we did not detonate yet, and are not pursuing anything?!? => switch purpose
                    bot.set("mood" -> "Lurking", "target" -> "")
                    bot.say("Lurking")
                }
        }
    }


    def reactAsDefensiveMissile(bot: MiniBot) {
        bot.view.offsetToNearest('s') match {
            case Some(delta: XY) =>
                // another slave is visible at the given relative position (i.e. position delta)
                // move closer!
                bot.move(delta.signum)
                bot.set("rx" -> delta.x, "ry" -> delta.y)

            case None =>
                // no target visible -- follow our targeting strategy
                val target = bot.inputAsXYOrElse("target", XY.Zero)

                // did we arrive at the target?
                if(target.isNonZero) {
                    // no -- keep going
                    val unitDelta = target.signum // e.g. CellPos(-8,6) => CellPos(-1,1)
                    bot.move(unitDelta)

                    // compute the remaining delta and encode it into a new 'target' property
                    val remainder = target - unitDelta // e.g. = CellPos(-7,5)
                    bot.set("target" -> remainder)
                } else {
                    // yes -- but we did not annihilate yet, and are not pursuing anything?!? => switch purpose
                    bot.set("mood" -> "Lurking", "target" -> "")
                    bot.say("Lurking")
                }
        }
    }


    /** Analyze the view, building a map of attractiveness for the 45-degree directions and
      * recording other relevant data, such as the nearest elements of various kinds.
      */
    def analyzeViewAsMaster(view: View) = {
        val directionValue = Array.ofDim[Double](8)
        var nearestEnemyMaster: Option[XY] = None
        var nearestEnemySlave: Option[XY] = None

        val cells = view.cells
        val cellCount = cells.length
        for(i <- 0 until cellCount) {
            val cellRelPos = view.relPosFromIndex(i)
            if(cellRelPos.isNonZero) {
                val stepDistance = cellRelPos.stepCount
                val value: Double = cells(i) match {
                    case 'm' => // another master: not dangerous, but an obstacle
                        nearestEnemyMaster = Some(cellRelPos)
                        if(stepDistance < 2) -1000 else 0

                    case 's' => // another slave: potentially dangerous?
                        nearestEnemySlave = Some(cellRelPos)
                        -100 / stepDistance

                    case 'S' => // out own slave
                        0.0

                    case 'B' => // good beast: valuable, but runs away
                        if(stepDistance == 1) 600
                        else if(stepDistance == 2) 300
                        else (150 - stepDistance * 15).max(10)

                    case 'P' => // good plant: less valuable, but does not run
                        if(stepDistance == 1) 500
                        else if(stepDistance == 2) 300
                        else (150 - stepDistance * 10).max(10)

                    case 'b' => // bad beast: dangerous, but only if very close
                        if(stepDistance < 4) -400 / stepDistance else -50 / stepDistance

                    case 'p' => // bad plant: bad, but only if I step on it
                        if(stepDistance < 2) -1000 else 0

                    case 'W' => // wall: harmless, just don't walk into it
                        if(stepDistance < 2) -1000 else 0

                    case _ => 0.0
                }
                val direction45 = cellRelPos.toDirection45
                directionValue(direction45) += value
            }
        }
        (directionValue, nearestEnemyMaster, nearestEnemySlave)
    }
}



// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

class ControlFunctionFactory {
    def create = (input: String) => {
        val (opcode, params) = CommandParser(input)
        opcode match {
            case "React" =>
                val bot = new BotImpl(params)
                if( bot.generation == 0 ) {
                    ControlFunction.forMaster(bot)
                } else {
                    ControlFunction.forSlave(bot)
                }
                bot.toString
            case _ => "" // OK
        }
    }
}


// -------------------------------------------------------------------------------------------------


trait Bot {
    // inputs
    def inputOrElse(key: String, fallback: String): String
    def inputAsIntOrElse(key: String, fallback: Int): Int
    def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY
    def view: View
    def energy: Int
    def time: Int
    def generation: Int

    // outputs
    def move(delta: XY) : Bot
    def say(text: String) : Bot
    def status(text: String) : Bot
    def spawn(offset: XY, params: (String,Any)*) : Bot
    def set(params: (String,Any)*) : Bot
    def log(text: String) : Bot
}

trait MiniBot extends Bot {
    // inputs
    def offsetToMaster: XY

    // outputs
    def explode(blastRadius: Int) : Bot
}


case class BotImpl(inputParams: Map[String, String]) extends MiniBot {
    // input
    def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)
    def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)
    def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

    val view = View(inputParams("view"))
    val energy = inputParams("energy").toInt
    val time = inputParams("time").toInt
    val generation = inputParams("generation").toInt
    def offsetToMaster = inputAsXYOrElse("master", XY.Zero)


    // output

    private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
    private var commands = ""                           // holds all other commands
    private var debugOutput = ""                        // holds all "Log()" output

    /** Appends a new command to the command string; returns 'this' for fluent API. */
    private def append(s: String) : Bot = { commands += (if(commands.isEmpty) s else "|" + s); this }

    /** Renders commands and stateParams into a control function return string. */
    override def toString = {
        var result = commands
        if(!stateParams.isEmpty) {
            if(!result.isEmpty) result += "|"
            result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
        }
        if(!debugOutput.isEmpty) {
            if(!result.isEmpty) result += "|"
            result += "Log(text=" + debugOutput + ")"
        }
        result
    }

    def log(text: String) = { debugOutput += text + "\n"; this }
    def move(direction: XY) = append("Move(direction=" + direction + ")")
    def say(text: String) = append("Say(text=" + text + ")")
    def status(text: String) = append("Status(text=" + text + ")")
    def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
    def spawn(offset: XY, params: (String,Any)*) =
        append("Spawn(direction=" + offset +
            (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
            ")")
    def set(params: (String,Any)*) = { stateParams ++= params; this }
    def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}


// -------------------------------------------------------------------------------------------------


/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {
    /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
    def apply(command: String): (String, Map[String, String]) = {
        /** "key=value" => ("key","value") */
        def splitParameterIntoKeyValue(param: String): (String, String) = {
            val segments = param.split('=')
            (segments(0), if(segments.length>=2) segments(1) else "")
        }

        val segments = command.split('(')
        if( segments.length != 2 )
            throw new IllegalStateException("invalid command: " + command)
        val opcode = segments(0)
        val params = segments(1).dropRight(1).split(',')
        val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
        (opcode, keyValuePairs)
    }
}


// -------------------------------------------------------------------------------------------------


/** Utility class for managing 2D cell coordinates.
  * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
  * The direction (1,-1) points right and up.
  */
case class XY(x: Int, y: Int) {
    override def toString = x + ":" + y

    def isNonZero = x != 0 || y != 0
    def isZero = x == 0 && y == 0
    def isNonNegative = x >= 0 && y >= 0

    def updateX(newX: Int) = XY(newX, y)
    def updateY(newY: Int) = XY(x, newY)

    def addToX(dx: Int) = XY(x + dx, y)
    def addToY(dy: Int) = XY(x, y + dy)

    def +(pos: XY) = XY(x + pos.x, y + pos.y)
    def -(pos: XY) = XY(x - pos.x, y - pos.y)
    def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

    def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
    def length: Double = math.sqrt(x * x + y * y) // Phythagorean

    def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
    def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

    def signum = XY(x.signum, y.signum)

    def negate = XY(-x, -y)
    def negateX = XY(-x, y)
    def negateY = XY(x, -y)

    /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
    def toDirection45: Int = {
        val unit = signum
        unit.x match {
            case -1 =>
                unit.y match {
                    case -1 =>
                        if(x < y * 3) Direction45.Left
                        else if(y < x * 3) Direction45.Up
                        else Direction45.UpLeft
                    case 0 =>
                        Direction45.Left
                    case 1 =>
                        if(-x > y * 3) Direction45.Left
                        else if(y > -x * 3) Direction45.Down
                        else Direction45.LeftDown
                }
            case 0 =>
                unit.y match {
                    case 1 => Direction45.Down
                    case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
                    case -1 => Direction45.Up
                }
            case 1 =>
                unit.y match {
                    case -1 =>
                        if(x > -y * 3) Direction45.Right
                        else if(-y > x * 3) Direction45.Up
                        else Direction45.RightUp
                    case 0 =>
                        Direction45.Right
                    case 1 =>
                        if(x > y * 3) Direction45.Right
                        else if(y > x * 3) Direction45.Down
                        else Direction45.DownRight
                }
        }
    }

    def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
    def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
    def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
    def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


    def wrap(boardSize: XY) = {
        val fixedX = if(x < 0) boardSize.x + x else if(x >= boardSize.x) x - boardSize.x else x
        val fixedY = if(y < 0) boardSize.y + y else if(y >= boardSize.y) y - boardSize.y else y
        if(fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
    }
}


object XY {
    /** Parse an XY value from XY.toString format, e.g. "2:3". */
    def apply(s: String) : XY = { val a = s.split(':'); XY(a(0).toInt,a(1).toInt) }

    val Zero = XY(0, 0)
    val One = XY(1, 1)

    val Right     = XY( 1,  0)
    val RightUp   = XY( 1, -1)
    val Up        = XY( 0, -1)
    val UpLeft    = XY(-1, -1)
    val Left      = XY(-1,  0)
    val LeftDown  = XY(-1,  1)
    val Down      = XY( 0,  1)
    val DownRight = XY( 1,  1)

    def fromDirection45(index: Int): XY = index match {
        case Direction45.Right => Right
        case Direction45.RightUp => RightUp
        case Direction45.Up => Up
        case Direction45.UpLeft => UpLeft
        case Direction45.Left => Left
        case Direction45.LeftDown => LeftDown
        case Direction45.Down => Down
        case Direction45.DownRight => DownRight
    }

    def fromDirection90(index: Int): XY = index match {
        case Direction90.Right => Right
        case Direction90.Up => Up
        case Direction90.Left => Left
        case Direction90.Down => Down
    }

    def apply(array: Array[Int]): XY = XY(array(0), array(1))
}


object Direction45 {
    val Right = 0
    val RightUp = 1
    val Up = 2
    val UpLeft = 3
    val Left = 4
    val LeftDown = 5
    val Down = 6
    val DownRight = 7
}


object Direction90 {
    val Right = 0
    val Up = 1
    val Left = 2
    val Down = 3
}


// -------------------------------------------------------------------------------------------------


case class View(cells: String) {
    val size = math.sqrt(cells.length).toInt
    val center = XY(size / 2, size / 2)

    def apply(relPos: XY) = cellAtRelPos(relPos)

    def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
    def absPosFromIndex(index: Int) = XY(index % size, index / size)
    def absPosFromRelPos(relPos: XY) = relPos + center
    def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))
    def absIndexFromRelIndex(index: Int) = indexFromAbsPos(absPosFromRelPos(relPosFromIndex(index)))

    def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
    def relPosFromAbsPos(absPos: XY) = absPos - center
    def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
    def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

    def offsetToNearest(c: Char) = {
        val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
        if( matchingXY.isEmpty )
            None
        else {
            val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
            Some(nearest)
        }
    }

    def notEnemy(i: Int): Boolean = {
        if(cells(i) != 'W' && cells(i) != 'm' && cells(i) != 's' && cells(i) != 'p' && cells(i) != 'b' && cells(i) != '?' && cells(i) != 'S')
            return true
        return false
    }

    val notEnemies = cells.map(p => p != 'W' && p != 'm' && p != 's' && p != 'p' && p != 'b' && p != '?' && p != 'S').toArray

    def neighbours(index: Int) : Array[Int] = {

        val pos = absPosFromIndex(index)

        Array(XY(0, 1), XY(0, -1), XY(1, 0), XY(-1, 0), XY(-1, 1), XY(1, -1), XY(-1, -1), XY(1, 1))
        .map(_ + pos)
        .filter(p => p.x < size && p.y < size && p.x >= 0 && p.y >= 0)
        .map(indexFromAbsPos(_))
        
    }

    def twoNeighbours(index: Int) = {
        relPosFromAbsPos(absPosFromIndex(index)) match {
            case XY.UpLeft => (XY.Up, XY.Left)
            case XY.Up => (XY.RightUp, XY.UpLeft)
            case XY.RightUp => (XY.Right, XY.Up)
            case XY.Right => (XY.DownRight, XY.RightUp)
            case XY.DownRight => (XY.Down, XY.Right)
            case XY.Down => (XY.LeftDown, XY.DownRight)
            case XY.LeftDown => (XY.Left, XY.Down)
            case XY.Left => (XY.UpLeft, XY.LeftDown)
        }
    }
    
    def notEnemiesAround(index : Int) = {
        neighbours(index)
        .filter(p => cells(p) != 'W' && cells(p) != 'm' && cells(p) != 's' && cells(p) != 'p' && cells(p) != 'b' && cells(p) != '?' && cells(p) != 'S')
        .toArray
    }
}

