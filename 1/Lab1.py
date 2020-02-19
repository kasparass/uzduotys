class DNA:

    Count = 0                                   # Similarity count
    Text = ''                                   # Similarity text
    Lcount = 0                                  # Iteration count

    def __init__(self, S, RS, resultT):
        self.S = S                              # Input String
        self.RS = RS                            # Reversed input String
        self.T = len(resultT) * 'X'             # Current output String
        self.RT = len(resultT) * 'X'            # Current output reversed String
        self.resultT = resultT                  # Result which changes
        self.resultT2 = resultT                 # Result which doesn't change

    def minimal(self):
        # notChanged = self.T

        while self.resultT2 != self.T:
            notChanged = self.T

            self.Text = ''
            self.Count = 0

            self.similarity(str(self.S))
            self.similarity(str(self.RS))
            self.similarity(str(self.T))
            self.similarity(str(self.RT))

            position = self.resultT.find(self.Text)
            i = self.findAll()

            for j in i:
                if j == 0:
                    self.resultT[j + self.Count] == 'Y'
                    position = j
                    break
                if i != 0 and j + self.Count < len(self.resultT):
                    if self.resultT[j-1] == 'Y' or self.resultT[j + self.Count] == 'Y':
                        position = j
                        break

            tList = list(self.T)
            resultTList = list(self.resultT)

            for j in range(self.Count):
                resultTList[position + j] = 'Y'
                tList[position + j] = self.Text[j]

            self.T = "".join(tList)
            self.resultT = "".join(resultTList)
            self.RT = self.T[::-1]
            self.Lcount += 1
            if notChanged == self.T:
                self.Lcount = -1
                break

        if self.Lcount > 0:
            # print('Loop Count')
            print(self.Lcount)
        else:
            print('impossible')

    def findAll(self):
        i = 0
        listI = []

        while i != -1:
            i = self.resultT.find(self.Text, i + 1)
            if i != -1:
                listI.append(i)
        return listI

    def similarity(self, text):
        while text != '':
            for x in range(len(text)):
                text1 = text[0:len(text) - x]
                text2 = text[len(text) - x:len(text)]
                if self.resultT.find(text1) != -1:
                    if self.Count < len(text1):
                        self.Count = len(text1)
                        self.Text = text1
                if self.resultT.find(text2) != -1:
                    if self.Count < len(text2):
                        self.Count = len(text2)
                        self.Text = text2
            text = self.remove(text, 0)
            text = self.remove(text, len(text) - 1)
        # print('Rezultatas')
        # print(self.Count)
        # print(self.Text)

    def remove(self, g, i):
        t = ''
        count = 0
        for x in g:
            if count != i:
                t += x
            count += 1
        return t

reader = open('data.txt', 'r')
loop = reader.readline()
loop = int(loop)
S = []
RS = []
resultT = []

for i in range(loop):
    S.append(reader.readline().rstrip('\n'))
    RS.append(S[i][::-1])
    resultT.append(reader.readline().rstrip('\n'))

for i in range(loop):
    p1 = DNA(S[i], RS[i], resultT[i])
    p1.minimal()

# S = 'sdsada'
# RS = S[::-1]
# resultT = 'dsaaadsassa'

# S = 'ACGT'
# RS = S[::-1]
# resultT = 'TCGATCGA'

# S = 'A'
# RS = S[::-1]
# resultT = 'AAAAAAAAAAAAAAAAAA'

# S = 'A'
# RS = S[::-1]
# resultT = 'C'

# S = 'AGCAT'
# RS = S[::-1]
# resultT = 'ACACCACAT'
#
# p1 = DNA(S,RS,resultT)
# p1.minimal()