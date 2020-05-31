count_letters([], 0).
count_letters([H|T], N) :-
    count_letters(T, X),
    (number(H) -> N is X; N is X + 1).


sum_integers([], 0).
sum_integers([H|T], N) :-
    sum_integers(T, X1),
    (
    is_list(H) -> (sum_integers(H, X2), N is X1 + X2); 
    integer(H) -> N is X1 + H; N = X1
    ).
	