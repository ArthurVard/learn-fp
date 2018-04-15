n_fact(0,1).
n_fact(N,F) :-
    N #> 0,
    F #= F0*N,
    N0 #= N -1,
    n_fact(N0,F0).


%% ? - n_fact(N,F).
