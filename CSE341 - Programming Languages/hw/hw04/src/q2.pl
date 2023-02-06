% knowladge base

schedule(istanbul, ankara, 1).
schedule(istanbul, izmir, 2).
schedule(istanbul, rize, 4).
schedule(istanbul, mardin, 15).
schedule(ankara, rize, 5).
schedule(ankara, van, 4).
schedule(ankara, diyarbakir, 8).
schedule(ankara, izmir, 6).
schedule(izmir, antalya, 2).
schedule(izmir, manisa, 4).
schedule(van, gaziantep, 3).
schedule(antalya, erzincan, 3).
schedule(antalya, diyarbakir, 4).
schedule(erzincan, canakkale, 6).

% rules

path(S, D, C) :-
    schedule(S, D, C).

path(S, D, C) :-
    schedule(D, S, C).
    
find_path(S, D, C, Visited) :-
    path(S, D, C).

find_path(S, D, C, Visited) :- 
    path(S, B, C1),                     % there is a path between S and B with cost C1
    \+ (member(B, Visited)),            % B is not visited before
    find_path(B, D, C2, [B | Visited]),
    C is C1 + C2,
    S \= D. 

% there exist a route between X and Y with cost C
connection(X, Y, C) :-
    find_path(X, Y, C, [X]).