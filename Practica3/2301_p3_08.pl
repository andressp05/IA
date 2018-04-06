% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space


%------- Ejercicio1 ----------
% pertenece
pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

% pertenece_m
pertenece_m(X,[X|_]) :- X\=[_|_].
pertenece_m(X,[A|_]) :- pertenece_m(X, A).
pertenece_m(X,[_|Rs]) :- pertenece_m(X, Rs).

% Pruebas
% pertenece_m(X, [2,[1,3],[1,[4,5]]]).
% pertenece_m([1,3], [2,[1,3],[1,[4,5]])
%------------------------------


% ------ Ejercicio2-----------
%Concatenar dos listas
concat([],L,L).
concat([X|L1],L2,[X|L]):- concat(L1,L2,L).
%concat([], [1, 2, 3], L)

%Inversa de una lista
invierte([], []).
invierte([X|L],L1) :- invierte(L,L2), concat(L2,[X],L1).
%------------------------------

