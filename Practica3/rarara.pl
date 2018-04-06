%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    Lab assignment 3: Prolog
%%    LAB GROUP: 2301
%%    Couple:  08
%%    Author 1: Andres Salas
%%    Author 2: Ricardo Riol
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 1
%  Comprueba si un elemento está en la lista o alguna de las sublistas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PERTENECE
%  Entrada:
%    X: elemento a buscar
%    L: lista donde buscar el elemento
%  Salida:
%    A specified list with Navigate-white-whole as name,
pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

% PERTENECE_M
%  Entrada:
%    X: elemento a buscar
%    L: lista con sublistas.
%  Salida:
%    A specified list with Navigate-white-whole as name,
pertenece_m(X,[X|_]) :- X\=[_|_].
pertenece_m(X,[A|_]) :- pertenece_m(X, A).
pertenece_m(X,[_|Rs]) :- pertenece_m(X, Rs).

% Pruebas
% pertenece_m(X, [2,[1,3],[1,[4,5]]]).
% pertenece_m([1,3], [2,[1,3],[1,[4,5]])
%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 2
%  Inserta un par de elementos en una lista de pares ordenados
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CONCAT
%  Entrada:
%    X: elemento a buscar
%    L: lista con sublistas.
%  Salida:
%    A specified list with Navigate-white-whole as name,

%concat([],L,L).
concat([X|L1],L2,[X|L]):- concat(L1,L2,L).
%concat([], [1, 2, 3], L)

% INVIERTE
%  Entrada:
%    L: 
%    R: 
%  Salida:
%    A specified list with Navigate-white-whole as name,
invierte([], []).
invierte([X|L],L1) :- invierte(L,L2), concat(L2,[X],L1).

%
%------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 3
%  Inserta un par de elementos en una lista de pares ordenados en una 
%  posición dada y desplazando el resto de elementos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 4.1
%  Comprueba que el elemento aparece un numero determinado de veces en una
%  lista dada
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 4.2
%  Comprueba que la lista contiene las ocurrencias de dos elementos en
%  forma de par
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 5
%  Comprueba que una lista contiene los elementos pares de la otra en
%  orden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 6
%  Transforma una lista de pares ordenados en un árbol de Huffman
%  simplificado
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 7.1
%  Codifica un elemento basándose en la estructura de árbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 7.2
%  Codifica una lista siguiendo la estructura de árbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 8
%  Codifica una lista basándose en el predicado Dictionary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%------------------------------
