%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%    Lab assignment 3: SWI PROLOG
%%    LAB GROUP: 2301
%%    Couple:  08
%%    Author 1: Andres Salas
%%    Author 2: Ricardo Riol
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 1
%  Comprueba si un elemento está en la lista o alguna de las sublistas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% PERTENECE
%  Entrada:
%    X: elemento a buscar
%    L: lista donde buscar el elemento
%  Salida:
%    Tantos trues como veces pertenezca el elemento a la lista o
%    cada uno de los valores que va tomando el elemento
pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

% Pruebas
% pertenece(1, [2, 1, 3, 1])
% pertenece(X, [2, 1, 3, 1])
% pertenece(1, L)


% PERTENECE_M
%  Entrada:
%    X: elemento a buscar
%    L: lista con sublistas.
%  Salida:
%    Tantos trues como veces pertenezca el elemento a la lista o sublista 
%    o cada uno de los valores que va tomando el elemento
pertenece_m(X,[X|_]) :- X\=[_|_].
pertenece_m(X,[A|_]) :- pertenece_m(X, A).
pertenece_m(X,[_|Rs]) :- pertenece_m(X, Rs).

% Pruebas
% pertenece_m(X, [2,[1,3],[1,[4,5]]])
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 2
%  Inserta un par de elementos en una lista de pares ordenados
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CONCATENA
%  Entrada:
%    L1: lista1
%    L2: lista2
%  Salida:
%    L3: Lista que es la concatenacion de L1 y L2
concatena([],L,L).
concatena([X|L1],L2,[X|L]):- concatena(L1,L2,L).

% Pruebas
% concatena([], [1, 2, 3], L)
% concatena([1, 2, 3], [4, 5], L)


% INVIERTE
%  Entrada:
%    L: Lista a invertir
%  Salida:
%    R: Lista invertida
invierte([], []).
invierte([X|L],L1) :- invierte(L,L2), concatena(L2,[X],L1).

% Pruebas
% invierte([1, 2], L)
% invierte([], L)
% invierte([1, 2, 3, 4, 5], L)
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 3
%  Inserta un par de elementos en una lista de pares ordenados en una 
%  posición dada y desplazando el resto de elementos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% INSERT
%  Entrada:
%  	 X-P: par de elementos a insertar
%    L: lista de pares ordenados
%  Salida:
%    R: lista con el par insertado en la posición p y desplazados los de L.
insert(X, [], X).
insert([X-P], [Y-Q|Rs], R):- P =< Q -> concatena([X-P], [Y-Q|Rs], R);
    concatena([Y-Q], L, R), insert([X-P], Rs, L).

% Pruebas
% insert([a-6],[], X)
% insert([a-6],[p-0], X)
% insert([a-6],[p-0, g-7] , X)
% insert([a-6],[p-0, g-7, t-2], X)
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 4.1
%  Comprueba que el elemento aparece un numero determinado de veces en 
%  una lista dada
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ELEM_COUNT
%  Entrada:
%    X: elemento a contar 
%    L: Lista en la que buscar el elemento 
%  Salida:
%    Xn: Numero de veces que se encuentra X en la lista L
elem_count(_,[], 0).
elem_count(X, [X|Rs], Xn):- elem_count(X, Rs, I), Xn is I+1.
elem_count(X, [Y|Rs], Xn):- Y\=X, elem_count(X, Rs, Xn).

% Pruebas
% elem_count(b, [b,a,b,a,b], Xn)
% elem_count(a, [b,a,b,a,b], Xn)
%------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 4.2
%  Comprueba que la lista contiene las ocurrencias de dos elementos en
%  forma de par
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% LIST_COUNT
%  Entrada:
%    L1: lista de elementos a contar  
%    L2: lista en la que se contaran los elementos
%  Salida:
%    L3: lista con los pares [elemento a contar-numero de veces que aparece]
list_count([], _, []).
list_count([X|RsX], Y, Xn):- elem_count(X, Y, Zn), concatena([X-Zn], L, Xn), 
    list_count(RsX, Y, L).

% Pruebas
% list_count([b], [b,a,b,a,b], Xn)
% list_count([b,a], [b,a,b,a,b], Xn)
% list_count([b,a,c], [b,a,b,a,b], Xn) 
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 5
%  Comprueba que una lista contiene los elementos pares de la otra en
%  orden
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SORT_LIST
%  Entrada:
%    L1: Lista de elementos a ordenar
%  Salida:
%    L2: lista de elementos ordenada
sort_list([], []).
sort_list([X-P|Rs], L2):- sort_list(Rs, LAux), insert([X-P], LAux, L2).

% Pruebas
% sort_list([p-0, a-6, g-7, t-2], X)
% sort_list([p-0, a-6, g-7, p-9, t-2], X)
% sort_list([p-0, a-6, g-7, p-9, t-2, 9-99], X) 
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 6
%  Transforma una lista de pares ordenados en un arbol de Huffman
%  simplificado
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BUILD-TREE
%  Entrada:
%    List: lista de pares de elementos ordenados 
%  Salida:
%    Tree: Arbol simplificado de Huffman creado a partir de la lista dada
build_tree([X-_], tree(X, nil, nil)).
build_tree([X-_|Rs], tree(1, T1, T2)):- T1 = tree(X,nil,nil), build_tree(Rs, T2).
          
% Pruebas
% build_tree([p-0, a-6, g-7, p-9, t-2, 9-99], X)
% build_tree([p-55, a-6, g-7, p-9, t-2, 9-99], X)
% build_tree([p-55, a-6, g-2, p-1], X)
% build_tree([a-11, b-6, c-2, d-1], X)
%------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 7.1
%  Codifica un elemento basandose en la estructura de arbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ENCODE_ELEM
%  Entrada:
%    X1: elemento a codificar
%    Tree: estructura de arbol segun la cual se codificara
%  Salida:
%    X2: version codificada del elemento X1 basado en el arbol Tree
encode_elem(X1,X2,tree(1, tree(X1, nil,nil), _)):- concatena([], [0], X2).
encode_elem(X1,X2,tree(1,_, tree(X1, nil, nil))):- concatena([], [1], X2).
encode_elem(X1,X2,tree(1, _ , RS)) :- encode_elem(X1, AUX, RS), concatena([1], AUX, X2).

% Pruebas
% encode_elem(a, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
% encode_elem(b, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
% encode_elem(c, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
% encode_elem(d, X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
%------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 7.2
%  Codifica una lista siguiendo la estructura de arbol
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ENCODE_LIST
%  Entrada:
%    L1: lista a codificar
%    Tree: estructura de arbol segun la cual se codificara
%  Salida:
%    L2: version codificada de la lista L1 basada en el arbol Tree
encode_list([], [], _).
encode_list([X1|RsX1], X2, Tree):- encode_elem(X1, X2Aux, Tree), concatena([X2Aux], L, X2), encode_list(RsX1, L, Tree).

% Pruebas
% encode_list([a], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
% encode_list([a,a], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
% encode_list([a,d,a], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))) 
% encode_list([a,d,a,q], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
% encode_list([a,b], X, tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil)))))
%------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               EJERCICIO 8
%  Codifica una lista usando el predicado Dictionary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado facilitado para la realización del ejercicio 8,
% crea el abecedario como lista
dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

% Pruebas
% dictionary(X)


% ENCODE
%  Entrada:
%    L1: lista a codificar
%  Salida:
%    L2: versión codificada de la lista L2 usando el predicado dictionary
encode(L1, L2):- dictionary(X),
    list_count(X, L1, LContada),
    sort_list(LContada, LOrdenada),
	invierte(LOrdenada, LInvertida), 
	build_tree(LInvertida, Tree), 
	encode_list(L1, L2, Tree).

% Pruebas
% encode([i,n,t,e,l,i,g,e,n,c,i,a,a,r,t,i,f,i,c,i,a,l], X)
% encode([i,a], X)
% encode([i,2,a], X)
%------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              FIN PRÁCTICA                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%