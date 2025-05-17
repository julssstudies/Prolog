:- module(_,_,[pure]).

author_data('Oribio', 'Basa', 'Julie Ann', '220317').

%% Ejercicio 1
% Definición estándar de número natural
natural(0).
natural(s(X)) :-
    natural(X).
% a: suma(X,Y,Z), cierto si y solo si Z es la suma de X e Y
% suma(X,Y,Z) :- Z  =  X + Y.
% caso base
suma(0,Y,Y) :- natural(Y).
% caso recursivo
suma(s(X),Y,s(Z)) :- suma(X,Y,Z).

% b: par(X), cierto si y sólo si X es par
% par(X) :- (X mod 2 = 0).
% caso base
par(0).
% caso recursivo
par(s(s(X))) :- 
    par(X).

% c: impar(X), cierto si y sólo si X es impar
% impar(X) :- (X mod 2 = 1).
% caso base
impar(s(0)).
% caso recursivo
impar(s(s(X))) :- 
    impar(X).


%% Ejercicio 2
% a: suma_a_lista(L,N,SL), cierto sí y sólo si SL es la lista que se
% obtiene al sumarle N a cada uno de los elementos de la lista L
% caso base
suma_a_lista([],_,[]).
% caso recursivo
suma_a_lista([X|Y],N,[R|Z]) :-
    suma(X,N,R),
    suma_a_lista(Y,N,Z).

% b: pares_lista(L,Ps), cierto sí y sólo si Ps es una lista que 
% contiene los números que son pares de la lista L
   %b.pares_lista(L,Ps)
% caso base
pares_lista([],[]).
% caso recursivo
pares_lista([X|Xs], [X|Ps]) :- 
    par(X), pares_lista(Xs, Ps).
pares_lista([X|Xs], Ps) :- 
    impar(X), pares_lista(Xs, Ps).


%% Ejercicio 3
% caso base: si I es cero, entonces E es el primer elemento de L y NL es el resto de L
extrae_elemento(0, [E|NL], E, NL).
% caso recursivo: si I es mayor que cero, se extrae el primer elemento de L y se llama recursivamente con I-1
extrae_elemento(s(I), [X|L], E, [X|NL]) :-
    extrae_elemento(I, L, E, NL).