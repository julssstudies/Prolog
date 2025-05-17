:- module(_, _, [classic, assertions, regtypes]).

:- doc(title, "Segunda práctica ProDe (ISO-Prolog)").

:- doc(author, "Julie Ann Oribio Basa, 220317").

:- doc(module, "

@begin{note}
@bf{Note:} Este fichero es la segunda práctica de la asignatura 
@apl{Programación Declarativa: Lógica y Restricciones}.  Se van a utilizar
los conceptos del bloque temático de @tt{ISO-Prolog}.
@end{note}
").

:- doc(hide,'\6\call_in_module'/2). 

:- doc(author_data/4, 
  "Define los datos del autor del fichero: primer apellido, segundo apellido, nombre y número de matrícula. Se define como:  @includedef{author_data/4}").
:- prop author_data(A,B,C,D).
author_data('Oribio', 'Basa', 'Julie Ann', '220317').


% PRELIMINARES
:- doc(split/3, "Verifica que una secuencia es formada por dos subsecuencias no vacías concatenadas. Se define como:  @includedef{split/3}").
:- pred split(Todo, Parte1, Parte2) #"@var{Todo} es la lista concatenada formada por la lista @var{Parte1} y @var{Parte2} si @var{Parte1} y @var{Parte2} son dos subsecuencias no vacías.".
split(Todo, Parte1, Parte2) :-
    append(Parte1, Parte2, Todo),
    Parte1 \= [],
    Parte2 \= [].

:- doc(group/3, "Agrupa una lista con un número añadiendo paréntesis solo si la lista tiene 2 elementos o más . Se define como:  @includedef{group/3}").
:- pred group(Parte,Num,Grupo) #"@var{Grupo} es una lista de caracteres y un número, que es el resultado de componer la lista @var{Parte} con un número de repeticiones @var{Num}, añadiendo paréntesis solo si @var{Parte} tiene 2 elementos o más.".
% Caso 1: Parte and Num dados y Parte.length >= 2
group(Parte, Num, Grupo) :-
    integer(Num),
    nonvar(Parte), nonvar(Num),
    length(Parte, L), L >= 2,
    append(Parte, ['>'], Temp),
    append(['<'|Temp], [Num], Grupo).
% Caso 2: Parte and Num  dados y Parte.length < 2
group(Parte, Num, Grupo) :-
    integer(Num),
    nonvar(Parte), nonvar(Num),
    length(Parte, L), L < 2,
    append(Parte, [Num], Grupo).
% Caso 3: Grupo dado y tiene '<' y '>'
group(Parte, Num, Grupo) :-
    integer(Num),
    nonvar(Grupo),
    Grupo = ['<' | Rest],
    append(ParteWithBracket, [Num], Rest),
    append(Parte, ['>'], ParteWithBracket).
% Caso 4: Grupo dado y no tiene '<' y '>'
group(Parte, Num, Grupo) :-
    integer(Num),
    nonvar(Grupo),
    \+ Grupo = ['<' | _],
    append(Parte, [Num], Grupo).

:- doc(is_repeated/3, "Verifica el número de veces que se repite una secuencia en una secuencia dada. Se define como:  @includedef{is_repeated/3}").
:- pred is_repeated(Cs, Parte, Num) #"@var{Cs} es la secuencia @var{Parte} repetida @var{Num} veces.".
is_repeated([], _, 0).
is_repeated(Cs, Parte, Num) :-
    Parte \= [],
    append(Parte, Resto, Cs),
    is_repeated(Resto, Parte, N),
    Num is N + 1.

:- doc(simple_repetition/2, "Genera o comprueba una compresión por repetición. Se define como: @includedef{simple_repetition/2}").
:- pred simple_repetition(Inicial, Comprimida) #"@var{Comprimida} es la representación comprimida de la secuencia @var{Inicial} usando el formato de repeticiones.".
simple_repetition(Inicial, Comprimida) :-
    split(Inicial, Parte, _),
    is_repeated(Inicial, Parte, Num),
    Num > 1,
    group(Parte, Num, Comprimida).

% COMPRESION FASE A
:- doc(compress/2, "Predicado principal que limpia la memoria de compresiones previas y aplica la compresión óptima a una secuencia. Se define como: @includedef{compress/2}").
:- pred compress(Inicial, Comprimida) #"@var{Inicial} es la secuencia original y @var{Comprimida} es el resultado de aplicar compresión óptima, 
     después de limpiar cualquier compresión memorizada previa.".
compress(Inicial, Comprimida):-
    clean_memo,
    recursive_compression(Inicial, Comprimida).

:- doc(repetition/2, "Genera o comprueba una compresión por repetición con compresión recursiva. Se define como: @includedef{repetition/2}").
:- pred repetition(Inicial, Comprimida) #"@var{Comprimida} es la representación comprimida de la secuencia @var{Inicial} usando el formato de repeticiones con compresión recursiva.".
repetition(Inicial, Comprimida) :-
    split(Inicial, Parte, _),
    is_repeated(Inicial, Parte, Num),
    Num > 1,
    recursive_compression(Parte, ParteComprimida),
    group(ParteComprimida, Num, Comprimida).

% COMPRESION FASE B
:- doc(compression/2, "Comprime una secuencia usando repetición o división. Se define como: @includedef{compression/2}").
:- pred compression(Inicial, Comprimida) #"@var{Comprimida} es la representación comprimida de @var{Inicial} usando repetición o división.".
% Comprimir por repetición
compression(Inicial, Comprimida) :-
    repetition(Inicial, Comprimida).
% Comprimir por división
compression(Inicial, Comprimida) :-
    division(Inicial, Comprimida).

:- doc(division/2, "Divide una secuencia en dos partes y las comprime recursivamente. Se define como: @includedef{division/2}").
:- pred division(Inicial, Comprimida) #"@var{Comprimida} es la representación comprimida de @var{Inicial} mediante división en dos partes.".
division(Inicial, Comprimida) :-
    split(Inicial, Parte1, Parte2),
    recursive_compression(Parte1, Comprimida1),
    recursive_compression(Parte2, Comprimida2),
    append(Comprimida1, Comprimida2, Comprimida).

% COMPRESION FASE C
:- doc(recursive_compression/2, "Aplica compresión recursiva con memoización para obtener la forma comprimida óptima. Se define como: @includedef{recursive_compression/2}").
:- pred recursive_compression(Inicial, Comprimida) 
   #"@var{Comprimida} es el resultado de aplicar compresión recursiva con memoización a la secuencia @var{Inicial}.".
recursive_compression(Inicial, Comprimida) :-
    better_compression_memo(Inicial, Comprimida).

:- doc(better_compression/2, "Selecciona la compresión óptima (de menor longitud) entre todas las posibilidades. Se define como: @includedef{better_compression/2}").
:- pred better_compression(Inicial, Comprimida) #"@var{Comprimida} es la representación comprimida óptima de @var{Inicial}, seleccionando la de menor longitud.".
better_compression(Inicial, Comprimida) :-
    length(Inicial, LenInicial),
    findall(Comp, compression(Inicial, Comp), Comps),
    seleccionar_mas_corta(Comps, LenInicial, Inicial, Comprimida).

:- doc(seleccionar_mas_corta/4, "Selecciona, entre una lista de compresiones candidatas, la de menor longitud que además sea más corta que la secuencia original. 
Si ninguna mejora la longitud original, se devuelve la secuencia original sin comprimir. Se define como: @includedef{seleccionar_mas_corta/4}").
:- pred seleccionar_mas_corta(Comps, LenOriginal, MejorActual, Final) 
   #"@var{Comps} es la lista de posibles compresiones, @var{LenOriginal} es la longitud de la secuencia original,
     @var{MejorActual} representa la mejor compresión encontrada hasta el momento, y
     @var{Final} es la mejor compresión encontrada tras procesar toda la lista.".
seleccionar_mas_corta([], _, Mejor, Mejor).
seleccionar_mas_corta([Comp|Resto], LenOriginal, MejorActual, Final) :-
    length(Comp, LComp),
    length(MejorActual, LMejor),
    ( LComp < LenOriginal, LComp < LMejor ->
        seleccionar_mas_corta(Resto, LenOriginal, Comp, Final)
    ;
        seleccionar_mas_corta(Resto, LenOriginal, MejorActual, Final)
    ).


% COMPRESION FASE D
:- doc(memo/2, "Base de datos dinámica que memoriza resultados de compresiones ya calculadas para evitar repeticiones de trabajo.").
:- dynamic memo/2.

:- doc(clean_memo/0, "Limpia la base de datos de memoización eliminando todas las compresiones almacenadas. Se define como: @includedef{clean_memo/0}").
:- pred clean_memo 
   #"Elimina todos los hechos almacenados mediante el predicado @tt{memo/2}, dejando la base de datos vacía.".
clean_memo :- retractall(memo(_,_)).

:- doc(better_compression_memo/2, "Implementa la compresión óptima usando memoización para evitar recomputaciones. Se define como: @includedef{better_compression_memo/2}").
:- pred better_compression_memo(Inicial, Comprimida) #"@var{Comprimida} es la representación comprimida óptima de @var{Inicial} con memoización de resultados previos.".
better_compression_memo(Inicial, Comprimida) :-
    memo(Inicial, Result),
    !,
    Comprimida = Result.
better_compression_memo(Inicial, Comprimida) :-
    better_compression(Inicial, Result),
    assert(memo(Inicial, Result)),
    Comprimida = Result.

% DESCOMPRESIÓN
:- doc(decompress/2, "Descomprime una secuencia previamente comprimida. Se define como: @includedef{decompress/2}").
:- pred decompress(Comprimida, Descomprimida) #"@var{Descomprimida} es el resultado de descomprimir la secuencia @var{Comprimida}.".
decompress([], []) :- !.
decompress([X, N|Resto], Resultado) :-
    integer(N),
    !,
    repetir(X, N, XRepetido),
    decompress(Resto, RestoDescomprimido),
    append(XRepetido, RestoDescomprimido, Resultado).
decompress(['<'|Resto], Resultado) :-
    !,
    extraer_grupo_anidado(Resto, [], 1, Grupo, RestoSinGrupo),
    RestoSinGrupo = [N|RestoFinal],
    integer(N),
    !,
    decompress(Grupo, GrupoDescomprimido),
    repetir_secuencia(GrupoDescomprimido, N, GrupoRepetido),
    decompress(RestoFinal, RestoDescomprimido),
    append(GrupoRepetido, RestoDescomprimido, Resultado).
decompress([X|Resto], [X|RestoDescomprimido]) :-
    decompress(Resto, RestoDescomprimido).

:- doc(extraer_grupo_anidado/5, "Extrae un grupo de una secuencia comprimida teniendo en cuenta el anidamiento. Se define como: @includedef{extraer_grupo_anidado/5}").
:- pred extraer_grupo_anidado(Lista, Acum, Nivel, Grupo, Resto) 
   #"@var{Grupo} es el grupo extraído desde @var{Lista} considerando el nivel de anidamiento 
   @var{Nivel}, con acumulador @var{Acum}, y @var{Resto} es la parte restante tras el grupo.".
extraer_grupo_anidado([], Acum, _, Acum, []).
extraer_grupo_anidado(['<'|Resto], Acum, Nivel, Grupo, RestoFinal) :-
    !,
    NuevoNivel is Nivel + 1,
    append(Acum, ['<'], NuevoAcum),
    extraer_grupo_anidado(Resto, NuevoAcum, NuevoNivel, Grupo, RestoFinal).
extraer_grupo_anidado(['>'|Resto], Acum, Nivel, Grupo, RestoFinal) :-
    Nivel > 1,
    !,
    NuevoNivel is Nivel - 1,
    append(Acum, ['>'], NuevoAcum),
    extraer_grupo_anidado(Resto, NuevoAcum, NuevoNivel, Grupo, RestoFinal).
extraer_grupo_anidado(['>'|Resto], Acum, 1, Acum, Resto) :- !.
extraer_grupo_anidado([X|Resto], Acum, Nivel, Grupo, RestoFinal) :-
    append(Acum, [X], NuevoAcum),
    extraer_grupo_anidado(Resto, NuevoAcum, Nivel, Grupo, RestoFinal).

:- doc(repetir/3, "Genera una lista repitiendo un elemento un número dado de veces. Se define como: @includedef{repetir/3}").
:- pred repetir(Elemento, N, Resultado) 
   #"@var{Resultado} es la lista formada por @var{Elemento} repetido @var{N} veces.".
repetir(_, 0, []) :- !.
repetir(X, N, [X|Xs]) :-
    N > 0,
    N1 is N - 1,
    repetir(X, N1, Xs).

:- doc(repetir_secuencia/3, "Genera una lista repitiendo una secuencia un número dado de veces. Se define como: @includedef{repetir_secuencia/3}").
:- pred repetir_secuencia(Secuencia, N, Resultado) 
   #"@var{Resultado} es la lista formada por @var{Secuencia} repetida @var{N} veces.".
repetir_secuencia(_, 0, []) :- !.
repetir_secuencia(Secuencia, 1, Secuencia) :- !.
repetir_secuencia(Secuencia, N, Resultado) :-
    N > 1,
    N1 is N - 1,
    repetir_secuencia(Secuencia, N1, ResultadoParcial),
    append(Secuencia, ResultadoParcial, Resultado).

% TESTS 
:- test split(Todo, Parte1, Parte2) :
    (Todo = [a, b], Parte1 = [a], Parte2 = [b]) + not_fails
    # "División simple de dos elementos".
:- test split(Todo, Parte1, Parte2) :
    (Todo = [1, 2, 3, 4], Parte1 = [1, 2], Parte2 = [3, 4]) + not_fails
    # "División intermedia".
:- test split(Todo, Parte1, Parte2) :
    (Todo = [x, y, z], Parte1 = [x], Parte2 = [y, z]) + not_fails
    # "División con lista de tres elementos".
:- test split(Todo, Parte1, Parte2) :
    (Todo = [a, b], Parte1 = [], Parte2 = [a, b]) + fails
    # "Parte1 vacía, debe fallar".
:- test split(Todo, Parte1, Parte2) :
    (Todo = [a, b], Parte1 = [a, b], Parte2 = []) + fails
    # "Parte2 vacía, debe fallar".
:- test split(Todo, Parte1, Parte2) :
    (Todo = [], Parte1 = [], Parte2 = []) + fails
    # "Lista vacía total, debe fallar".

:- test group(Parte, Num, Grupo) :
    (Parte = [a, b], Num = 3) =>
    (Grupo = ['<', a, b, '>', 3]) + not_fails # "Agrupar lista de longitud >= 2 con número".
:- test group(Parte, Num, Grupo) :
    (Parte = [a], Num = 2) =>
    (Grupo = [a, 2]) + not_fails # "Agrupar lista de un solo elemento".
:- test group(Parte, Num, Grupo) :
    (Grupo = ['<', x, y, '>', 4], Num = 4) =>
    (Parte = [x, y]) + not_fails # "Desagrupar grupo con paréntesis".
:- test group(Parte, Num, Grupo) :
    (Grupo = [z, 1], Num = 1) =>
    (Parte = [z]) + not_fails # "Desagrupar grupo sin paréntesis".
:- test group(Parte, Num, Grupo) :
    (Grupo = ['<', x, '>', 5], Num = 5) =>
    (Parte = [x]) + not_fails # "Paréntesis con un solo elemento".

:- test is_repeated(Cs, Parte, Num) : 
    (Cs = [a,a,a,a], Parte = [a,a]) => (Num = 2) + not_fails # "Secuencia repetida dos veces".
:- test is_repeated(Cs, Parte, Num) : 
    (Cs = [x,x,x,x,x,x], Parte = [x,x]) => (Num = 3) + not_fails # "Tres repeticiones exactas".
:- test is_repeated(Cs, Parte, Num) : 
    (Cs = [1,2,1,2,1,2], Parte = [1,2]) => (Num = 3) + not_fails # "Repetición de secuencia compuesta".
:- test is_repeated(Cs, Parte, Num) : 
    (Cs = [], Parte = [a]) => (Num = 0) + not_fails # "Secuencia vacía, sin repeticiones".
:- test is_repeated(Cs, Parte, Num) : 
    (Cs = [x,y], Parte = []) + fails # "Parte vacía no es válida".

:- test simple_repetition(Inicial, Comprimida) : 
    (Inicial = [1,2,1,2,1,2]) => (Comprimida = ['<',1,2,'>',3]) + not_fails # "Tres repeticiones de una secuencia compuesta".
:- test simple_repetition(Inicial, Comprimida) : 
    (Inicial = [a,b,a,b,a,b]) => (Comprimida = ['<',a,b,'>',3]) + not_fails # "Tres repeticiones alternadas".
:- test simple_repetition(Inicial, Comprimida) : 
    (Inicial = [a,a,b,a,a,b]) => (Comprimida = ['<',a,a,b,'>',2]) + not_fails # "Dos repeticiones con secuencia de tres elementos".
:- test simple_repetition(Inicial, Comprimida) : 
    (Inicial = [z,z]) => (Comprimida = [z,2]) + not_fails # "Dos repeticiones de un solo elemento".
:- test simple_repetition(Inicial, Comprimida) : 
    (Inicial = [a,b,c]) + fails # "Sin repetición".

:- test repetition(Inicial, Comprimida) : 
    (Inicial = [a,a,a,a,a,a,a]) => (Comprimida = [a,7]) + not_fails # "Compresión directa simple".
:- test repetition(Inicial, Comprimida) : 
    (Inicial = [a,a,b,a,a,b]) => (Comprimida = ['<',a,a,b,'>',2]) + not_fails # "Compresión con subsecuencia de 3 elementos".
:- test repetition(Inicial, Comprimida) : 
    (Inicial = [x,y,x,y,x,y]) => (Comprimida = ['<',x,y,'>',3]) + not_fails # "Tres repeticiones alternadas".
:- test repetition(Inicial, Comprimida) : 
    (Inicial = [a,a,a,b,b,b]) + fails # "No puede comprimirse con una sola repetición".
:- test repetition(Inicial, Comprimida) : 
    (Inicial = [1,2,3]) + fails # "Secuencia sin repetición".
:- test repetition(Inicial, Comprimida) : 
    (Inicial = [z,z]) => (Comprimida = [z,2]) + not_fails # "Dos repeticiones de un solo elemento".

:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [a,a,a,a]) => (Comprimida = [a,4]) + not_fails # "Repetición directa".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [a,a,a,b,b,b]) => (Comprimida = [a,3,b,3]) + not_fails # "División en dos partes comprimibles".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [a,b,b,b,b,a,b,b,b,b]) => (Comprimida = ['<',a,b,4,'>',2]) + not_fails # "Compresión interna y repetición del grupo".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [x,y,z]) => (Comprimida = [x,y,z]) + not_fails # "Secuencia sin compresión, cae en cláusula por defecto".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [1,1,1,2,2,2]) => (Comprimida = [1,3,2,3]) + not_fails # "División con compresión parcial".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [a,a,b,b,a,a,b,b]) => (Comprimida = ['<',a,a,b,b,'>',2]) + not_fails # "Compresión con bloque de 4 repetido".

:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [a,a,a,a]) => (Comprimida = [a,4]) + not_fails # "Compresión óptima elegida por mejor longitud".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [a,b,a,b,a,b]) => (Comprimida = ['<',a,b,'>',3]) + not_fails # "Compresión óptima por repetición compuesta".
:- test recursive_compression(Inicial, Comprimida) : 
    (Inicial = [q,w,e]) => (Comprimida = [q,w,e]) + not_fails # "Sin compresión posible, se devuelve original".

:- test better_compression_memo(Inicial, Comprimida) : 
    (Inicial = [a,a,a,a]) => (Comprimida = [a,4]) + not_fails # "Memoización de compresión simple".
:- test better_compression_memo(Inicial, Comprimida) : 
    (Inicial = [a,b,a,b,a,b]) => (Comprimida = ['<',a,b,'>',3]) + not_fails # "Memoización de compresión por repetición de secuencia alterna".
:- test better_compression_memo(Inicial, Comprimida) : 
    (Inicial = [x,y,z]) => (Comprimida = [x,y,z]) + not_fails # "Secuencia no comprimible, se memoriza original".

:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = []) => (Descomprimida = []) + not_fails # "Caso de lista vacía". 
:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = [a, 3, b, 2]) => (Descomprimida = [a, a, a, b, b]) + not_fails # "Caso simple con repetición de elementos". 
:- test decompress(Comprimida, Descomprimida) : 
    (Comprimida = [a, a, a, b, b, b]) => (Descomprimida = [a, a, a, b, b, b]) + not_fails # "Caso con secuencias no comprimidas".  
:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = [a, 2, '<', a, 1, '>', 3]) => (Descomprimida = [a, a, a, a, a]) + not_fails # "Caso con un grupo anidado y repetición de secuencias".
:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = [a, 2, b, 2, '<', a, 1, '>', 2]) => (Descomprimida = [a, a, b, b, a, a]) + not_fails # "Caso con múltiples secuencias comprimidas y una anidada".
:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = [a, b, c]) => (Descomprimida = [a, b, c]) + not_fails # "Secuencia no comprimida, se mantiene igual". 
:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = [a, 4, b, 3, c, 2]) => (Descomprimida = [a, a, a, a, b, b, b, c, c]) + not_fails # "Secuencias repetidas no anidadas". 
:- test decompress(Comprimida, Descomprimida) :
    (Comprimida = [a, 2, '<', a, 2, b, 3, '>', 1]) => (Descomprimida = [a, a, a, a, b, b, b]) + not_fails # "Caso complejo con anidamientos y repeticiones".
