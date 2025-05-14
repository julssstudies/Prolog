:- module(_,_,[pure,assertions,regtypes]).


:- doc(title, "Primera práctica (Programación Lógica Pura)").

:- doc(author, "Julie Ann Oribio Basa, 220317").

:- doc(module, "

@begin{note}
@bf{Note:} Este fichero es la primera práctica de la asignatura 
@apl{Programación Declarativa: Lógica y Restricciones}.  Se van a utilizar
los conceptos del bloque temático de @tt{Programación Lógica Pura}.
@end{note}
").

:- doc(author_data/4, 
  "Define los datos del autor del fichero: primer apellido, segundo apellido, nombre y número de matrícula. Se define como:  @includedef{author_data/4}").
:- prop author_data(A,B,C,D).
author_data('Oribio', 'Basa', 'Julie Ann', '220317').

:- doc(bit/1, "Define un bit binario. Se define como:  @includedef{bit/1}").
:- prop bit(B) # "@var{B} es un dígito binario.".
bit(b(0)).
bit(b(1)).

:- doc(binary_byte/1, "Define un byte binario como una lista de 8 bits. Se define como:  @includedef{binary_byte/1}").
:- prop binary_byte(L) # "@var{L} es una lista de 8 bits.".
binary_byte([B7, B6, B5, B4, B3, B2, B1, B0]) :-
    bit(B7), 
    bit(B6), 
    bit(B5), 
    bit(B4),
    bit(B3), 
    bit(B2), 
    bit(B1), 
    bit(B0).

:- doc(hexd/1, "Define un dígito hexadecimal (0–f). Se define como:  @includedef{hexd/1}").
:- prop hexd(H) # "@var{H} es un dígito hexadecimal.".
hexd(h(0)). 
hexd(h(1)). 
hexd(h(2)). 
hexd(h(3)).
hexd(h(4)). 
hexd(h(5)). 
hexd(h(6)). 
hexd(h(7)).
hexd(h(8)). 
hexd(h(9)). 
hexd(h(a)). 
hexd(h(b)).
hexd(h(c)). 
hexd(h(d)). 
hexd(h(e)). 
hexd(h(f)).

:- doc(hex_byte/1, "Define un byte hexadecimal como una lista de dos dígitos hex. Se define como:  @includedef{hex_byte/1}").
:- prop hex_byte(HB) # "@var{HB} es un byte hexadecimal (dos dígitos hex).".
hex_byte([H1, H0]) :-
    hexd(H1), 
    hexd(H0).

:- doc(byte/1, "Define un byte como binario o hexadecimal. Se define como:  @includedef{byte/1}").
:- prop byte(B) # "@var{B} es un byte binario o hexadecimal.".
byte(BB) :- 
    binary_byte(BB).
byte(HB) :- 
    hex_byte(HB).

:- doc(byte_list/1, "Verifica que una lista es una lista de bytes (ya sean binarios o hex). Se define como:  @includedef{byte_list/1}").
:- pred byte_list(L) #"@var{L} es la lista que necesita ser verificada.".
byte_list([]).
byte_list([X|Y]) :- byte(X), byte_list(Y).

:- doc(concatenar/3, "Junta dos listas. Se define como:  @includedef{concatenar/3}").
:- pred concatenar(L1,L2,L) #"@var{L} es la lista concatenada de la lista @var{L1} y @var{L2}.".
concatenar([], L, L).
concatenar([X1|Y1], L, [X1|Y2]) :- concatenar(Y1, L, Y2).

:- doc(hex_to_bin/2, "Convierte un dígito hexadecimal a una lista de 4 bits binarios. Se define como: @includedef{hex_to_bin/2}").
:- pred hex_to_bin(H, Bin) #"@var{H} es el dígito hexadecimal que se convierte en una lista de 4 bits binarios @var{Bin}.".
hex_to_bin(0, [b(0),b(0),b(0),b(0)]).
hex_to_bin(1, [b(0),b(0),b(0),b(1)]).
hex_to_bin(2, [b(0),b(0),b(1),b(0)]).
hex_to_bin(3, [b(0),b(0),b(1),b(1)]).
hex_to_bin(4, [b(0),b(1),b(0),b(0)]).
hex_to_bin(5, [b(0),b(1),b(0),b(1)]).
hex_to_bin(6, [b(0),b(1),b(1),b(0)]).
hex_to_bin(7, [b(0),b(1),b(1),b(1)]).
hex_to_bin(8, [b(1),b(0),b(0),b(0)]).
hex_to_bin(9, [b(1),b(0),b(0),b(1)]).
hex_to_bin(a, [b(1),b(0),b(1),b(0)]).
hex_to_bin(b, [b(1),b(0),b(1),b(1)]).
hex_to_bin(c, [b(1),b(1),b(0),b(0)]).
hex_to_bin(d, [b(1),b(1),b(0),b(1)]).
hex_to_bin(e, [b(1),b(1),b(1),b(0)]).
hex_to_bin(f, [b(1),b(1),b(1),b(1)]).

:- doc(byte_convert/2, "Convierte un byte hexadecimal a un byte binario. Se define como: @includedef{byte_convert/2}").
:- pred byte_convert(HexByte, BinByte) #"@var{HexByte} es el byte hexadecimal que se convierte en un byte binario @var{BinByte}.".
byte_convert([h(H1), h(H0)], BB) :-
    hex_to_bin(H1, B1), hex_to_bin(H0, B0),
    concatenar(B1, B0, BB).

:- doc(byte_list_convert/2, "Convierte una lista de bytes hexadecimales a una lista de bytes binarios. Se define como: @includedef{byte_list_convert/2}").
:- pred byte_list_convert(HL, BL) #"@var{HL} es la lista de bytes hexadecimales que se convierte en una lista de bytes binarios @var{BL}.".
byte_list_convert([], []).
byte_list_convert([H|T], [B|BT]) :-
    byte_convert(H, B), byte_list_convert(T, BT).

:- doc(invertir/2, "Invierte una lista. Se define como:  @includedef{invertir/2}").
:- pred invertir(L,Inv) #"@var{Inv} es la lista invertida de la lista @var{L}.".
invertir([], []).
invertir([H|T], Inv) :- invertir(T, RT), concatenar(RT, [H], Inv).

:- doc(get_nth_bit_from_byte/3, "Extrae el @var{N}-ésimo bit desde un byte (ya sean binario o hex). Se define como:  @includedef{get_nth_bit_from_byte/3}").
:- pred get_nth_bit_from_byte(N, B, BN) :: (byte(Byte), bit(Bit)) 
#"@var{N} es la posición de un byte @var{B} del que sacaremos el bit @var{BN}".
get_nth_bit_from_byte(N, B, BN) :-
    hex_byte(B), 
    byte_convert(B, BB), 
    invertir(BB, RevBinByte),  
    get_nth_bit_from_binary_byte(N, RevBinByte, BN).
get_nth_bit_from_byte(N, B, BN) :-
    binary_byte(B),  
    invertir(B, RevBinByte),  
    get_nth_bit_from_binary_byte(N, RevBinByte, BN).

:- doc(get_nth_bit_from_binary_byte/3, "Extrae el @var{N}-ésimo bit desde un byte binario. Se define como:  @includedef{get_nth_bit_from_binary_byte/3}").
:- pred get_nth_bit_from_binary_byte(N, B, BN) :: (byte(Byte), bit(Bit)) 
#"@var{N} es la posición de un byte @var{B} del que sacaremos el bit @var{BN}".
get_nth_bit_from_binary_byte(0, [BN|_], BN).
get_nth_bit_from_binary_byte(s(N), [_|T], BN) :-
    get_nth_bit_from_binary_byte(N, T, BN).

:- doc(hex_byte_list/1, "Verifica que una lista es una lista de bytes hexadecimales. Se define como:  @includedef{hex_byte_list/1}").
:- prop hex_byte_list(L) #"@var{L} es la lista que necesita ser verificada.".
hex_byte_list([]).
hex_byte_list([H|T]) :-
    hex_byte(H),
    hex_byte_list(T).

:- doc(binary_byte_list/1, "Verifica que una lista es una lista de bytes binarios. Se define como:  @includedef{binary_byte_list/1}").
:- prop binary_byte_list(L) #"@var{L} es la lista que necesita ser verificada.".
binary_byte_list([]).
binary_byte_list([H|T]) :-
    binary_byte(H),
    binary_byte_list(T).

:- doc(byte_list_clsh/2, "Realiza un desplazamiento circular a la izquierda sobre una lista de bytes. Se define como:  @includedef{byte_list_clsh/2}").
:- pred byte_list_clsh(L, CLShL) #"@var{L} es la lista del que se realizará un desplazamiento circular a la izquierda y se convertirá en la lista @var{CLShL}.".
byte_list_clsh([], []).
% Si es hexadecimal
byte_list_clsh(L, CLShL) :-
    hex_byte_list(L),
    byte_list_convert(L, BinL),
    process_byte_llist(BinL, Rbin),
    byte_list_convert(CLShL, Rbin).
% Si es binario
byte_list_clsh(L, CLShL) :-
    binary_byte_list(L),
    process_byte_llist(L, CLShL).

:- doc(process_byte_llist/2, "Realiza un desplazamiento circular a la izquierda sobre una lista de bits. Se define como:  @includedef{process_byte_llist/2}").
:- pred process_byte_llist(BinL, R) #"@var{BinL} es la lista de bits del que se realizará un desplazamiento circular a la izquierda y se convertirá en la lista @var{R}.".
process_byte_llist(BinL, R) :-
    group_bytes(F, BinL),
    rotate_left(F, Frot),
    group_bytes(Frot, R).

:- doc(rotate_left/2, "Realiza un desplazamiento circular a la izquierda de una lista. Se define como: @includedef{rotate_left/2}").
:- pred rotate_left(L, R) #"@var{L} es la lista del que se realizará un desplazamiento circular a la izquierda y se convertirá en la lista @var{R}.".
rotate_left([X|Xs], R) :- concatenar(Xs, [X], R).

:- doc(group_bytes/2, "Agrupa una lista de bits en sublistas de 8 bits. Se define como: @includedef{group_bytes/2}").
:- pred group_bytes(L, G) #"@var{L} es la lista de bits que se agrupará en sublistas de 8 bits y se convertirá en la lista @var{R}.".
group_bytes([B7, B6, B5, B4, B3, B2, B1, B0],[[B7, B6, B5, B4, B3, B2, B1, B0]]).
group_bytes([B7,B6,B5,B4,B3,B2,B1,B0|Rest], [[B7,B6,B5,B4,B3,B2,B1,B0]|Bs]) :-
    group_bytes(Rest, Bs).

:- doc(byte_list_crsh/2, "Realiza un desplazamiento circular a la derecha sobre una lista de bytes. Se define como:  @includedef{byte_list_crsh/2}").
:- pred byte_list_crsh(L, CRShL) #"@var{L} es la lista del que se realizará un desplazamiento circular a la derecha y se convertirá en la lista @var{CRShL}.".
byte_list_crsh([], []).
% Si es hexadecimal
byte_list_crsh(L, CRShL) :-
    hex_byte_list(L),
    byte_list_convert(L, BinL),
    process_byte_rlist(BinL, Rbin),
    byte_list_convert(CRShL, Rbin).
% Si es binario
byte_list_crsh(L, CRShL) :-
    binary_byte_list(L),
    process_byte_rlist(L, CRShL).

:- doc(process_byte_rlist/2, "Realiza un desplazamiento circular a la derecha sobre una lista de bits. Se define como:  @includedef{process_byte_rlist/2}").
:- pred process_byte_rlist(BinL, R) #"@var{BinL} es la lista de bits del que se realizará un desplazamiento circular a la derecha y se convertirá en la lista @var{R}.".
process_byte_rlist(BinL, R) :-
    group_bytes(Lflat, BinL),
    invertir(Lflat,Lin),
    rotate_left(Lin, Lres),
    invertir(Lres,Lresin),
    group_bytes(Lresin, R).
    
:- doc(xor_bit/3, "Realiza una operación XOR entre dos bits. Se define como: @includedef{xor_bit/3}").
:- pred xor_bit(B1, B2, B3) :: (bit(B1), bit(B2), bit(B3)).
xor_bit(0, 0, 0).
xor_bit(0, 1, 1).
xor_bit(1, 0, 1).
xor_bit(1, 1, 0).

:- doc(byte_xor/3, "Realiza una operación XOR entre dos bytes, resultado en un tercer byte. Se define como: @includedef{byte_xor/3}").
:- pred byte_xor(B1, B2, B3) :: (byte(B1), byte(B2), byte(B3)).
byte_xor([], [], []).
byte_xor([b(X)|T1], [b(Y)|T2], [b(Z)|T3]) :-
    xor_bit(X, Y, Z), byte_xor(T1, T2, T3).
byte_xor([h(H1), h(H0)], [h(H1_2), h(H0_2)], [h(H1_3), h(H0_3)]) :-
    byte_convert([h(H1), h(H0)], B1),
    byte_convert([h(H1_2), h(H0_2)], B2),
    byte_xor(B1, B2, B3),
    byte_convert([h(H1_3), h(H0_3)], B3).


% TESTS 
:- test byte_list(L) : (L = []) + not_fails # "Lista vacía".
:- test byte_list(L) : 
    (L = [[b(0),b(0),b(0),b(1),b(0),b(0),b(1),b(0)], [h(f),h(f)]]) + not_fails
    # "Mezcla de bytes parte 1".
:- test byte_list(L) : 
    (L = [[h(0),h(0)], [h(1),h(1)], [b(1),b(1),b(1),b(0),b(1),b(1),b(0),b(1)]]) + not_fails
    # "Mezcla de bytes parte 2".
:- test byte_list(L) : 
    (L = [[b(0),b(0),b(0)], [h(1),h(1)]]) + fails
    # "Byte muy corto".

:- test byte_convert(HB, BB) : 
    (HB = [h(1),h(0)]) => 
    (BB = [b(0),b(0),b(0),b(1),b(0),b(0),b(0),b(0)]) + not_fails # "10 hexadecimal a binario".
:- test byte_convert(HB, BB) : 
    (HB = [h(a),h(a)]) => 
    (BB = [b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)]) + not_fails # "aa hexadecimal a binario".

:- test byte_list_convert(HL, BL) : 
    (HL = [[h(0),h(1)], [h(f),h(f)]]) => 
    (BL = [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(1)], 
           [b(1),b(1),b(1),b(1),b(1),b(1),b(1),b(1)]]) + not_fails # "Hexadecimal a binario".

:- test get_nth_bit_from_byte(N, B, BN) :
    (N = 8, B = [b(0),b(0),b(0),b(1),b(0),b(0),b(1),b(0)]) + fails
    # "0ut of bounds".
:- test get_nth_bit_from_byte(N, B, BN) :
    (N = 0, B = [b(0),b(0),b(0),b(1),b(0),b(0),b(1),b(1)]) => 
    (BN = b(1)) + not_fails # "Con un binario la posición 0".
:- test get_nth_bit_from_byte(N, B, BN) :
    (N = 0, B = [b(1),b(1),b(1)]) + fails
    # "Byte binario incompleto".

:- test concatenar(L1, L2, L) : 
    (L1 = [1,2], L2 = [3,4]) => 
    (L = [1,2,3,4]) + not_fails # "Concatenar de manera simple".
:- test concatenar(L1, L2, L) : 
    (L1 = [], L2 = [a,b]) => 
    (L = [a,b]) + not_fails # "Concatenar con lista vacía".

:- test invertir(L, Inv) : 
    (L = [1,2,3]) => 
    (Inv = [3,2,1]) + not_fails # "Invertir de manera simple".
:- test invertir(L, Inv) : 
    (L = []) => 
    (Inv = []) + not_fails # "Invertir con lista vacía".

:- test group_bytes(L, G) : 
    (L = [b(1),b(0),b(0),b(0),b(0),b(0),b(0),b(1)]) => 
    (G = [[b(1),b(0),b(0),b(0),b(0),b(0),b(0),b(1)]]) + not_fails # "Un solo byte".
:- test group_bytes(L, G) : 
    (L = [b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(1),b(1),b(1),b(1),b(1),b(1),b(1),b(1)]) => 
    (G = [[b(0),b(0),b(0),b(0),b(0),b(0),b(0),b(0)], [b(1),b(1),b(1),b(1),b(1),b(1),b(1),b(1)]]) + not_fails # "Dos bytes".

:- test byte_list_clsh(L, CLShL) : 
    (L = [[b(1),b(0),b(1),b(0),b(0),b(0),b(0),b(0)]]) => 
    (CLShL = [[b(0),b(1),b(0),b(0),b(0),b(0),b(0),b(1)]]) + not_fails # "Mover a la izq de un solo bit con byte binario".
:- test byte_list_clsh(L, CLShL) : 
    (L = [[h(8),h(0)]]) => 
    (CLShL = [[h(0),h(1)]]) + not_fails # "Mover a la izq de un solo bit con byte hexadecimal".

:- test byte_list_crsh(L, CRShL) : 
    (L = [[b(0),b(0),b(1),b(0),b(0),b(0),b(0),b(1)]]) => 
    (CRShL = [[b(1),b(0),b(0),b(1),b(0),b(0),b(0),b(0)]]) + not_fails # "Mover a la dch de un solo bit con byte binario".
:- test byte_list_crsh(L, CRShL) : 
    (L = [[h(0),h(1)]]) => 
    (CRShL = [[h(8),h(0)]]) + not_fails # "Mover a la dch de un solo bit con byte hexadecimal".

:- test byte_xor(B1, B2, B3) : 
    (B1 = [b(1),b(0),b(1),b(0),b(1),b(0),b(1),b(0)], 
     B2 = [b(1),b(1),b(1),b(1),b(1),b(1),b(1),b(1)]) => 
    (B3 = [b(0),b(1),b(0),b(1),b(0),b(1),b(0),b(1)]) + not_fails # "XOR del byte binario".
:- test byte_xor(B1, B2, B3) : 
    (B1 = [h(a),h(a)], 
     B2 = [h(f),h(f)]) => 
    (B3 = [h(5),h(5)]) + not_fails # "XOR del byte hexadecimal".
