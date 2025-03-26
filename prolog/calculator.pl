:- use_module(library(readline)).

% Tokenize - Input string'i tokenlara ayır
tokenize(Input, Tokens) :-
    atom_chars(Input, Chars),
    tokenize_chars(Chars, [], Tokens).

tokenize_chars([], Acc, Tokens) :-
    reverse(Acc, Tokens).
tokenize_chars([C|Cs], Acc, Tokens) :-
    (is_digit(C) ->
        collect_number([C|Cs], NumChars, Rest),
        atom_chars(NumAtom, NumChars),
        atom_number(NumAtom, Num),
        tokenize_chars(Rest, [Num|Acc], Tokens)
    ; is_operator(C) ->
        tokenize_chars(Cs, [C|Acc], Tokens)
    ; C = '(' ->
        tokenize_chars(Cs, ['('|Acc], Tokens)
    ; C = ')' ->
        tokenize_chars(Cs, [')'|Acc], Tokens)
    ; is_whitespace(C) ->
        tokenize_chars(Cs, Acc, Tokens)
    ; % unexpected character
        write('Unexpected character: '), write(C), nl,
        fail
    ).

collect_number([], [], []).
collect_number([C|Cs], [C|NumChars], Rest) :-
    is_digit(C), !,
    collect_number(Cs, NumChars, Rest).
collect_number([C|Cs], [], [C|Cs]).

is_digit(C) :- char_type(C, digit).
is_operator('+').
is_operator('-').
is_operator('*').
is_operator('/').
is_whitespace(C) :- char_type(C, space).

% İşlem önceliği için recursive descent parser
parse_expression(Tokens, Result, Rest) :-
    parse_add_sub(Tokens, Result, Rest).

parse_add_sub(Tokens, Result, Rest) :-
    parse_mul_div(Tokens, Left, TempRest),
    parse_add_sub_rest(Left, TempRest, Result, Rest).

parse_add_sub_rest(Left, [Op|Tokens], Result, Rest) :-
    (Op = '+' ; Op = '-'),
    !,
    parse_mul_div(Tokens, Right, TempRest),
    (Op = '+' -> NextLeft is Left + Right ; NextLeft is Left - Right),
    parse_add_sub_rest(NextLeft, TempRest, Result, Rest).
parse_add_sub_rest(Result, Rest, Result, Rest).

parse_mul_div(Tokens, Result, Rest) :-
    parse_primary(Tokens, Left, TempRest),
    parse_mul_div_rest(Left, TempRest, Result, Rest).

parse_mul_div_rest(Left, [Op|Tokens], Result, Rest) :-
    (Op = '*' ; Op = '/'),
    !,
    parse_primary(Tokens, Right, TempRest),
    (Op = '*' -> 
        NextLeft is Left * Right 
    ; 
        (Right =:= 0 -> 
            write('Hata: Sıfıra bölme hatası'), nl, fail 
        ; 
            NextLeft is Left / Right)
    ),
    parse_mul_div_rest(NextLeft, TempRest, Result, Rest).
parse_mul_div_rest(Result, Rest, Result, Rest).

parse_primary([Num|Rest], Num, Rest) :-
    number(Num),
    !.
parse_primary(['('|Tokens], Result, Rest) :-
    !,
    parse_expression(Tokens, Result, [')'|Rest2]),
    Rest = Rest2.
parse_primary(Tokens, _, _) :-
    write('Syntax error near: '), write(Tokens), nl,
    fail.

% Hesaplama - Parser kullanarak işlem önceliğini dikkate al
calculate(Tokens, Result) :-
    parse_expression(Tokens, Result, []).

% Ana hesaplama yardımcı yöntemi
eval_expression(Input, Result) :-
    tokenize(Input, Tokens),
    calculate(Tokens, Result).

% Ana program
main :-
    write('Prolog Hesap Makinesi'), nl,
    write('Çıkmak için "exit" yazın'), nl,
    repl.

repl :-
    write('> '),
    read_line_to_codes(user_input, Codes),
    atom_codes(Input, Codes),
    (Input = exit ->
        write('Hoşçakalın!'), nl
    ;
        catch(
            (eval_expression(Input, Result) -> 
                write('Sonuç: '), write(Result), nl
            ; 
                true % Hata zaten yazdırıldı
            ),
            Error,
            (write('Hata: '), write(Error), nl)
        ),
        repl
    ).

:- initialization(main). 