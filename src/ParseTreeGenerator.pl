:- table expression/3,term/3.

vertex(FileName) :- open(FileName, read, InStream),
		  input(InStream,InputString),
		  tokenizer(InputString, Tokens),
		  preprocessor(Tokens,ProcessedTokens),
		  program(ParseTree,ProcessedTokens,[]),
		  close(InStream),
		  open('intermediateCode.intc', write, OutStream),
		  writeq(OutStream, ParseTree),
		  write(OutStream, '.'),
		  close(OutStream),
		  run('intermediateCode.intc').



input(InStream,[]):-at_end_of_stream(InStream).
input(InStream,[TokenCode|RemTokens]):- get_code(InStream,TokenCode),input(InStream,RemTokens).

tokenizer([],[]).
tokenizer([Code|Rem],Tokens):-char_type(Code,space),tokenizer(Rem,Tokens),!.
tokenizer([Code|Codes],[Strings|Tokens]):-char_type(Code,alnum), wordSplit([Code|Codes],Words,Rem), name(Word,Words), atom_string(Word,Strings), tokenizer(Rem,Tokens),!.
tokenizer([Code|Rem],[Strings|Tokens]):-name(Char,[Code]), atom_string(Char,Strings), tokenizer(Rem,Tokens).

wordSplit([Code1,Code2|Rem],[Code1|Words],Res):-char_type(Code2,alnum), wordSplit([Code2|Rem],Words,Res).
wordSplit([Code1|Rem],[Code1],Rem).

%Preprocessor

preprocessor([],[]).
preprocessor([H|T],[H,Str,H1|R1]) :- H = "\"" , processor(T,[H1|T1],R),atom_string(R,Str),preprocessor(T1,R1).
preprocessor([H|T],[H|R]) :- H \= "\"" , preprocessor(T,R).

processor([H|T],L,R) :- H\= "\"",atom_string(H,H1),processor(T,L,R1),string_concat(H1," ",R2),string_concat(R2,R1,R).
processor([H|T],[H|T],"") :- H = "\"".

% ParseTree

program(start(T)) --> ["start"], block(T),["end"].
block(block(T)) --> ["{"],command_list(T),["}"].

% All Commands are consumed using command_base (for right recursive call) and command predicates.
% command can be Assignment,If Else,while,block

command_list(cmd(T1,T2)) --> command(T1),command_list(T2).
command_list(T) --> command(T).
declaration(declare(X,Y,Z)) --> datatype(X), identifier(Y),["="],value(Z).
declaration(declare(X,Y)) --> datatype(X), identifier(Y).

datatype(int) --> ["int"].
datatype(bool) --> ["bool"].
datatype(string) --> ["string"].

value(t_string(S)) --> ["\""],[S], {string(S)},["\""].
value(t_integer(N)) --> [M],{atom_number(M,N)}.
value(t_bool(true)) --> ["true"].
value(t_bool(false)) --> ["false"].

command(T) --> declaration(T),[";"].
command(display(T)) --> ["display"],["("],expression(T),[")"],[";"].
command(displayln(empty)) --> ["displayln"],["("],[")"],[";"].
command(displayln(T)) --> ["displayln"],["("],expression(T),[")"],[";"].
command(T) --> command_assign(T),[";"].
command(ifThen(T1,T2)) --> ["if"],["("], composite_boolean_expr(T1),[")"], ["then"], block(T2).
command(ifElse(T1,T2,T3)) --> ["if"],["("], composite_boolean_expr(T1),[")"], ["then"], block(T2) ,["else"], block(T3).
command(while(T1,T2)) -->["while"],["("], composite_boolean_expr(T1),[")"],block(T2).
command(for(T1,T2)) --> ["for"],["("], loopscope(T1),[")"],block(T2).
command(forRange(T1,T2,T3,T4)) -->["for"],["("], identifier(T1), ["in"] ,["range"],["("],digit(T2), [","] ,digit(T3),[")"],[")"],block(T4).
command(T) --> block(T).

command_assign(assign(T1,T2)) --> identifier(T1),["="],expression(T2).
command_assign(assign(T1,T2)) --> identifier(T1),["="],command_ternary(T2).
command_assign(increment(T)) --> identifier(T),["+"],["+"].
command_assign(decrement(T)) --> identifier(T),["-"],["-"].
command_ternary(ternary(T1,T2,T3)) --> ["("], composite_boolean_expr(T1),[")"], ["?"], expression(T2) ,[":"], expression(T3).
loopscope(loopScope(T1,T2,T3)) --> command_assign(T1),[";"],composite_boolean_expr(T2),[";"], command_assign(T3).
% Boolean expressions
composite_boolean_expr(T) --> boolean_expr(T).
composite_boolean_expr(bool_and(T1,T2)) --> boolean_expr(T1),["AND"], composite_boolean_expr(T2).
composite_boolean_expr(bool_or(T1,T2)) --> boolean_expr(T1),["OR"], composite_boolean_expr(T2).
boolean_expr(T) --> expression(T).
boolean_expr(isEqual(T1,T2)) --> expression(T1),["="],["="],expression(T2).
boolean_expr(isNotEqual(T1,T2)) --> expression(T1),["~"],expression(T2).
boolean_expr(greaterThan(T1,T2)) --> expression(T1),[">"],expression(T2).
boolean_expr(greaterThanEq(T1,T2)) --> expression(T1),[">"],["="],expression(T2).
boolean_expr(lessThan(T1,T2)) --> expression(T1),["<"],expression(T2).
boolean_expr(lesserThanEq(T1,T2)) --> expression(T1),["<"],["="],expression(T2).
boolean_expr(not_BoolExp(T)) --> ["not"],boolean_expr(T).
boolean_expr(false) --> [false].
boolean_expr(true) --> [true].

expression(t_string_reverse(T)) --> ["reverse"],["("],stringword(T),[")"].
expression(t_string_concat(T1,T2)) --> ["concat"],["("],stringword(T1),[","],stringword(T2),[")"].
expression(expr_assign(T1,T2)) --> identifier(T1),["="],expression(T2).
expression(increment(T)) --> identifier(T),["+"],["+"].
expression(decrement(T)) --> identifier(T),["-"],["-"].
expression(t_add(T1,T2)) --> expression(T1),["+"],term(T2).
expression(t_sub(T1,T2)) --> expression(T1),["-"],term(T2).
expression(T) --> term(T).

term(t_mul(T1,T2)) --> term(T1),["*"],element(T2).
term(t_div(T1,T2)) --> term(T1),["/"],element(T2).
term(T) --> ["("],expression(T),[")"].
term(T) --> element(T).

element(T) --> value(T),!.
element(T) --> identifier(T).

stringword(T) --> value(T),{T=t_string(_)}.
stringword(T) --> identifier(T).

identifier(id(X)) --> [Y],{atom_string(X,Y),atom_chars(Y, L),check_identifier(L)}.
digit(t_integer(N)) --> [M],{atom_number(M,N),integer(N)}.

check_identifier([]).
check_identifier([H|T]):- atom_chars(H, L), char_type(L, alnum),check_identifier(T).
