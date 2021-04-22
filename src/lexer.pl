%read the input stream
input(InStream,[]):-at_end_of_stream(InStream).
input(InStream,[TokenCode|RemTokens]):-input(InStream,RemTokens), get_code(InStream,TokenCode).

% Checks for terminators/delimitiers
wordSplit([Code1,Code2|Rem],[Code1|Words],Res):-char_type(Code2,alnum), wordSplit([Code2|Rem],Words,Res).
wordSplit([Code1|Rem],[Code1],Rem).

% Generates each token
% atom_string changes the right parameter into the string equivalent
% name generates the character code
% char type checks for alphanumerics and also any layout characters(spaces, tabs, new lines., etc)
token([],[]).
token([Code|Rem],Tokens):-char_type(Code,space),token(Rem,Tokens),!.
token([Code|Codes],[Strings|Tokens]):-char_type(Code,alnum), wordSplit([Code|Codes],Words,Rem), name(Word,Words), atom_string(Word,Strings), token(Rem,Tokens),!.
token([Code|Rem],[Strings|Tokens]):-name(Char,[Code]), atom_string(Char,Strings), token(Rem,Tokens).


