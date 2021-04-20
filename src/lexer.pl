% Generates each token
% atom_string changes the right parameter into the string equivalent
% name generates the character code
% char type checks for alphanumerics and also any layout characters(spaces, tabs, new lines., etc)
token([],[]).
token([Code|Rem],Tokens):-char_type(Code,space),token(Rem,Tokens),!.
token([Code|Codes],[Strings|Tokens]):-char_type(Code,alnum), wordSplit([Code|Codes],Words,Rem), name(Word,Words), atom_string(Word,Strings), token(Rem,Tokens),!.
token([Code|Rem],[Strings|Tokens]):-name(Char,[Code]), atom_string(Char,Strings), token(Rem,Tokens).
