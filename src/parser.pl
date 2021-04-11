:- use_rendering(svgtree).
:- table expression/3,term/3.

program(start(T)) --> [start], block(T),[end].

block(blocks(T1,T2)) --> ['{'],declaration_list(T1),command_list(T2),['}'].
block(block_declare(T)) --> ['{'],declaration_list(T),['}'].
block(block_commands(T)) --> ['{'],command_list(T),['}'].

% All Commands are consumed using command_base (for right recursive call) and command predicates.
% command can be Assignment,If Else,while,block

declaration_list(declare(T1,T2)) --> declaration(T1),[;],declaration_list(T2).
declaration_list(T) --> declaration(T),[;].

command_list(cmd(T1,T2)) --> command(T1),command_list(T2).
command_list(T) --> command(T).

declaration(var(T)) --> [var], identifier(T).

command(T) --> command_assign(T),[;].
command(ifThen(T1,T2)) --> [if],['('], composite_boolean_expr(T1),[')'], [then], block(T2), [endif].
command(ifElse(T1,T2,T3)) --> [if],['('], composite_boolean_expr(T1),[')'], [then], block(T2) ,[else], block(T3) ,[endif].
command(for(T1,T2)) --> [for],['('], loopscope(T1),[')'],block(T2).
command(foreach(T1,T2,T3,T4)) -->[for],['('], identifier(T1), [in] ,[range],['('],digit(T2), [','] ,digit(T3),[')'],[')'],block(T4).
command(block(T)) --> block(T).


command_assign(assign(T1,T2)) --> identifier(T1),[=],expression(T2).

loopscope(loopScope(T1,T2,T3)) --> command_assign(T1),[;],composite_boolean_expr(T2),[;], command_assign(T3).

% Boolean expressions

composite_boolean_expr(bool_expr(T)) --> boolean_expr(T).
composite_boolean_expr(bool_and(T1,T2)) --> boolean_expr(T1),[and], composite_boolean_expr(T2).
composite_boolean_expr(bool_or(T1,T2)) --> boolean_expr(T1),[or], composite_boolean_expr(T2).

boolean_expr(isEqual(T1,T2)) --> expression(T1),[=],expression(T2).
boolean_expr(not_BoolExp(T)) --> [not],boolean_expr(T).
boolean_expr(false) --> [false].
boolean_expr(true) --> [true].


expression(expr_assign(T1,T2)) --> identifier(T1),[=],expression(T2).
expression(t_add(T1,T2)) --> expression(T1),[+],term(T2).
expression(t_sub(T1,T2)) --> expression(T1),[-],term(T2).
expression(T) --> term(T).

term(t_mul(T1,T2)) --> term(T1),[*],element(T2).
term(t_div(T1,T2)) --> term(T1),[/],element(T2).
term(T) --> ['('],expression(T),[')'].
term(T) --> element(T).

element(T) --> identifier(T).
element(T) --> digit(T).

print(print(X)) --> [print], alphanum(X).

identifier(id(X)) --> [X],{atom(X)}.
digit(num(N)) --> [N],{number(N)}.
alphanum(alphaNum(X)) --> [X],{atom(X)}.