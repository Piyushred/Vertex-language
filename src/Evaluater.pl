run(FileName) :- 
    open(FileName, read, InStream), 
    read(InStream, P), 
    close(InStream), program_eval(P).


% Evaluation expressions ----------------------------------------------

program_eval(start(T)) :-
    eval_block(T,_,_).
eval_block(block(T),Env,NewEnv) :- eval_command_list(T,Env,NewEnv),!.

% Evaluations of various commands.
eval_command_list(cmd(T1,T2),Env,NewEnv) :- eval_command(T1,Env,Env1),eval_command_list(T2,Env1,NewEnv).
eval_command_list(T,Env,NewEnv) :- eval_command(T,Env,NewEnv).

eval_command(declare(T1,T2),Env,NewEnv) :- eval_id(T2,Id), atom(Id),defaultValue(T1,Val),insert(Id,T1,Val,Env,NewEnv).
eval_command(declare(T1,T2,T3),Env,NewEnv) :- eval_id(T2,Id), atom(Id),eval_value(T3,Val),insert(Id,T1,Val,Env,NewEnv).
eval_command(assign(T1,T2),Env,NewEnv):- eval_expr(T2,Env,Env1,Res1),eval_id(T1,Id), update(Id,Res1,Env1,NewEnv).
eval_command(assign(T1,T2),Env,NewEnv):- eval_ternary(T2,Env,Env1,Res1),eval_id(T1,Id), update(Id,Res1,Env1,NewEnv).
eval_command(increment(T),Env,NewEnv):- eval_increment(T,Env,NewEnv,_).
eval_command(decrement(T),Env,NewEnv):- eval_decrement(T,Env,NewEnv,_).
eval_command(ifThen(T1,T2),Env,NewEnv) :- eval_comp_bool(T1,Env,Env1,true),eval_command(T2,Env1,NewEnv).
eval_command(ifThen(T1,_),Env,NewEnv) :- eval_comp_bool(T1,Env,NewEnv,false).
eval_command(ifElse(T1,T2,_),Env,NewEnv) :- eval_comp_bool(T1,Env,Env1,true),eval_command(T2,Env1,NewEnv).
eval_command(ifElse(T1,_,T3),Env,NewEnv) :- eval_comp_bool(T1,Env,Env1,false),eval_command(T3,Env1,NewEnv).
eval_command(while(T1,T2),Env,NewEnv) :- eval_comp_bool(T1,Env,Env1,true),eval_command(T2,Env1,Env2), eval_command(while(T1,T2),Env2,NewEnv).
eval_command(while(T1,_),Env,NewEnv) :- eval_comp_bool(T1,Env,NewEnv,false).
eval_command(block(T),Env,NewEnv):- eval_block(block(T),Env,NewEnv).
eval_command(for(T1,T2),Env,NewEnv) :- eval_loopscope_initialize(T1,Env,Env1,true),eval_block(T2,Env1,Env2),eval_command(forLoop(T1,T2),Env2,NewEnv).
eval_command(for(T1,_),Env,NewEnv) :- eval_loopscope_initialize(T1,Env,NewEnv,false).
eval_command(forLoop(T1,T2),Env,NewEnv) :- eval_loopscope(T1,Env,Env1,true),eval_block(T2,Env1,Env2),eval_command(forLoop(T1,T2),Env2,NewEnv).
eval_command(forLoop(T1,_),Env,NewEnv) :- eval_loopscope(T1,Env,NewEnv,false).
eval_command(forRange(T1,T2,T3,T4),Env,NewEnv):-eval_value(T2,InitialVal),eval_id(T1,Id),update(Id,InitialVal,Env,Env1),eval_bool(lessThan(T1,T3),Env1,Env2,true),
    eval_block(T4,Env2,Env3),
    lookup(Id,Env3,Val),
    NewVal is Val+1,    
	update(Id,NewVal,Env3,Env4),
    eval_command(forRangeLoop(T1,T3,T4),Env4,NewEnv).
eval_command(forRange(T1,T2,T3,_),Env,NewEnv):- eval_value(T2,Val),eval_id(T1,Id),update(Id,Val,Env,Env1),eval_bool(lessThan(T1,T3),Env1,NewEnv,false).
eval_command(forRangeLoop(T1,T3,T4),Env,NewEnv):-eval_id(T1,Id), eval_bool(lessThan(T1,T3),Env,Env1,true),
    eval_block(T4,Env1,Env2),
    lookup(Id,Env2,Val),    
    NewVal is Val+1,    
	update(Id,NewVal,Env2,Env3),
    eval_command(forRangeLoop(T1,T3,T4),Env3,NewEnv).
eval_command(forRangeLoop(T1,T3,_),Env,NewEnv):- eval_bool(lessThan(T1,T3),Env,NewEnv,false).
eval_command(display(T),Env,Env):- eval_expr(T,Env,Env,Res),write(Res).
eval_command(displayln(empty),Env,Env):-writeln("").
eval_command(displayln(T),Env,Env):- eval_expr(T,Env,Env,Res),writeln(Res).


eval_ternary(ternary(T1,T2,_),Env,NewEnv,Res):- eval_comp_bool(T1,Env,Env1,true),eval_expr(T2,Env1,NewEnv,Res).
eval_ternary(ternary(T1,_,T3),Env,NewEnv,Res):- eval_comp_bool(T1,Env,Env1,flase),eval_expr(T3,Env1,NewEnv,Res).

defaultValue('int',0).
defaultValue('bool',false).
defaultValue('string',"").

eval_increment(T,Env,NewEnv,NewVal):-eval_id(T,Id),lookup(Id,Env,Val), NewVal is Val+1, update(Id,NewVal,Env,NewEnv).
eval_decrement(T,Env,NewEnv,NewVal):-eval_id(T,Id),lookup(Id,Env,Val), NewVal is Val-1, update(Id,NewVal,Env,NewEnv).

eval_loopscope_initialize(loopScope(T1,T2,_),Env,NewEnv,true):- eval_command(T1,Env,Env1),eval_comp_bool(T2,Env1,NewEnv,true).
eval_loopscope_initialize(loopScope(T1,T2,_),Env,NewEnv,false):- eval_command(T1,Env,Env1),eval_comp_bool(T2,Env1,NewEnv,false).
eval_loopscope(loopScope(_,T2,T3),Env,NewEnv,true):- eval_command(T3,Env,Env1),eval_comp_bool(T2,Env1,NewEnv,true).
eval_loopscope(loopScope(_,T2,T3),Env,NewEnv,false):- eval_command(T3,Env,Env1),eval_comp_bool(T2,Env1,NewEnv,false).

% Boolean expressions evaluation.
eval_comp_bool(bool_and(T1,T2),Env,NewEnv,Val) :- eval_bool(T1,Env,Env1,Res1), eval_comp_bool(T2,Env1,NewEnv,Res2),bool_and(Res1,Res2,Val).
eval_comp_bool(bool_or(T1,T2),Env,NewEnv,Val) :- eval_bool(T1,Env,Env1,Res1), eval_comp_bool(T2,Env1,NewEnv,Res2),bool_or(Res1,Res2,Val).
eval_comp_bool(T,Env,NewEnv,Res):- eval_bool(T,Env,NewEnv,Res).

eval_bool(T,Env,NewEnv,Val):- eval_expr(T,Env,NewEnv,Val).
eval_bool(not_BoolExp(T),Env,NewEnv,Val) :- eval_bool(T,Env,NewEnv,Val1),eval_negate(Val1,Val).
eval_bool(isEqual(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_equality(Res1,Res2,Val).
eval_bool(isNotEqual(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_inEquality(Res1,Res2,Val).
eval_bool(lessThan(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_lessThan(Res1,Res2,Val).
eval_bool(greaterThan(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_greaterThan(Res1,Res2,Val).
eval_bool(lessThan(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_lessThan(Res1,Res2,Val).
eval_bool(greaterThan(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_greaterThan(Res1,Res2,Val).
eval_bool(lesserThanEq(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_lessThanEqual(Res1,Res2,Val).
eval_bool(greaterThanEq(T1,T2),Env,NewEnv,Val) :- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), eval_greaterThanEqual(Res1,Res2,Val).
eval_bool(true,Env,Env,true).
eval_bool(false,Env,Env,false).

bool_and(Val1,Val2,true):- Val1 = true,Val2 = true.
bool_and(Val1,Val2,false):- Val1 = false;Val2 = false.
bool_or(Val1,Val2,true):- Val1 = true; Val2 = true.
bool_or(Val1,Val2,false):- Val1 = false, Val2 = false.

eval_equality(Res1,Res2,true):- Res1=Res2.
eval_equality(Res1,Res2,false):- Res1\=Res2.
eval_inEquality(Res1,Res2,true):- Res1\=Res2.
eval_inEquality(Res1,Res2,false):- Res1=Res2.
eval_lessThan(Res1,Res2,true):- Res1<Res2.
eval_lessThan(Res1,Res2,false):- Res1>=Res2.
eval_greaterThan(Res1,Res2,true):- Res1>Res2.
eval_greaterThan(Res1,Res2,false):- Res1=<Res2.
eval_lessThanEqual(Res1,Res2,true):- Res1=<Res2.
eval_lessThanEqual(Res1,Res2,false):- Res1>Res2.
eval_greaterThanEqual(Res1,Res2,true):- Res1>=Res2.
eval_greaterThanEqual(Res1,Res2,false):- Res1<Res2.
eval_negate(true,false).
eval_negate(false,true).

% Expressions evaluation.
eval_expr(expr_assign(T1,T2),Env,NewEnv,Res):- eval_expr(T2,Env,Env1,Res),eval_id(T1,Id), update(Id,Res,Env1,NewEnv).
eval_expr(t_string_reverse(T),Env,Env,Res):- eval_expr(T,Env,Env,Str),string(Str),string_to_list(Str,L),reverse(L,Rev),string_to_list(Res,Rev).
eval_expr(t_string_concat(T1,T2),Env,Env,Res) :- eval_expr(T1,Env,Env,R1),eval_expr(T2,Env,Env,R2),string(R1),string(R2),string_concat(R1,R2,Res).
eval_expr(increment(T),Env,NewEnv,Res):- eval_increment(T,Env,NewEnv,Res).
eval_expr(decrement(T),Env,NewEnv,Res):- eval_decrement(T,Env,NewEnv,Res).
eval_expr(t_add(T1,T2), Env,NewEnv, Res):- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), Res is Res1 + Res2.
eval_expr(t_sub(T1,T2), Env,NewEnv, Res):- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), Res is Res1 - Res2.
eval_expr(t_mul(T1,T2), Env,NewEnv, Res):- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), Res is Res1 * Res2.
eval_expr(t_div(T1,T2), Env,NewEnv, Res):- eval_expr(T1,Env,Env1,Res1),eval_expr(T2,Env1,NewEnv,Res2), Res is Res1 / Res2.

eval_expr(T,Env,Env,Res):- eval_value(T,Res).
eval_expr(T,Env,Env,Res):- eval_id(T,Id),lookup(Id,Env,Res).

eval_value(t_integer(X),X):- number(X).
eval_value(t_string(X),X):- string(X). % NEED TO CHECK FURTHER
eval_value(t_bool(X),X).
eval_id(id(T),T).

% Lookup predicate to check the environment for variable values
lookup(Id,[],_):- write(Id),write(' not found'),fail.
lookup(Id,[(Id,_,Val)|_],Val).
lookup(Id1,[(Id2,_,_)|Env],Res):- Id1 \= Id2, lookup(Id1,Env,Res).

% update predicate to update a value in the environment
update(Id,_,[],_):- write(Id),write(' not declared'),fail.
update(Id,Val,[(Id,Type,_)|T],[(Id,Type,Val)|T]):- checkTypeValue(Type,Val,valid).
update(Id,Val,[(Id,Type,OldVal)|T],[(Id,Type,OldVal)|T]):- checkTypeValue(Type,Val,invalid),write('You are trying to update invalid value for '),writeln(Id),!,fail.
update(Id,Val,[H|T],[H|R]) :- H \= (Id,_), update(Id, Val, T, R).

checkTypeValue(int,Val,valid) :- integer(Val).
checkTypeValue(int,Val,invalid) :- \+ integer(Val).
checkTypeValue(bool,true,valid).
checkTypeValue(bool,false,valid).
checkTypeValue(bool,E,invalid) :- E\= true ; E\= false.
checkTypeValue(string,Val,valid) :- string(Val).
checkTypeValue(string,Val,invalid) :- \+ string(Val).

% Insert
insert(Id,Type,Val,[],[(Id,Type,Val)]) :- checkTypeValue(Type, Val , valid),!.
insert(_Id,Type,Val,E,E) :- checkTypeValue(Type, Val , invalid), writeln('Invalid value assignment'),!,fail.
insert(Id,Type,Val,[H|T],[H|R]) :- insert(Id, Type, Val, T, R).
