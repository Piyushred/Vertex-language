eval_value(t_integer(X),X):- number(X).
eval_value(t_string(X),X):- atom(X). % NEED TO CHECK FURTHER
eval_value(t_boolean(X),X).
eval_id(id(T),T).

% Lookup predicate to check the environment for variable values
lookup(Id,[],_):- write(Id),write(' not found'),fail.
lookup(Id,[(Id,_,Val)|_],Val).
lookup(Id1,[(Id2,_,_)|Env],Res):- Id1 \= Id2, lookup(Id1,Env,Res).
% update predicate to update a value in the environment
update(Id,_,[],_):- write(Id),write(' not declared'),fail.
update(Id,Val,[(Id,Type,_)|T],[(Id,Type,Val)|T]):- checkTypeValue(Type,Val,valid).
update(Id,Val,[(Id,Type,OldVal)|T],[(Id,Type,OldVal)|T]):- checkTypeValue(Type,Val,invalid),write('You are trying to insert invalid value for '),write(Id),fail.
update(Id,Val,[H|T],[H|R]) :- H \= (Id,_), update(Id, Val, T, R).
checkTypeValue(_,_,valid).%TODO TODO TODO TODO
checkTypeValue(int,Val,valid).%TODO TODO TODO TODO
% Insert
insert(Id,Type,Val,E,[(Id,Type,Val)|E]).
insert(Id,Type,Val,[H|T],[H|R]) :- insert(Id, Type, Val, T, R)