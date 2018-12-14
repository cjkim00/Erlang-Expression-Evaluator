%Code for operator precedence modified from McKeeman's Parsing in Erlang : https://www.cs.dartmouth.edu/~mckeeman/cs118/languages/erlang/exprParser.html
%Chris Kim
%11/26/18
%pr2.erl

-module(pr2).
-compile(export_all).

%
% A suite of functions for handling arithmetical expressions
%

% Expressions are represented like this
%
%     {num, N}
%     {var, A}
%     {add, E1, E2}
%     {mul, E1, E2}
%
% where N is a number, A is an atom,
% and E1, E2 are themselves expressions,

	
	
	
%method used to start concurrency usage
%this function shows concurrency by evaluating and executing the same line from the file
%at the same time.
mymain() ->
	{ok, Input} = file:open("expressions.txt", read),
	{ok, Output1} = file:open("eval_output.txt", write),
	{ok, Output2} = file:open("execute_output.txt", write),
	
	mymainParser(Input, Output1, Output2),
		
	file:close(Input),
	file:close(Output1),
	file:close(Output2).
	
%function reads each line of the input then uses concurrency to evaluate and execute the expression
mymainParser(Input, Output1, Output2) ->
	Env = [{a, 5}, {b, 7}],
	Line = io:get_line(Input, ""),
	if
		Line =/= eof ->
			%remove new line character and spaces
			New = re:replace(Line, "\\r|\\n|\\s+", "", [global,{return,list}]),
			%parse the expression on the line
			Ex = expr:start(New),
			%spawn a process to evaluate the expression
			spawn(fun() -> eval_main(Output1, Ex, Env) end),
			%spawn a process to evaluate the expression
			spawn(fun() -> execute_main(Output2, Ex, Env) end),
			mymainParser(Input, Output1, Output2);
		true -> pass
	end.
%evaluates the given expression then writes it to the output file	
eval_main(Output, Ex, Env) ->
	Eval = integer_to_list(expr:eval(Env, Ex)),
	%is supposed to add a newline to the end of the result, but instead adds ""
	Formatted = io_lib:format("~p~n", [Eval]),
	%write the evaluated expression to the file
	file:write(Output, Formatted).
%executes the given expression then writes it to the output file	
execute_main(Output, Ex, Env) ->
	Eval = integer_to_list(expr:execute(Env, Ex)),
	%is supposed to add a newline to the end of the result, but instead adds ""
	Formatted = io_lib:format("~p~n", [Eval]),
	%write the evaluated expression to the file
	file:write(Output, Formatted).
	
	
	



%spawns two processes that evaluate and execute the equations in the txt file
go() ->
	
	spawn(fun() -> mymain1()end),
	spawn(fun() -> mymain2() end).
	
%mymain1 opens the expressions file, and calls the parser for mymain1 to evaluate the expression
mymain1() ->
  {ok, Input} = file:open("expressions.txt", read),
  {ok, Output} = file:open("mymain1_output.txt", write),
  mymain1Parser(Input, Output),
  file:close(Input),
  file:close(Output).
  
%mymain1Parser takes the input file, reads it line by line, removes spaces and the newline character
%then parses the expression on each line and sends it to be evaluated.
%the function then writes the solution to the output file
mymain1Parser(Input, Output) ->
  Line = io:get_line(Input, ""),
  if
    Line =/= eof ->
	%remove new line character and spaces
	  New = re:replace(Line, "\\r|\\n|\\s+", "", [global,{return,list}]),
	  %parses the new string
	  Ex = start(New),
	  %io:format("Main1: ~p~n", [Ex]),
	  Env = [{a, 5}, {b, 7}], 
	  %evaluates the expression
	  Eval = integer_to_list(eval(Env, Ex)),
	  %is supposed to add a newline to the end of the result, but instead adds ""
	  Formatted = io_lib:format("~p~n", [Eval]),
	  %write the evaluated expression to the file
	  file:write(Output, Formatted),
      mymain1Parser(Input, Output);
    true -> pass
  end.
%mymain2 opens the expressions file, and calls the parser for mymain2 to execute the expression
mymain2() ->%insert this into expr.erl
  {ok, Input} = file:open("expressions.txt", read),
  {ok, Output} = file:open("mymain2_output.txt", write),
  mymain2Parser(Input, Output),
  file:close(Input),
  file:close(Output).

%mymain2Parser takes the input file, reads it line by line, removes spaces and the newline character
%then parses the expression on each line and sends it to be executed.
%the function then writes the solution to the output file
mymain2Parser(Input, Output) ->
  Line = io:get_line(Input, ""),
  if
    Line =/= eof ->
	  %remove new line character and spaces
	  New = re:replace(Line, "\\r|\\n|\\s+", "", [global,{return,list}]),
	  %parses the new string
	  Ex = start(New),
	  
	  Env = [{a, 5}, {b, 7}], 
	  %executes the expression
	  Eval = integer_to_list(execute(Env, Ex)),
	  %is supposed to add a newline to the end of the result, but instead adds ""
	  Formatted = io_lib:format("~p~n", [Eval]),
	  %write the evaluated expression to the file
	  file:write(Output, Formatted),
      mymain2Parser(Input, Output);
    true -> pass
  end.

-type expr() :: {'num',integer()}
             |  {'var',atom()}
             |  {'add',expr(),expr()}
             |  {'mul',expr(),expr()}
			 |  {'int',expr(),expr()}
			 |  {'divi',expr(),expr()}.
			 
-spec print(expr()) -> string().

print({num,N}) ->
    integer_to_list(N);
print({var,A}) ->
    atom_to_list(A);
print({add,E1,E2}) ->
    "("++ print(E1) ++ "+" ++ print(E2) ++")";
print({mul,E1,E2}) ->
    "("++ print(E1) ++ "*" ++ print(E2) ++")";
print({int,E1,E2}) ->
    "("++ print(E1) ++ "#" ++ print(E2) ++")";
print({divi,E1,E2}) ->
    "("++ print(E1) ++ "%" ++ print(E2) ++")".
	
%------------------------------------------------------------------
%parse takes an expression and evaluates it, returning a parsed version of the expression
start("")  -> "";
start(Expression) -> 
  New = re:replace(Expression, "\\r|\\n|\\s+", "", [global,{return,list}]),
  Final = parse(New),
  Final.

parse([$(|Rest]) ->
	Operand = [],
	Operator = [{sentinel, 0}],
	{Final, _} = create_stacks(Rest, Operand, Operator),
	Final.
	

parse2([$(|Rest]) ->
	Operand = [],
	Operator = [{sentinel, 0}],
	{Final, _} = create_stacks(Rest, Operand, Operator).	
create_stacks([], Operand, Operator) -> 
	{[], []};
create_stacks([$(|Rest], Operand, Operator) ->
	{Final, Rest2} = parse2([$(|Rest]),
	create_stacks(Rest2, [Final|Operand], Operator);
%	Operand2 = [],
%	Operator2 = [{sentinel, 0}],
%	Final = create_stacks(Rest, Operand2, Operator2),
%	create_stacks(Rest, [Final] ++ Operand, Operator);
create_stacks([$)|Rest], Operand, Operator) ->
	{final_iteration(Operand, Operator), Rest};
%create_stacks([$(|Rest], Operand, Operator) ->
%	Final = create_stacks(Rest, Operand, Operator),
%	create_stacks(Rest, [Final|Operand], Operator);
create_stacks(Rest, Operand, Operator) ->
	{Temp, Temp2} = parser(Rest),
	if
		(Temp == {"+", 1}) or (Temp == {"*", 2}) or (Temp == {"#", 2}) or (Temp == {"%", 2}) ->	
			check_precedence(Temp, Temp2, Operand, Operator);
		true ->
			NewOperand = [Temp|Operand],
			create_stacks(Temp2, NewOperand, Operator)
	
	%by here evey operator in the stack should have the same level except the sentinel
	end.

final_iteration(Operand, Operator) when length(Operator) == 1 ->
	lists:nth(1, Operand);
final_iteration(Operand, Operator) ->
	{Op, _} = lists:nth(1, Operator),
	NewOps = remove_first(Operator),

	First = lists:nth(1, Operand),
	NewOpe = remove_first(Operand),
	
	Second = lists:nth(1, NewOpe),
	NewOpe2 = remove_first(NewOpe),
	
	{New, _} = parser(Op, Second, First),
	
	final_iteration([New|NewOpe2], NewOps).
	
	
check_precedence(Temp, Rest, Operand, Operator) ->
	{Op, Level} = Temp,
	%if the level is higher then push it onto the stack
	Top = lists:nth(1, Operator),
	{Op2, TopLevel} = Top,
	
	if
		Level > TopLevel ->
		NewOperator = [Temp|Operator],
		create_stacks(Rest, Operand, NewOperator);
	true ->
				
		%if the level of the current operator is equal to or less than the top oerator
		%create a tree from the top operator and two top operands
		%pop the operator off the stack and use it to create the tree
		%compare the next operator level in the stack and if it is equal to or less than
		%repeat else push the current operator onto the stack

		First = lists:nth(1, Operand),
		NewList = remove_first(Operand),
		Second = lists:nth(1, NewList),
		NewList2 = remove_first(NewList),
		{Operation, _} = parser(Op2, Second, First),
		FinalList = [Operation|NewList2],
		check_precedence(Temp, Rest, FinalList, remove_first(Operator))
	end.

	
%add function to iterate through the operators and if there are any remaining then do them

%if there is none left, except the sentinel, then return the operands list as a string
remove_first([H|T]) ->
	T.
append([H|T], Tail) ->
	[H|append(T, Tail)];
append([], Tail) ->
	Tail.
 
 %-----------------------------------------------------------------
 

parser([$+|Rest]) ->
	{{"+", 1}, Rest};
parser([$*|Rest]) ->
	{{"*", 2}, Rest};
parser([$#|Rest]) ->
	{{"#", 2}, Rest};
parser([$%|Rest]) ->
	{{"%", 2}, Rest};
parser([Ch|Rest]) when ($0 =< Ch andalso Ch =< $9) orelse Ch==$- ->
    {Succeeds,Remainder} = get_while(fun is_digit/1,Rest),
    {{num, list_to_integer([Ch|Succeeds])}, Remainder};


% recognise a variable: an atom built of small letters only.

parser([Ch|Rest])  when $a =< Ch andalso Ch =< $z ->
    {Succeeds,Remainder} = get_while(fun is_alpha/1,Rest),
    {{var, list_to_atom([Ch|Succeeds])}, Remainder}.

%The operators passed to this function are parsed accordingly, returning a tuple of the operation and the two numbers/varaibels
parser(Op, E1, E2) ->
  {case Op of %returned tuple
	  "+" -> {add,E1,E2};
	  "*" -> {mul,E1,E2};
	  "#" -> {int,E1,E2};
	  "%" -> {divi, E1,E2}
        end,
       ""}.
  
	   
-spec is_digit(integer()) -> boolean().

is_digit(Ch) ->
    $0 =< Ch andalso Ch =< $9.

% recognise a small letter

-spec is_alpha(integer()) -> boolean().

is_alpha(Ch) ->
    $a =< Ch andalso Ch =< $z.
	   
-spec get_while(fun((T) -> boolean()),[T]) -> {[T],[T]}.    
%-spec get_while(fun((T) -> boolean()),[T]) -> [T].    
			 
get_while(P,[Ch|Rest]) ->
    case P(Ch) of
	true ->
	    {Succeeds,Remainder} = get_while(P,Rest),
	    {[Ch|Succeeds],Remainder};
	false ->
	    {[],[Ch|Rest]}
    end;
get_while(_P,[]) ->
    {[],[]}.
%
% Evaluate an expression
%

-type env() :: [{atom(),integer()}].

-spec eval(env(),expr()) -> integer().

eval(_Env,{num,N}) ->
    N;
eval(Env,{var,A}) ->
    lookup(A,Env);
eval(Env,{add,E1,E2}) ->
    eval(Env,E1) + eval(Env,E2);
eval(Env,{mul,E1,E2}) ->
    eval(Env,E1) * eval(Env,E2);
eval(Env,{int,E1,E2}) ->
    eval(Env,E1) div eval(Env,E2);
eval(Env,{divi,E1,E2}) ->
    eval(Env,E1) rem eval(Env,E2).

%
% Compiler and virtual machine
%
% Instructions
%    {push, N} - push integer N onto the stack
%    {fetch, A} - lookup value of variable a and push the result onto the stack
%    {add2} - pop the top two elements of the stack, add, and push the result
%    {mul2} - pop the top two elements of the stack, multiply, and push the result

-type instr() :: {'push',integer()}
              |  {'fetch',atom()}
              |  {'add2'}
              |  {'mul2'}
			  |	 {'int2'}
			  |  {'divi2'}.

-type program() :: [instr()].

% compiler

-spec compile(expr()) -> program().

compile({num,N}) ->
    [{push, N}];
compile({var,A}) ->
    [{fetch, A}];
compile({add,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}];
compile({int,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{int2}];
compile({divi,E1,E2}) ->
    compile(E1) ++ compile(E2) ++ [{divi2}].

% run a code sequence in given environment and empty stack

-spec run(program(),env()) -> integer().
   
run(Code,Env) ->
    run(Code,Env,[]).

% execute an instruction, and when the code is exhausted,
% return the top of the stack as result.
% classic tail recursion

-type stack() :: [integer()].

-spec run(program(),env(),stack()) -> integer().

run([{push, N} | Continue], Env, Stack) ->
    run(Continue, Env, [N | Stack]);
run([{fetch, A} | Continue], Env, Stack) ->
    run(Continue, Env, [lookup(A,Env) | Stack]);
run([{add2} | Continue], Env, [N1,N2|Stack]) ->
    run(Continue, Env, [(N1+N2) | Stack]);
run([{mul2} | Continue], Env ,[N1,N2|Stack]) ->
    run(Continue, Env, [(N1*N2) | Stack]);
run([{int2} | Continue], Env ,[N1,N2|Stack]) ->
    run(Continue, Env, [(N2 div N1) | Stack]);
run([{divi2} | Continue], Env ,[N1,N2|Stack]) ->
    run(Continue, Env, [(N2 rem N1) | Stack]);
run([],_Env,[N]) ->
    N.

% compile and run ...
% should be identical to eval(Env,Expr)

-spec execute(env(),expr()) -> integer().
     
execute(Env,Expr) ->
	%io:fwrite("~w~n", [Expr]),
    run(compile(Expr),Env).


% Auxiliary function: lookup a
% key in a list of key-value pairs.
% Fails if the key not present.

-spec lookup(atom(),env()) -> integer().

lookup(A,[{A,V}|_]) ->
    V;
lookup(A,[_|Rest]) ->
    lookup(A,Rest).

% Test data.

-spec env1() -> env().    
env1() ->
    [{a,23},{b,-12}].

-spec expr1() -> expr().    
expr1() ->
    {add,{var,a},{mul,{num,2},{var,b}}}.

-spec test1() -> integer().    
test1() ->
    eval(env1(),expr1()).

-spec expr2() -> expr().    
expr2() ->
    {add,{mul,{num,1},{var,b}},{mul,{add,{mul,{num,2},{var,b}},{mul,{num,1},{var,b}}},{num,0}}}.

% simplification ...

zeroA({add,E,{num,0}}) ->
    E;
zeroA({add,{num,0},E}) ->
    E;
zeroA(E) ->
    E.

mulO({mul,E,{num,1}}) ->
    E;
mulO({mul,{num,1},E}) ->
    E;
mulO(E) ->
    E.

mulZ({mul,_,{num,0}}) ->
    {num,0};
mulZ({mul,{num,0},_}) ->
    {num,0};
mulZ(E) ->
    E.

compose([]) ->
    fun (E) -> E end;
compose([Rule|Rules]) ->
    fun (E) -> (compose(Rules))(Rule(E)) end.

rules() ->
    [ fun zeroA/1, fun mulO/1, fun mulZ/1].

simp(F,{add,E1,E2}) ->
    F({add,simp(F,E1),simp(F,E2)});
simp(F,{mul,E1,E2}) ->
    F({mul,simp(F,E1),simp(F,E2)});
simp(_F,E) -> E.

simplify(E) ->
    simp(compose(rules()),E).