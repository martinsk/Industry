%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(i_utils).

-export([
	 uid/0,
	 string/0,
	 ejson/0,
	 integer/0,
	 boolean/0,
	 pos_integer/0,
	 password/0,
	 set/1,
	 pair/2,
	 list/1,
	 queue/1,
	 ref/1,
	 pid/0,
	 option/1,
	 enum/1,
	 node/2
	]).


-export([
	 get/2,
	 set/3,
	 state/2,
	 state_diff/3
	]).


-export([
	 render/2,
	 render_prepared/2,
	 render_type/2,
	 compile_schemas/1
    	]).

-export([
	 parse_tree_schema/1
	]).


uid()         -> uid.
pid()         -> pid.
string()      -> string.
integer()     -> integer.
boolean()     -> boolean.
ejson()       -> ejson.
pos_integer() -> integer.
password()    -> password.
set(Of)       -> {set, Of}.
pair(E1, E2)  -> {pair, E1, E2}.
list(Of)      -> {list, Of}.
queue(Of)     -> {queue, Of}.
ref(Of)       -> {ref, Of}.
option(Types) -> {option, Types}.
enum(Options) -> {enum, Options}.


node(Name, Children)      -> {node, Name, Children}.


get(Key, State) when is_atom(Key) ->
    proplists:get_value(Key, State);
get(Keys, State) when is_list(Keys) ->
    lists:foldl(fun i:get/2, State, Keys).
    
set(Key, Value, State) ->
    [{Key, Value} | State].


state(State, Defaults) ->
    State ++ Defaults.


state_diff(_Schema, State, State)-> [];
state_diff(Schema, OldState, NewState)->
    Attributes = [ K || {K, _} <- i_utils:get(attributes, Schema)],
    Old = [ {K, i_utils:get(K, OldState)} || K <- Attributes],
    New = [ {K, i_utils:get(K, NewState)} || K <- Attributes],
    {_,Diff} = lists:unzip(
		 lists:filter(
		   fun({X,X})         -> false;
		      ({{K,X},{K,Y}}) -> true
		   end, lists:zip(Old, New))),
    Diff.


render_prepared(undefined, _) -> null;
render_prepared(String, string) -> iolist_to_binary(String);
render_prepared(Boolean,   boolean) when Boolean == true;
					 Boolean == false -> Boolean;
render_prepared(Int,   integer) -> Int;
render_prepared(Set, {set, Of}) -> sets:from_list([render_prepared(E, Of)
						   || E <- sets:to_list(Set)]);
render_prepared(Ref, {ref, Of}) -> render_prepared(Ref, Of);
render_prepared(Enum, {enum, Options}) -> 
    true = lists:member(Enum, Options),
    render_prepared(atom_to_list(Enum), string).    


render(Boolean, boolean) -> 
    atom_to_list(Boolean);
render(String, string) -> 
    lager:warning("render(~p, string) = ~p", [String, ["'", binary_to_list(iolist_to_binary(String)),"'"]]),
    ["'", binary_to_list(iolist_to_binary(String)),"'"];
render(Int, integer) -> io_lib:format("~p", [Int]); 
render(Set, {set, Of}) -> ["{", 
			   string:join([render(E, Of) 
					|| E <- sets:to_list(Set)],
				       ",") 
			   ,"}"];
render(Enum, {enum, Options}) ->
    true = lists:member(Enum, Options),
    render(atom_to_list(Enum), string).


render_type(uid,      _Env) -> "uuid";
render_type(string,   _Env) -> "varchar"; 
render_type(ejson,     Env) -> render_type(string, Env);
render_type(integer,  _Env) -> "int";
render_type(boolean,  _Env) -> "boolean";
render_type({enum, _}, Env) -> render_type(string, Env);
render_type({ref, Of}, Env) -> render_type(i_utils:get([Of, attributes, id], Env), Env);
render_type({set, Of}, Env) -> ["set<", render_type(Of, Env),">"].
				    
    

compile_schemas(Schemas) ->
    Env = [{i_utils:get(type, Schema), Schema} || Schema <- Schemas],
    [compile_schema(Schema, Env) || Schema <- Schemas].

compile_schema(Schema, Env) ->
    [compile_schema_setting(Setting, Env) || Setting <- Schema].

compile_schema_setting({attributes, Attributes}, Env) ->
    {attributes, [compile_schema_attribute(Attr, Env) || Attr <- Attributes]}; 
compile_schema_setting(Else, _Env) -> Else.

compile_schema_attribute({Var, Type}, Env) -> 
    {Var, compile_schema_attribute_type(Type, Env)}.

compile_schema_attribute_type(string   , _Env) -> string; 
compile_schema_attribute_type(integer  , _Env) -> integer; 
compile_schema_attribute_type(boolean  , _Env) -> boolean; 
compile_schema_attribute_type(ejson    , _Env) -> ejson; 
compile_schema_attribute_type(pid      , _Env) -> pid;
compile_schema_attribute_type({enum, Of}, Env) -> {enum, Of}; 
compile_schema_attribute_type({ref, Of},  Env) -> i_utils:get([Of, attributes, id], Env); 
compile_schema_attribute_type({set, Of},  Env) -> {set, compile_schema_attribute_type(Of, Env)}. 
    

syntax_check_tree_schema(Schema, Env) ->
    Nodes = i_utils:get(nodes, Schema),
    lists:all(fun(Node) -> syntax_check_node_schema(Node, Env) end, Nodes). 

syntax_check_node_schema({node, Name, Args}, Env) when is_atom(Name) ->
    syntax_check_node_args(Args, Env).


syntax_check_node_args(Args, Env) ->
    lists:all(fun(Arg) ->
		      syntax_check_node_arg(Arg, Env)
	      end, Args). 

syntax_check_node_arg_list({ref, Of}, Env) -> lists:member(Of, Env); 
syntax_check_node_arg_list(string, _Env) -> true;
syntax_check_node_arg_list(int, _Env) -> true;
syntax_check_node_arg_list(_, _Env) -> false. 

syntax_check_node_arg(string, _Env) -> true;
syntax_check_node_arg({list, Of}, Env) ->  syntax_check_node_arg_list(Of, Env);
syntax_check_node_arg(Type, _Env) -> 
    lager:warning("unimplemented Type in args ~p ", [Type]),
    false.
    

parse_tree_schema(Grammar) ->
    Root = i_utils:get(root, Grammar),
    Env = [i_utils:get(name, Rule) || Rule <- i_utils:get(rules, Grammar)],
    true = lists:all(fun(Rule) ->
			     syntax_check_tree_schema(Rule, Env) 
		     end, i_utils:get(rules, Grammar) ),
    
    Term = {'or', [{'memberof', "Somethign"},
		   {'and', [{present, "shizzle"}]}
		  ]},
    true = in_language(Term, Grammar),
    Schema = translate_into_schema('filter', Grammar).

in_language(String, Grammar) ->
    Root = i_utils:get(root, Grammar),
    Rules = i_utils:get(rules, Grammar),
    [RootRule] = [Rule ||Rule <- Rules, Root =:= i_utils:get(name, Rule)],
    valid_term(String, RootRule, Rules).


valid_term(Tuple, Type, Rules) when is_tuple(Tuple) -> 
    valid_term(tuple_to_list(Tuple), Type, Rules);
valid_term(Str, string, Rules) -> 
    true;
valid_term(Term, {ref, Type}, Rules) ->
    [NewRule] = [Rule ||Rule <- Rules, Type =:= i_utils:get(name, Rule)],
    valid_term(Term, NewRule, Rules);    
valid_term(Terms, {list, Type}, Rules) -> 
    lists:all(fun(Term) -> 
		      valid_term(Term, Type, Rules)
	      end, Terms);
valid_term([Name | Args] , Rule, Rules) ->
    lists:any(fun({node, Name1, Types}) ->
		      Name1 =:= Name 
			  andalso lists:all(fun({Arg, Type}) ->
						    valid_term(Arg, Type, Rules)
					    end, lists:zip(Args,Types))
	      end, i_utils:get(nodes, Rule)).




translate_arg_types({ref, Of}, Grammar, Name) ->
    Env = [i_utils:get(name,Rule) || Rule <- i_utils:get(rules, Grammar)],
    case lists:member(Of, Env) of
	true ->
	    {ref, Name};
	false ->
	    {ref, Of}
    end;
translate_arg_types({list, Of}, Grammar, Name) ->
    {list, translate_arg_types(Of, Grammar, Name)};
translate_arg_types(Other, _Grammar, _Name) ->
    Other.



	

translate_rule_node_into_attributes({node, Transition, ArgTypes}, Rule, Grammar, Name) ->
    RuleName = i_utils:get(name, Rule),
    RuleNameStr = atom_to_list(RuleName),
    TrName = atom_to_list(Transition),
    lists:map(fun({ArgType, Int}) ->
		      IntStr = lists:flatten(io_lib:format("~p", [Int])),
		      AttributeName = list_to_atom("_" ++ RuleNameStr
						   ++ "_" ++ TrName ++ "_" ++ IntStr),
		      {AttributeName, translate_arg_types(ArgType, Grammar, Name)}
	      end, lists:zip(ArgTypes, lists:seq(0,length(ArgTypes) -1))).
	

translate_rule_into_attributes(Rule, Grammar, Name) ->
    lists:append(
      lists:map(fun(Node) ->
			translate_rule_node_into_attributes(Node, Rule, Grammar, Name)
		end, i_utils:get(nodes, Rule))).
    

    
translate_into_schema(Name, Grammar) ->
    TypeName = list_to_atom("grammar_" ++ atom_to_list(Name)),
    Expected = [{type, grammar_filter},
		{attributes, [
			      {id,                  i_utils:integer()},
			      {rules,               i_utils:enum([filter])},
			      {'_filter',           i_utils:enum(['or', 'and', 'present', 'memberof'])},
			      {'_filter_or_0',       i_utils:list(i_utils:ref(grammer_filter))},
			      {'_filter_and_0',      i_utils:list(i_utils:ref(grammer_filter))},
			      {'_filter_present_0',  i_utils:string()},
			      {'_filter_memberof_0', i_utils:string()}
			     ]}],
    
    RuleNames = {rules, i_utils:enum([i_utils:get(name,Rule) || Rule <- i_utils:get(rules, Grammar)])},
    RulesTransitions = lists:map (fun(Rule) ->
					  RuleName = i_utils:get(name, Rule),
					  TName = [ TransitionName 
						      || {node, TransitionName, _Args} <- i_utils:get(nodes, Rule)],
					  {list_to_atom("_" ++ atom_to_list(RuleName)), i_utils:enum(TName)}
				  end, i_utils:get(rules, Grammar)),
    
    ArgumentAttrbs = lists:append(
		       lists:map(fun(Rule) ->
					 translate_rule_into_attributes(Rule, Grammar, TypeName)
				 end, i_utils:get(rules, Grammar))
		      ),
    
    Ret = [{type, TypeName},
	   {attributes, [{id, i_utils:integer()}, RuleNames]
	    ++ RulesTransitions ++ ArgumentAttrbs}],
    io:format("~p~n", [Ret]),
    
    io:format("~p~n", [Expected]).
