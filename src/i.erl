%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(i).

-export([
	 uid/0,
	 string/0,
	 integer/0,
	 pos_integer/0,
	 password/0,
	 set/1,
	 pair/2,
	 list/1,
	 queue/1,
	 ref/1,
	 pid/0,
	 option/1,
	 enum/1
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


uid()        -> uid.
pid()        -> pid.
string()     -> string.
integer()    -> integer.
pos_integer()-> integer.
password()   -> password.
set(Of)      -> {set, Of}.
pair(E1, E2) -> {pair, E1, E2}.
list(Of)     -> {list, Of}.
queue(Of)    -> {queue, Of}.
ref(Of)      -> {ref, Of}.
option(Types) -> {option, Types}.
enum(Options) -> {enum, Options}.


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
    Attributes = [ K || {K, _} <- i:get(attributes, Schema)],
    Old = [ {K, i:get(K, OldState)} || K <- Attributes],
    New = [ {K, i:get(K, NewState)} || K <- Attributes],
    {_,Diff} = lists:unzip(
		 lists:filter(fun({X,X})         -> false;
				 ({{K,X},{K,Y}}) -> true
			      end, lists:zip(Old, New))),
    %% lager:warning("Diff ~p", [Diff]),
    Diff.
    
    
    

render_prepared(String, string) -> list_to_binary(String);
render_prepared(Int,   integer) -> Int;
render_prepared(Set, {set, Of}) -> sets:from_list([render_prepared(E, Of)
						   || E <- sets:to_list(Set)]);
render_prepared(Ref, {ref, Of}) -> render_prepared(Ref, Of);
render_prepared(Enum, {enum, Options}) -> 
    true = lists:member(Enum, Options),
    render_prepared(atom_to_list(Enum), string).    


render(String, string) -> 
    lager:warning("render(~p, string) = ~p", [String, ["'", list_to_binary(String),"'"]]),
    ["'", list_to_binary(String),"'"];
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
render_type(integer,  _Env) -> "int";
render_type({enum, _}, Env) -> render_type(string, Env);
render_type({ref, Of}, Env) -> render_type(i:get([Of, attributes, id], Env), Env);
render_type({set, Of}, Env) -> ["set<", render_type(Of, Env),">"].
				    
    

compile_schemas(Schemas) ->
    Env = [{i:get(type, Schema), Schema} || Schema <- Schemas],
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
compile_schema_attribute_type(pid      , _Env) -> pid;
compile_schema_attribute_type({enum, Of}, Env) -> {enum, Of}; 
compile_schema_attribute_type({ref, Of},  Env) -> i:get([Of, attributes, id], Env); 
compile_schema_attribute_type({set, Of},  Env) -> {set, compile_schema_attribute_type(Of, Env)}. 
    

