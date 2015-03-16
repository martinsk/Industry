%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(factory_inspector).

%% cowboy rest api
-export([init/3]).
-export([content_types_provided/2]).
-export([to_json/2]).




%%%===================================================================
%%% Callbacks
%%%===================================================================

%% upgrade to rest
init(_Transport, _Req, _Opts) -> 
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, to_json}
     ], Req, State}.

to_json(Req, State) ->
    {TypeBin, Req1} = cowboy_req:binding(type,Req),
    
    lager:warning("TYPEBIN : ~p", [TypeBin]),
    Type = list_to_existing_atom(binary_to_list(TypeBin)),
    
    lager:warning("TYPE : ~p", [Type]),
    FactoryState = factory:get_state(Type),
    EJson = state_to_ejson(FactoryState),
    Body = jiffy:encode(EJson),
    {Body, Req1, State}.



%%%===================================================================
%%% Implementations
%%%===================================================================

state_to_ejson(State) ->
    {lists:map(fun({K,V}) ->
		       {K, normalize_value(V, {list, string})}
	       end, State)}.

normalize_value(Values, {list, Type}) ->
    lists:map(fun(Value) ->
		      normalize_value(Value, Type)
	      end, Values);
normalize_value(Value, string) ->
    {[{name, list_to_binary(Value)}]}.


    
