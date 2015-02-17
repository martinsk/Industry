%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(industry_inspector).

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
    IndustryState = industry:get_state(),
    EJson = state_to_ejson(IndustryState),
    Body = jiffy:encode(EJson),
    {Body, Req, State}.



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
    Value.


    
