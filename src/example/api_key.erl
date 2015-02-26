%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(api_key).

-behaviour(factory_worker).

-export([validate/2]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 

validate(Key,Secret) ->
    factory_worker:call({Key, ?MODULE}, {validate, Secret}).


schema() ->
    [{name,   "api_key"},
     {type,   api_key},
     {attributes, [
		   {id        , i:integer()},
		   {secret    , i:string() },
		   {resource  , i:ref(resource)},
		   {owner     , i:ref(account)}
		  ]},
     {options, [{on_create, {db_worker, insert}},
		{on_change, {db_worker, update}},
		{on_delete, {db_worker, delete}}]},
     {timeout, timer:seconds(10)},
     {module, ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    Secret   = {secret , "TopSecret"},
    {ok, [Secret | Props]}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(WorkerState) -> {ok, WorkerState}.

handle_call({validate, Secret}, _From, State) ->
    Valid = Secret == i:get(secret, State),
    {reply, Valid, State};
handle_call(_Request, _From, State) ->
    lager:warning("api_key:handle_call"),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("api_key:handle_cast ~p ~n", [Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:warning("api_keu:handle_info"),
    {noreply, State}.
