%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(resource).

-behaviour(factory_worker).

-export([get_api_keys/1,
	 send_message/2,
	 notify_message_status_change/3]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 



get_api_keys({Id, Type}) ->
    factory_worker:call({Id, Type}, get_api_keys).
send_message({Id, Type}, Msg) ->
    factory_worker:call({Id, Type}, {send_message, Msg}).


notify_message_status_change(Id, MsgId, Status) ->
    factory_worker:cast({Id, ?MODULE}, {message_status_change, MsgId, Status}).

schema() ->
    [{name, "resource"},
     {type, resource},
     {attributes,[
		  {id      , i:integer()},
		  {owner   , i:ref(account)},
		  {api_keys, i:set(i:ref(api_key))},
		  {messages, i:set(i:ref(message))}
		 ]},
     {options, [{on_create, {db_worker, insert}},
		{on_change, {db_worker, update}},
		{on_delete, {db_worker, delete}}]},
     {timeout, timer:seconds(10)},
     {module, ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    ApiKeyConf = [{owner,    i:get(owner, Props)},
		  {resource, i:get(id, Props)}],
    {ApiKey, api_key} = factory:create(api_key, ApiKeyConf),
    ApiKeys  = {api_keys, sets:from_list([ApiKey])},
    Messages = {messages, sets:new()},
    State = i:state([ApiKeys, Messages], Props),
    {ok, State}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(WorkerState) -> 
    {ok, WorkerState}.

handle_call({send_message, Msg}, _From, State) ->
    Messages = i:get(messages, State),
    NewMessages = sets:add_element(Msg, Messages),
    NewState = [{messages, NewMessages} | State],
    {reply, ok, NewState};
handle_call(get_api_keys, _From, State) ->
    Keys = i:get(api_keys, State),
    {reply, Keys, State};
handle_call(_Request, _From, State) ->
    lager:warning("resource:handle_call"),
    {reply, ok, State}.



handle_cast({message_status_change, MsgId, Status}, State) ->
    lager:warning("~p::Message ~p had status change to ~p", [?MODULE, MsgId, Status]),
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("resource:handle_cast ~p ~n", [Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    lager:warning("resource:handle_info"),
    {noreply, State}.
