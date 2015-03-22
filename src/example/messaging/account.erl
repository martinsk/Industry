%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(account).

-behaviour(factory_worker).

-export([send_message/3,
	 add_resource/1,
	 message_count/1,
	 get_api_keys/1,
	 print_state/1,
	 notify_message_status_change/3]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 

print_state({Id, Type}) ->
    factory_worker:call({Id, Type}, print_state).

add_resource({Id, Type}) ->
    factory_worker:call({Id, Type}, add_resource).

get_api_keys({Id, Type}) ->
    factory_worker:call({Id, Type}, get_api_keys).


send_message({SenderId, Type}, Msg, {RecipientId, Type}) ->
    lager:warning("~p ::  ~p is sending the message ~p to ~p ", [?MODULE, SenderId, Msg, RecipientId]),
    factory_worker:cast({SenderId, Type}, {send_message, Msg, {RecipientId, Type}}).

message_count({Id, Type}) ->
    factory_worker:call({Id, Type}, message_count).

notify_message_status_change(Id, MsgId, Status) ->
    factory_worker:cast({Id, ?MODULE}, {message_status_change, MsgId, Status}).
    

schema() ->
    [{name,   "account"},
     {type,   ?MODULE},
     {attributes, [
		   {id          , i:integer()},
		   {first_name  , i:string() },
		   {last_name   , i:string() },
		   {organization, i:ref(organization)},
		   {resources   , i:set(i:ref(resource))},
		   {roster      , i:set(i:ref(account))},
		   {messages    , i:set(i:ref(message))}
		  ]},
     {options, [{on_create, [{db_worker, insert}]},
		{on_load,   [{db_worker, select}]},
		{on_change, [{db_worker, update}]},
		{on_delete, [{db_worker, delete}]}]},
     {timeout, timer:seconds(1)},
     {module, ?MODULE}].


%%%===================================================================
%%% API
%%%===================================================================
-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    Resources = {resources, sets:new()}, 
    Roster =    {roster,    sets:new()},
    Messages =  {messages,  sets:new()},
    ok = organization:add_member({i:get(organization, Props), organization}, 
				 i:get(id, Props)),
    {ok, [Messages, Roster, Resources | Props]}.

-spec loaded(factory_worker:worker_state()) -> {ok, factory_worker:worker_state()}.
loaded(WorkerState) -> {ok, WorkerState}.


handle_call(print_state, _From, State) ->
    io:format("State ~p ~n", [State]),
    {reply, ok, State};
handle_call(get_api_keys, _From, State) ->
    Resources  = i:get(resources, State),
    KeySets = [resource:get_api_keys({Resource, resource}) 
	       || Resource <- sets:to_list(Resources)],
    Keys = sets:union(KeySets),
    {reply, Keys, State};
handle_call(add_resource, _From, State) ->
    Id         = i:get(id,        State),
    Resources  = i:get(resources, State),
    {Resource, resource} = factory:create(resource, [{owner, Id}]),
    NewState = i:state([{resources, sets:add_element(Resource, Resources)}],
		       State),
    {reply, Resource, NewState};
handle_call(message_count,_From, State) ->
    Count = length(sets:to_list(i:get(messages, State))),
    {reply, Count, State};
handle_call(_Request, _From, State) ->
    lager:warning("account:handle_call"),
    {reply, ok, State}.


handle_cast({send_message, Msg, {Recepient, Type}}, State) ->
    Roster      = i:get(roster,   State),
    Id          = i:get(id,       State),
    Messages    = i:get(messages, State),
    NewRoster   = sets:add_element(Recepient, Roster),
    NewMessages = sets:add_element(Msg,       Messages),
    factory_worker:cast({Recepient, Type}, {receive_message, {Id, ?MODULE}, Msg}),
    NewState = [{messages, sets:add_element(Msg,       Messages)},
		{roster,   sets:add_element(Recepient, Roster  )}| State],
    {noreply, NewState};
handle_cast({receive_message, {Sender, ?MODULE}, Msg}, State) ->
    Roster      = i:get(roster,   State),
    Messages    = i:get(messages, State),
    NewState    = [{messages, sets:add_element(Msg,    Messages)}, 
		   {roster,   sets:add_element(Sender, Roster  )}| State],
    {noreply, NewState};

handle_cast({message_status_change, MsgId, Status}, State) ->
    Resources  = i:get(resources, State),
    [resource:notify_message_status_change(Resource, MsgId, Status) 
     || Resource <- sets:to_list(Resources)],
    {noreply, State};
handle_cast(Msg, State) ->
    io:format("account:handle_cast ~p ~n", [Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    lager:warning("account:handle_info"),
    {noreply, State}.



