%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(message).

-behaviour(factory_worker).

-export([create/3,
	 change_status/2]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 

create(Sender, Recipient, Body) ->
    lager:warning("~p ::  ~p is sending the message ~p to ~p ", [?MODULE, Sender, Body,Recipient]),
    Spec = [{sender,    Sender}, 
	    {recipient, Recipient},
	    {body,      Body}],
    Message = factory:create(?MODULE, Spec).

change_status(MsgId, Status) when Status =:= delivered;
				  Status =:= read ->
    factory_worker:call({MsgId, ?MODULE}, {change_status, Status}).

schema() ->
    [{name,   "message"},
     {type,   ?MODULE},
     {attributes, [
		   {id          , i:integer()},
		   {sender      , i:ref(account)},
		   {recipient   , i:ref(account)},
		   {body        , i:string()},
		   {status      , i:enum([sent, delivered, read])}
		  ]},
     {options, [{on_create, [{db_worker, insert}]},
		{on_load,   [{db_worker, select}]},
		{on_change, [{db_worker, update}]},
		{on_delete, [{db_worker, delete}]}]},
     {timeout, timer:seconds(10)},
     {module, ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    Sender    = i:get(sender, Props),
    Recipient = i:get(recipient, Props),
    ok = account:send_message({Sender, account},
			      i:get(id, Props),
			      {Recipient, account}),
    
    {ok, [{status, sent} | Props]}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(WorkerState) -> 
    {ok, WorkerState}.

handle_call({change_status, Status}, _From, State) ->
    Id         = i:get(id,        State),
    Sender     = i:get(sender,    State),
    Recipient  = i:get(recipient, State),
    account:notify_message_status_change(Sender,    Id, Status),
    account:notify_message_status_change(Recipient, Id, Status),
    NewState = [{status, Status} | State],
    {reply, ok, NewState};    
handle_call(_Request, _From, State) ->
    lager:warning("message:handle_call"),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("message:handle_cast ~p ~n", [Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:warning("message:handle_info"),
    {noreply, State}.



