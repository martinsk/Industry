%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(factory_worker).

-behaviour(gen_server).

%% API

-export([new/2,
	 call/2,
	 cast/2,
	 stop/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-record(state, {
	  module         = undefined :: atom(),
	  schema         = []        :: term(),
	  instance_state = []        :: proplists:proplist()
	 }).

%%%===================================================================
%%% Behaviour
%%%===================================================================
-type worker_state() :: proplists:proplist(). 

-export_type([worker_state/0]).

-callback new(term()) -> {ok, worker_state()} | {error, term()}.
-callback loaded(worker_state()) -> {ok, worker_state()} | {error, term()}.

-callback handle_call(term(), pid(), worker_state()) -> {reply,   term(), worker_state()}.
-callback handle_cast(term(),        worker_state()) -> {noreply,         worker_state()}.
-callback handle_info(term(),        worker_state()) -> {noreply,         worker_state()}.


%%%===================================================================
%%% API
%%%===================================================================

new(Schema, State) ->
    gen_server:start_link(factory_worker, [new, State, Schema], []).

stop({Id, Type}) ->
    cast_internal({Id, Type}, stop).


call({Id, Type}, Request) ->
    {ok, Pid} = factory:lookup({Id, Type}),
    gen_server:call(Pid, {req, Request}).


cast({Id, Type}, Request) ->
    {ok, Pid} = factory:lookup({Id, Type}),
    gen_server:cast(Pid, {req, Request}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
init([new, Props, Schema]) ->
    Module = proplists:get_value(module, Schema),
    {ok, NewInstanceState} = Module:new(Props),
    {ok, InstanceState} = Module:loaded(NewInstanceState),
    State = #state{module = Module, 
		   schema = Schema,
		   instance_state = InstanceState},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
handle_call({req, Request}, From, State) ->
    Module = State#state.module,
    {reply, Reply, NewInstanceState} = Module:handle_call(Request, From, State#state.instance_state),
    NewInstanceState =:= instance_state 
	orelse save_state_change(State#state.instance_state, NewInstanceState),
    {reply, Reply, State#state{instance_state = NewInstanceState}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast({req,Msg}, State) ->
    Module = State#state.module,
    {noreply, NewInstanceState} = Module:handle_cast(Msg, State#state.instance_state),
    NewInstanceState =:= instance_state 
	orelse save_state_change(State#state.instance_state, NewInstanceState),
    {noreply, State#state{instance_state = NewInstanceState}};
handle_cast({internal, stop}, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info(Info, State) ->
    Module = State#state.module,
    {noreply, NewInstanceState} = Module:handle_info(Info, State#state.instance_state),
    NewInstanceState =:= instance_state 
	orelse save_state_change(State#state.instance_state, NewInstanceState),
    {noreply, State#state{instance_state = NewInstanceState}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% call_internal({Id, Type}, Request) ->
%%     {ok, Pid} = factory:lookup({Id, Type}),
%%     gen_server:call(Pid, {internal, Request}).


cast_internal({Id, Type}, Request) ->
    {ok, Pid} = factory:lookup({Id, Type}),
    gen_server:cast(Pid, {internal, Request}).


save_state_change(_OldState, _NewState) ->
%%    io:format("should be saving state~n", []),
    ok.
