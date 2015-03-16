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
	 load/2,
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
	  module         = undefined   :: atom(),
	  schema         = []          :: term(),
	  instance_state = []          :: proplists:proplist(),
	  timeout        = never       :: pos_integer() | never,
	  run_state      = running     :: running | stopping
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


load(Schema, Id) ->
    gen_server:start_link(factory_worker, [load, Id, Schema], []).

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
    Module  = i:get(module,  Schema),
    Options = i:get(options, Schema),
    Type    = i:get(type,    Schema),
    Timeout = i:get(timeout, Schema),
    
    {ok, NewInstanceState} = Module:new(Props),
    {ok, InstanceState}    = Module:loaded(NewInstanceState),
    
    ok = case i:get(on_create, Options) of
	     undefined  -> ok;
	     Opts ->
		 Id = i:get(id, NewInstanceState),
		 lists:foreach(fun({Mod,Fun}) ->
				       Mod:Fun(Schema,Id, NewInstanceState)
			       end, Opts)
	 end,
    
    State = #state{module = Module, 
		   schema = Schema,
		   instance_state = cleanup_state(InstanceState, Schema),
		   timeout = Timeout},
    case Timeout of
	never ->
	    {ok, State};
	Timeout ->
	    {ok, State, Timeout}
    end;
init([load, Id, Schema]) ->
    Module  = i:get(module,  Schema),
    Options = i:get(options, Schema),
    Type    = i:get(type,    Schema),
    Timeout = i:get(timeout, Schema),
    

    Props = case i:get(on_load, Options) of
	[{Mod,Fun}] ->  Mod:Fun(Schema, Id)
    end,
	    
    {ok, InstanceState}    = Module:loaded(Props),
    State = #state{module = Module, 
		   schema = Schema,
		   instance_state = cleanup_state(InstanceState, Schema),
		   timeout = Timeout},
    case Timeout of
	never ->
	    {ok, State};
	Timeout ->
	    {ok, State, Timeout}
    end.


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
handle_call(Req, From, State = #state{run_state      = shutdown,
				      instance_state = InstanceState,
				      schema         = Schema,
				      timeout        = Timeout}) ->
    Type    = i:get(type,    Schema),
    Id      = i:get(id,      InstanceState), 
    Reply = call({Type, Id}, Req),
    finalize(reply, Reply, State, Timeout);
handle_call({req, Request}, From, #state{instance_state = InstanceState,
					 schema         = Schema,
					 module         = Module,
					 timeout        = Timeout} = State) ->
    {reply, Reply, NewInstanceState} = Module:handle_call(Request, From, InstanceState),
    ok = handle_state_change(Schema, InstanceState, NewInstanceState),
    finalize(reply, Reply, State#state{instance_state = cleanup_state(NewInstanceState, Schema)}, Timeout).

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
handle_cast(Req, State = #state{instance_state = InstanceState,
			     schema         = Schema,
			     run_state      = shutdown,
			     timeout        = Timeout}) ->
    Type    = i:get(type,    Schema),
    Id      = i:get(id,      InstanceState), 
    cast({Type, Id}, Req),
    finalize(noreply, State, Timeout);
handle_cast({req,Msg}, #state{instance_state = InstanceState, 
			      schema         = Schema, 
			      module         = Module,
			      timeout        = Timeout} = State) ->
    {noreply, NewInstanceState} = Module:handle_cast(Msg, InstanceState),
    ok = handle_state_change(Schema, InstanceState, NewInstanceState),
    finalize(noreply, State#state{instance_state = cleanup_state(NewInstanceState, Schema)}, Timeout);
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
handle_info(timeout, State = #state{run_state = running,
				 instance_state = InstanceState,
				 timeout = Timeout,
				 schema = Schema}) ->
    Id   = i:get(id, InstanceState),
    Type = i:get(type, Schema),
    factory:unregister({Type, Id}, self()),
    {noreply, State#state{run_state = shutdown}, Timeout};
handle_info(timeout, State = #state{run_state = shutdown}) ->
    {stop, normal, State};
handle_info(Info, #state{instance_state = InstanceState, 
			 schema         = Schema, 
			 module         = Module,
			 timeout        = Timeout} = State) ->
    {noreply, NewInstanceState} = Module:handle_info(Info, InstanceState),
    ok = handle_state_change(Schema, InstanceState, NewInstanceState),
    finalize(noreply, State#state{instance_state = cleanup_state(NewInstanceState, Schema)}, Timeout).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
terminate(normal, _State) ->
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


cleanup_state(State, Schema) ->
    Attributes = [Attr || {Attr, Type} <- i:get(attributes, Schema)],
    [{Attr, i:get(Attr, State)} || Attr <- Attributes].

handle_state_change(Schema, OldState, NewState) ->
    Diff = i:state_diff(Schema, OldState, NewState),
    case Diff of 
	[] -> ok;
	Diff ->
	    lager:warning("Diff ~p ", [Diff]), 
	    Options = i:get(options, Schema),
	    Type    = i:get(type,    Schema),
	    Id      = i:get(id,      OldState), 
	    case i:get(on_change, Options) of
		undefined -> ok;
		Opts ->
		    lists:foreach(fun({Mod,Fun}) ->
					  Mod:Fun(Schema, Id, Diff)
				  end, Opts)
	    end
    end.

cast_internal({Id, Type}, Request) ->
    {ok, Pid} = factory:lookup({Id, Type}),
    gen_server:cast(Pid, {internal, Request}).

finalize(noreply, State, never)         -> {noreply, State};
finalize(noreply, State, Timeout)      -> {noreply, State, Timeout}.
finalize(reply, Reply, State, never)    -> {reply, Reply, State};
finalize(reply, Reply, State, Timeout) -> {reply, Reply, State, Timeout}.

