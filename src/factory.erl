%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(factory).

-behaviour(gen_server).

%% API
-export([start/1]).

-export([create/2, 
	 lookup/1,
	 unregister/2,
	 stop_workers/1,
	 get_state/1
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).


-record(state, {
	  mod_model = undefined  :: atom(),
	  schema    = []         :: term(),
	  workers   = dict:new() :: dict:dict(atom(), pid()),
	  table     = undefined  :: ets:tref()
	 }).

%%%===================================================================
%%% Behaviour
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start(Schema)->
    Type = proplists:get_value(type, Schema),
    Name = call_name(Type),
    gen_server:start_link({local, list_to_atom(Name)}, factory, [Schema], []).


create(Type, Props) ->
    case call(Type, {create, Props}) of
	{ok, Id}-> 
	    {Id, Type};
	Else -> Else
    end.

stop_workers(Type) ->
    call(Type, stop_workers).


lookup({Id, Type}) ->
    lookup({Id, Type}, 3).

unregister({Type, Id}, Pid) -> 
    ets_remove(Type, {Id,Pid}).


get_state(Type) -> 
    call(Type, get_state).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
%% start_link() ->
%%     gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([Schema]) ->
    lager:warning("init factory"),
    Type = i:get(type, Schema),
    TRef = ets_new(Type),
    {A, B, C} = now(),
    random:seed(A,B,C),
    State = #state{table = TRef, schema = Schema},
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
handle_call(stop_workers, _From, State) ->
    Type = i:get(type, State#state.schema),
    TableName = list_to_existing_atom(ets_tname(Type)),
    Workers = ets:tab2list(TableName),
    lists:foreach(fun({Id, _Pid}) ->
			  ok = factory_worker:stop({Id, Type}),
			  ets:delete(TableName, Id)
		  end, Workers),
    {reply, ok, State};
handle_call({load, Id}, _From, State) ->
    Type = i:get(type, State#state.schema),
    ok = case ets_lookup(Id, Type) of
	     [{Id, _Pid}] ->  
		 ok;
	     [] ->
		 {ok, Pid} =  factory_worker:load(State#state.schema, Id),
		 true = ets_insert(Type, {Id, Pid}),
		 ok
	 end,		
    {reply, ok, State};
handle_call({create, Props}, _From, State) ->
    Type = i:get(type, State#state.schema),
    InitProps = case i:get(id, Props) of
		    undefined ->
			Id = random:uniform(100000000),
			[{id, Id} | Props];
		    Id -> 
			Props
		end,
    Schema = State#state.schema,
    
    case ets_lookup(Id, Type) of
	[] ->
	    lager:warning("creating ~p", [Props] ), 
	    {ok, Pid} = factory_worker:new(Schema, InitProps),
	    
	    %% store in ets for lookup
	    true = ets_insert(Type, {Id, Pid}),
	    
	    {reply, {ok,Id}, State};
	[{Id, _Pid}] ->

	    lager:warning("Found ~p ~p",[Type,Id]), 
	    {reply, {error_existt}, State}
    end; 
handle_call(get_state, _From, State) ->
    Name = i:get(name, State#state.schema),
    Workers = [Id || {Id, _Pid} <- dict:to_list(State#state.workers)],
    Reply = [{name, {Name, string}}, [workers, {Workers, {list, string}}]],
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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


call_name(Type) ->
    lists:flatten(io_lib:format("industry_factory_~p", [Type])).


call(Type, Request) ->
    FactoryName = call_name(Type),
    FactoryNameA = list_to_existing_atom(FactoryName),
    gen_server:call(FactoryNameA, Request).

ets_tname(Type) ->
    lists:flatten(io_lib:format("industry_factory_~p_lookup", [Type])).

ets_new(Type) ->
    TName = ets_tname(Type),
    ets:new(list_to_atom(TName), [set, named_table, {read_concurrency, true}, {write_concurrency, false}, public]).

ets_insert(Type, {Id,Pid}) ->
    TName = ets_tname(Type),
    true = ets:insert(list_to_existing_atom(TName), {Id, Pid}).

ets_remove(Type, {Id,Pid}) ->
    TName = ets_tname(Type),
    true = ets:delete_object(list_to_existing_atom(TName), {Id, Pid}).
    
    
ets_lookup(Id, Type)->
    TName = ets_tname(Type),
    ets:lookup(list_to_existing_atom(TName), Id).


lookup({Id, Type}, 0) -> 
    EMessage = io_lib:format("failure to load {~p,~p} ", [Id, Type]),
    {error, EMessage};
lookup({Id, Type}, Retries) when Retries > 0 ->
    case ets_lookup(Id, Type) of
	[{Id, Pid}] -> {ok, Pid};
	[] -> 
	    ok = call(Type, {load, Id}),
	    lookup({Id, Type}, Retries -1)
    end. 
		  
