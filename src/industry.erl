%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(industry).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([get_state/0,
	 add_factory/1,
	 add_factories/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {factories = sets:new():: any}).

%%%===================================================================
%%% API
%%%===================================================================

get_state() ->
    gen_server:call(?SERVER, get_state).

add_factory(Schema) ->
    [Resp] = gen_server:call(?SERVER, {add_factories, [Schema]}),
    Resp.

add_factories(Schemas) ->
    gen_server:call(?SERVER, {add_factories, Schemas}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    %% RoutingTable = [
    %% 		    {"/api/",                            industry_inspector,        []},
    %% 		    {"/api/factories/:type",             factory_inspector,         []},
    %% 		    {"/api/factories/:type/worker/:id",  factory_worker_inspector,  []},
    %% 		    {"/[...]", cowboy_static, {priv_dir, industry, "www"}}
    %% 		   ],
    
    %% Dispatch = cowboy_router:compile([{'_', RoutingTable}]),
    
    %% HostSettings = [{port, 8080}],
    %% EnvSettings   = [{env, [{dispatch, Dispatch}]}],
    %% Connections  = 100,
    
    %% case cowboy:start_http(industry_http_listener, Connections, HostSettings, EnvSettings) of
    %% 	{error, badarg} -> lager:error("unable to initialize cowboy server ~p ~p", [EnvSettings]);
    %% 	_ -> ok
    %% end,
	
    {ok, #state{}}.

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
handle_call({add_factories, Schemas}, _From, State) ->
    CompiledSchemas = i:compile_schemas(Schemas),

    lager:warning("COMPILED SCHEMAS ~p", [CompiledSchemas]), 
    {Replies, NewFactories} = 
	lists:foldl(fun(Schema,{Repls,Facs}) ->
			    {Reply, NewFacs} = create_factory(Schema, Facs),
			    {[Reply | Repls], NewFacs}
		    end, {[], State#state.factories}, CompiledSchemas),
    {reply, Replies, State#state{factories = NewFactories}};
handle_call(get_state, _From, State) ->
    RetState = [{factories, sets:to_list(State#state.factories)}],
    {reply, RetState, State};
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
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


create_factory(Schema, Factories) ->
    Name = i:get(name, Schema),
    Type = i:get(name, Schema),
    case sets:is_element(Name, Factories) of
	false -> 
	    factory:start(Schema),
	    NewFactories = sets:add_element(Name,Factories),
	    {ok, NewFactories};
	true -> 
	    {{error_exits, Type}, Factories}
    end.
