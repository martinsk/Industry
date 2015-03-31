%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(db_worker).

-behaviour(factory_worker).

-export([insert/3,
	 select/2,
	 update/3,
	 delete/2,
	 create_table/1,
	 create_tables/1
	]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 

-define(MAX_WORKERS, 10).


insert(Schema, Id, Values) -> db_factory_worker_call({insert, Schema, Id, Values}).
select(Schema, Id)         -> db_factory_worker_call({select, Schema, Id}).
update(Schema, Id, Values) -> db_factory_worker_call({update, Schema, Id, Values}).
delete(Schema, Id)         -> db_factory_worker_call({delete, Schema, Id}).
create_table (Schema)      -> db_factory_worker_call({create_tables, [Schema]}).
create_tables(Schemas)     -> db_factory_worker_call({create_tables, Schemas}).

db_factory_worker_call(Request) ->
    WorkerId = random:uniform(?MAX_WORKERS),
    factory_worker:call({WorkerId, ?MODULE}, Request).

attribute_pairs() ->
    i_utils:list(i_utils:pair(i_utils:string(), i_utils:string())).

schema() ->
    [{name,    "db_worker"},
     {type,    ?MODULE},
     {attributes, [
		   {hostname  , i_utils:string()},
		   {port      , i_utils:integer()},
		   {namespace , i_utils:string()},
		   {pid       , i_utils:pid()}
		  ]},
     {options, []},
     {timeout, never},
     {module,  ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
%    lager:warning("Creating ~p", [Props]), 

    Defaults = [{hostname, "localhost"},
		{port,            9042},
		{namespace, "industry"}],
    State = i_utils:state(Props, Defaults),
    {ok, State}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(State) -> 
    Hostname  = i_utils:get(hostname,  State),
    Port      = i_utils:get(port,      State),
    {ok, Pid} = seestar_session:start_link(Hostname, Port),
    {ok, i_utils:state(State, [{pid, Pid}])}.


handle_call({insert, Schema, _Id, Values}, _From, State) ->
    Type      = i_utils:get(type,      Schema),
    NameSpace = i_utils:get(namespace, State),
    Pid       = i_utils:get(pid,       State),

    {Query, Row} = industry_seestar_helper:prepare_insert(NameSpace, Type,
							  Schema, Values),
    lager:notice("~p", [Query]),
    {ok, Res} = seestar_session:prepare(Pid, Query),
    QryID     = seestar_result:query_id(Res),
    Types     = seestar_result:types(Res),
    
    {ok, _} = seestar_session:execute(Pid, QryID, Types, Row, one),
    {reply, ok, State};
handle_call({select, Schema, Id}, _From, State) ->
    Type      = i_utils:get(type,      Schema),
    Attributes= i_utils:get(attributes,Schema),
    NameSpace = i_utils:get(namespace, State),
    Pid       = i_utils:get(pid,       State),

    AttributeNames = [ N || {N, _} <- Attributes],

    Query = industry_seestar_helper:prepare_select(NameSpace, Type,
						   Schema, Id),
    lager:notice("~p", [Query]),
    {ok, Rows} = seestar_session:perform(Pid, Query, one),
    Result = case seestar_result:rows(Rows) of
		 [Row] ->
		     industry_seestar_helper:format_row_results(lists:zip(AttributeNames, Row), Schema)
		     ;
		 [] ->
		     not_found
	     end,
    {reply, Result, State}; 
handle_call({update, Schema, Id, Values}, _From, State) ->
    Type      = i_utils:get(type,      Schema),
    NameSpace = i_utils:get(namespace, State),
    Pid       = i_utils:get(pid,       State),

    Query = industry_seestar_helper:prepare_update(NameSpace, Type,
						   Schema, Id, Values),
    lager:notice("~p", [Query]),
    {ok, _} = seestar_session:perform(Pid, Query, one),
    {reply, ok, State};
handle_call({create_tables, Schemas}, _From, State) ->
    NameSpace = i_utils:get(namespace, State),
    Pid       = i_utils:get(pid,       State),
    Queries = industry_seestar_helper:prepare_create_tables(NameSpace, Schemas),
    [lager:notice("~p", [Q]) || Q <- Queries], 
        lists:foreach(fun(Query) ->
			  seestar_session:perform(Pid, Query, one)
		  end, Queries),
    {reply,ok, State}.

handle_cast(Msg, State) ->
    io:format("db_worker:handle_cast ~p ~n", [Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    lager:warning("account:handle_info"),
    {noreply, State}.


