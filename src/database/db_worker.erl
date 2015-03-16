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
    i:list(i:pair(i:string(), i:string())).

schema() ->
    [{name,    "db_worker"},
     {type,    ?MODULE},
     {attributes, [
		   {hostname  , i:string()},
		   {port      , i:integer()},
		   {namespace , i:string()},
		   {pid       , i:pid()}
		  ]},
     {options, []},
     {timeout, never},
     {module,  ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    lager:warning("Creating ~p", [Props]), 

    Defaults = [{hostname, "localhost"},
		{port,            9042},
		{namespace, "industry"}],
    State = i:state(Props, Defaults),
    {ok, State}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(State) -> 
    Hostname  = i:get(hostname,  State),
    Port      = i:get(port,      State),
    {ok, Pid} = seestar_session:start_link(Hostname, Port),
    {ok, i:state(State, [{pid, Pid}])}.


handle_call({insert, Schema, _Id, Values}, _From, State) ->
    Type      = i:get(type,      Schema),
    NameSpace = i:get(namespace, State),
    Pid       = i:get(pid,       State),

    Attributes = i:get(attributes, Schema),
    lager:warning("Attrs : ~p ", [Attributes]), 
    
    Query = [
	     io_lib:format("INSERT INTO ~s.~p", [NameSpace, Type]),
	     " (", string:join([ io_lib:format("~p", [K]) 
				 || {K,_} <- Attributes], ","), ") VALUES",
	     " (", string:join(["?" || _X <- Attributes], ","), ")"
	    ],

    lager:warning("Q : ~p ", [lists:flatten(Query)]), 
    {ok, Res} = seestar_session:prepare(Pid, lists:flatten(Query)),
    QryID = seestar_result:query_id(Res),
    Types = seestar_result:types(Res),
    
    Row = [ begin
		Value = proplists:get_value(Attribute, Values),
		i:render_prepared(Value, AttrType)
	    end || {Attribute, AttrType} <- Attributes],

    
    {ok, _} = seestar_session:execute(Pid, QryID, Types, Row, one),
    {reply, ok, State};
handle_call({select, Schema, Id}, _From, State) ->
    Type      = i:get(type,      Schema),
    NameSpace = i:get(namespace, State),
    Pid       = i:get(pid,       State),

    Attributes = i:get(attributes, Schema),

    Query = [ "SELECT ", string:join([ io_lib:format("~p", [K]) 
				       || {K,_} <- Attributes], ","),
	      " FROM ", io_lib:format("~s.~p", [NameSpace, Type]),
	      "  WHERE id=", i:render(Id, i:get([attributes, id], Schema))],
    lager:warning("Query : ~p ", [lists:flatten(Query)]),
    {ok, Rows} = seestar_session:perform(Pid, lists:flatten(Query), one),
    Result = case seestar_result:rows(Rows) of
		 [Row] ->
		     format_select_results(Row, Schema);
		 [] ->
		     not_found
	     end,
    lager:warning("RESULT ~p", [Result]),
    {reply, Result, State}; 
handle_call({update, Schema, Id, Values}, _From, State) ->
    Type      = i:get(type,      Schema),
    NameSpace = i:get(namespace, State),
    Pid       = i:get(pid,       State),

    QueryAssignments = [begin
			    KeyType = i:get([attributes, Key], Schema),
			    RenderedKey = i:render(Value, KeyType),
			    io_lib:format("~p = ~s", [Key, RenderedKey])
			end || {Key, Value} <- Values],
    
    IdType = i:get([attributes, id], Schema), 
    Query = [
	     io_lib:format("UPDATE ~s.~p", [NameSpace, Type]),
	     " SET ", string:join(QueryAssignments, ", "),
	     io_lib:format(" WHERE id= ~s", [i:render(Id, IdType)]) 
	    ],

    lager:warning("Query : ~p ", [lists:flatten(Query)]),
    {ok, _} = seestar_session:perform(Pid, lists:flatten(Query), one),
    {reply, ok, State};
handle_call({create_tables, Schemas}, _From, State) ->
    Env = [{Type, Schema} || Schema <- Schemas, begin
						    Type = i:get(type, Schema),
						    lager:warning("Type ~p", [Type]),
						    true
						end],
    %% lager:warning("Env : ~p ", [Env]),
    lists:foreach(fun(Schema) ->
			  create_table_impl(Schema, Env, State)
		  end, Schemas),
    {reply,ok, State}.

handle_cast(Msg, State) ->
    io:format("db_worker:handle_cast ~p ~n", [Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    lager:warning("account:handle_info"),
    {noreply, State}.



create_table_impl(Schema, Env, State) ->
    Type      = i:get(type,      Schema),
    NameSpace = i:get(namespace, State),
    Pid       = i:get(pid,       State),
    
    Attributes = i:get(attributes, Schema),
    Query = [
	     io_lib:format("CREATE TABLE ~s.~p", [NameSpace, Type]),
	     "(", string:join([begin
				   RType = i:render_type(Type, Env),
				   Primary = case Key of
						 id -> "PRIMARY KEY ";
						 _ -> ""
							  
					     end,
				   io_lib:format(" ~p ~s ~s", [Key, RType, Primary])
			       end || {Key, Type} <- Attributes],","),
	     ")"
	    ],
    lager:warning("Query : ~s", [lists:flatten(Query)]), 
    Resp = seestar_session:perform(Pid, lists:flatten(Query), one).


format_select_results(Row, Schema) -> 
    Tuples = lists:zip(Row, i:get(attributes, Schema)),
    [{Name, format_element(Value, Type)} || {Value, {Name, Type}} <- Tuples].

format_element(Value, integer) ->
    Value;
format_element(Value,  string) ->
    binary_to_list(Value);
format_element(null, {set, Of}) ->
    sets:new();
format_element(Values, {set, Of}) ->
    sets:from_list(lists:map(fun(Value) ->
		      format_element(Value, Of)
	      end, sets:to_list(Values)));
format_element(null,  {list, Of}) ->
    [].
