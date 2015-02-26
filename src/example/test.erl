-module(test).

-export([go/0, volume/2, cql/1]).


app_start() ->
    %% hackary to get things started.
    application:start(druuid),
    application:start(rafter),
    application:start(seestar),
    application:start(industry).
    

go() ->
    app_start(),
    
    DBWorkerSchema  = db_worker:schema(),
    industry:add_factory(DBWorkerSchema),


    OrganizationSchema = organization:schema(),
    ResourceSchema     = resource:schema(),
    ApiKeySchema       = api_key:schema(),
    AccountSchema      = account:schema(),
    MessageSchema      = message:schema(),

    Schemas = [ResourceSchema,
	       AccountSchema, 
	       ApiKeySchema,
	       OrganizationSchema, 
	       MessageSchema],
    
    %% hack to get off the ground
    [ factory:create(db_worker, [{id, X}]) || X <- lists:seq(1,10)],
    db_worker:create_tables(Schemas), 

    %% setup the factories
    industry:add_factories(Schemas),

    {Org, organization} = factory:create(organization, []),

    Account1Spec = [{first_name,"James"},{last_name,"Dean"}, {organization, Org}],
    Account2Spec = [{first_name,"Pete"} ,{last_name,"Pony"}, {organization, Org}],
    
    Account1 = {A, account} = factory:create(account, Account1Spec),
    Account2 = {B, account} = factory:create(account, Account2Spec),
 
    [account:add_resource(Account1) || _X  <- lists:seq(1,10)],

    account:add_resource(Account1), 
    account:print_state(Account1),

    Keys = account:get_api_keys(Account1),
    [Key | _ ] = sets:to_list(Keys),
    Valid = api_key:validate(Key, "TopSecret"),
    
    lager:warning("Key = ~p", [Key]), 
    lager:warning("Valid = ~p", [Valid]), 

    {Ts, _} = timer:tc(fun() -> 
			       {MsgId, message} = message:create(A, B, "Hello There"),
			       erlang:yield(),
			       1 = account:message_count(Account2),
			       account:print_state(Account2),
			       message:change_status(MsgId, delivered), 
			       message:change_status(MsgId, read) 
		       end),
    lager:warning("sending a message took ~p us", [Ts]),
    Ts.
    

volume(AccountCount, MessageCount) ->
    app_start(),

    DBWorkerSchema  = db_worker:schema(),
    industry:add_factory(DBWorkerSchema),
    
    [factory:create(db_worker, [{id, X}]) || X <- lists:seq(1,10)],

    
    Schema  = account:schema(),
    
    AccountSpec = [{first_name, "martin"},
		   {last_name, "Kristiansen"}],
    
    industry:add_factory(Schema),
    
    Accounts = [ factory:create(account, AccountSpec) || _X <- lists:seq(1,AccountCount)],
    

    L = length(Accounts),
    Pairs = [ begin 
		  Idx = random:uniform(L),
		  {Idx, A}
	      end  || A  <- Accounts, 
		      _X <- lists:seq(1, MessageCount)],
    
    AccountPairs = permutate(match_index(Pairs, Accounts)),

    lists:foldr(fun(A, Acc) ->
			Acc + account:message_count(A)
		end, 0, Accounts),

	
    {Ts1, _} = timer:tc(fun() ->
			       lists:foreach(fun({A, B}) ->
						     account:send_message(A, "Hello", B)
					     end, AccountPairs)
		       end),
    io:format("send_message_times ~p ~n", [Ts1]),
   
    %% count the messages
    {Ts2, R} = timer:tc(fun() ->
			       lists:foldr(fun(A, Acc) ->
						   Acc + account:message_count(A)
					   end, 0, Accounts)
		       end),
    io:format("time to count ~p ~n", [Ts2]),
    io:format("messges per sec  ~p ~n", [MessageCount * AccountCount / (Ts1*1.0 / 1000000.0)]),

    Ts2.


match_index(Idxs, Accounts) ->
    match_index(lists:sort(Idxs), 
		1, Accounts, []).

match_index([], Idx, Accts, Acc) -> Acc;
match_index([{Idx, A1}| ITail ], Idx, [A2 | ATail], Acc) ->
    match_index(ITail, Idx, [A2 | ATail], [{A1, A2} | Acc]);
match_index(Idxs, Idx, [_| ATail], Acc) ->
    match_index(Idxs, Idx+1, ATail, Acc).


permutate(Ls) ->
    RandPrefix = [ {random:uniform(), N} || N <- Ls],
    [ X || {_,X} <- lists:sort(RandPrefix)].



cql(Count) ->
    {ok, Pid} = seestar_session:start_link("localhost", 9042),
    Q = "INSERT INTO industrytest.seestar_test_table (id, test1, value) VALUES (?,?,?)",
    {ok, R} = seestar_session:prepare(Pid, Q),
    QryID = seestar_result:query_id(R),
    Types = seestar_result:types(R),
    
    {Ts, _} = timer:tc(fun() ->
			       [begin
				    Row = [random:uniform(1000000000), <<"this">>, <<"that">>],
				    {ok, _} = seestar_session:execute(Pid, QryID, Types, Row, one)
				end || X <- lists:seq(1,Count)]
		       end),
    io:format("time to insert ~p ~n", [Ts]),
    io:format("inserts per sec  ~p ~n", [Count / (Ts*1.0 / 1000000.0)]).
    

