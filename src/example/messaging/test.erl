-module(test).

-export([go/0, tree/0]).


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
 

    [account:add_resource(Account1) || _X  <- lists:seq(1,1)],
    [account:add_resource(Account2) || _X  <- lists:seq(1,1)],



    account:add_resource(Account1), 
    account:print_state(Account1),


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
    


tree() -> 
    %% filter := or [<<filters>>...]
    %%         | and [<<filters>>...]
    %%         | present <<string>>
    %%         | memberof <<string>>


    
    
    Grammar = [{root, filter},
	      {rules, [[{name, filter},
			{nodes, [i:node('or',  [i:list(i:ref(filter))]),
				 i:node('and', [i:list(i:ref(filter))]),
				 i:node('present',  [i:string()]),
				 i:node('memberof', [i:string()])]}
		       ]]}],
    
    i:parse_tree_schema(Grammar).
