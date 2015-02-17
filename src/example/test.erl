-module(test).

-export([go/0, volume/2]).

go() ->
    application:start(industry),
    Schema  = account:schema(),
    AccountSpec = [{name, "martin"}],
    
    industry:add_factory(Schema),
    Account1 = factory:create(account, AccountSpec),
    account:print_state(Account1),

    Account2 = factory:create(account, AccountSpec),
    account:send_message(Account1, "Hello there", Account2).


volume(AccountCount, MessageCount) ->
    application:start(industry),
    Schema  = account:schema(),
    AccountSpec = [{name, "martin"}],
    
    industry:add_factory(Schema),
    
    Accounts = [ factory:create(account, AccountSpec) || _X <- lists:seq(1,AccountCount)],
    
    Pairs = [begin
		 Index1 = random:uniform(length(Accounts)),
		 Index2 = random:uniform(length(Accounts)),
		 A1 = lists:nth(Index1,Accounts),
		 A2 = lists:nth(Index2,Accounts),
		 {A1, A2}
	     end || _X <- lists:seq(1, MessageCount)],

    lists:foldr(fun(A, Acc) ->
			Acc + account:message_count(A)
		end, 0, Accounts),
	
    {Ts1, _} = timer:tc(fun() ->
			       lists:foreach(fun({A, B}) ->
						     account:send_message(A, "Hello", B)
					     end, Pairs)
		       end),
   
    %% count the messages
    {Ts2, R} = timer:tc(fun() ->
			       lists:foldr(fun(A, Acc) ->
						   Acc + account:message_count(A)
					   end, 0, Accounts)
		       end),
    io:format("send_message_times ~p ~n ", [Ts1] ),
    io:format("time to count ~p ~n ", [Ts2] ),
    R.
