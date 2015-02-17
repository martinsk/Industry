%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(account).

-behaviour(factory_worker).

-export([send_message/3,
	 message_count/1,
	 print_state/1]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 

print_state({Id, Type}) ->
    factory_worker:call({Id, Type}, print_state).

send_message(Sender, Msg, Recepient) ->
    factory_worker:cast(Sender, {send_message, Msg, Recepient}).

message_count({Id, Type}) ->
    factory_worker:call({Id, Type}, message_count).


schema() ->
    [{name, "account"},
     {type, account},
     {module, ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    {ok, [{counter, 0 } | Props]}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(WorkerState) -> {ok, WorkerState}.


handle_call(print_state, _From, State) ->
    io:format("State ~p ~n", [State]),
    {reply, ok, State};
handle_call(message_count,_From, State) ->
    Counter  = proplists:get_value(counter, State),
    {reply, Counter, State};
handle_call(_Request, _From, State) ->
    lager:warning("account:handle_call"),
    {reply, ok, State}.


handle_cast({send_message, Msg, Recepient}, State) ->
    factory_worker:cast(Recepient, {receive_message, Msg}),
    {noreply, State};
handle_cast({receive_message, _Msg}, State) ->
    Counter  = proplists:get_value(counter, State),
    {noreply, [{counter, Counter +1} | State]};
handle_cast(Msg, State) ->
    io:format("account:handle_cast ~p ~n", [Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    lager:warning("account:handle_info"),
    {noreply, State}.



