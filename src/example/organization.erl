%%%-------------------------------------------------------------------
%%% @author Martin Kristiansen <msk@ajour.io>
%%% @copyright (C) 2015, Martin Kristiansen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(organization).

-behaviour(factory_worker).

-export([add_member/2, 
	 list_members/1]).

-export([schema/0, new/1, loaded/1, handle_call/3, handle_cast/2, handle_info/2]).
 

add_member({Id, Type}, Member) ->
    factory_worker:call({Id, ?MODULE}, {add_member, Member}).

list_members({Id, Type}) ->
    factory_worker:call({Id, ?MODULE}, list_members).

schema() ->
    [{name,   "organization"},
     {type,   ?MODULE},
     {attributes, [
		   {id      , i:integer()},
		   {members , i:set(i:ref(account))}
		  ]},
     {options, [{on_create, {db_worker, insert}},
		{on_change, {db_worker, update}},
		{on_delete, {db_worker, delete}}]},
     {timeout, timer:seconds(10)},
     {module, ?MODULE}].

-spec new(term()) -> {ok, factory_worker:worker_state()}.
new(Props) -> 
    Members = [{members, sets:new()}],
    {ok, i:state(Members,Props)}.

-spec loaded(factory_worker:worker_state()) -> 
		    {ok, factory_worker:worker_state()}.
loaded(WorkerState) -> {ok, WorkerState}.


handle_call({add_member, Member}, _From, State) ->
    Members    = i:get(members ,State),
    NewMembers = sets:add_element(Member, Members),
    NewState   = i:state([{members, NewMembers}],State),
    {reply, ok, NewState};
handle_call(list_members, _From, State) ->
    Members  = i:get(members, State),
    {reply, sets:to_list(Members), State};
handle_call(_Request, _From, State) ->
    lager:warning("organization:handle_call"),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    io:format("organization:handle_cast ~p ~n", [Msg]),
    {noreply, State}.


handle_info(_Info, State) ->
    lager:warning("organization:handle_info"),
    {noreply, State}.



