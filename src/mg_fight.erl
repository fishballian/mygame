%%%-------------------------------------------------------------------
%%% @author yuanxiaopeng(fishballian@live.com)
%%% @copyright (C) 2016, <mc>
%%% @doc
%%%
%%% @end
%%% Created : 2016-05-09 11:26:44.407904
%%%-------------------------------------------------------------------
-module(mg_fight).
-author("yuanxiaopeng").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(fight_log, {left, right, winner, round_logs}).

-type side_info() :: any().

-spec pk(Left, Right, Options) -> {ok, FightLog} when
    Left :: side_info(),
    Right :: side_info(),
    Options :: list(),
    FightLog :: #fight_log{}.
pk(Left, Right, Options) ->
    {ok, Winner, RoundLogs} = do_pk(Left, Right, Options),
    FightLog = #fight_log{left = Left, right = Right, winner = Winner, round_logs = RoundLogs},
    {ok, FightLog}.

%% API

%% Internel
-spec do_pk(Left, Right, Options) -> {ok, Winner, RoundLogs} when
    Left :: side_info(),
    Right :: side_info(),
    Options :: list(),
    Winner :: left | right,
    RoundLogs :: list().
do_pk(Left, Right, Options) ->
    {ok, left, []}.

%% Eunit
-ifdef(TEST).

-endif.



