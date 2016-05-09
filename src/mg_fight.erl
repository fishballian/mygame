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

%% API
-export([pk/3]).
-export([pk2/5]). %% @todo del

-type side_info() :: any().
-type mod() :: any().

%% @doc pk
-spec pk(side_info(), side_info(), [mod()])-> any().
pk(_LeftInfo, _RightInfo, _Mods) ->
    ok.

%% Internel
%% right all dead, left win
pk2(_LDonePets, _LReadyPets, [], [], _Mods) ->
    left_win;
%% left all dead, right win
pk2([], [], _RDonePets, _ReadyPets, _Mods) ->
    right_win;
pk2(_LDonePets, _LReadyPets, _RDonePets, _ReadyPets, _Mods) ->
    next_round.

%% Eunit
-ifdef(TEST).

-endif.



