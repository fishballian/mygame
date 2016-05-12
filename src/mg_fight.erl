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

-record(fight_log, {id :: non_neg_integer(), 
                    left,
                    right,
                    result :: non_neg_integer(),
                    mods = [] :: [mod()],
                    rounds = [] :: list()}).

-record(round_log, {num :: non_neg_integer(),
                    events :: list()}).

-record(attack_event, {src_pos :: non_neg_integer(),
                     target_effects:: list(),
                     skill_id :: non_neg_integer()}).

-record(newpet_event, {new_pets :: list()}).
        

-record(effect, {pos :: non_neg_integer(),
                 type :: non_neg_integer(), 
                 value :: any()}).



-record(fight_data, {left, right, round}).


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
pk2(_LDonePets, _LReadyPets, [], [], _Fight) ->
    left_win;
%% left all dead, right win
pk2([], [], _RDonePets, _ReadyPets, _Fight) ->
    right_win;
pk2(_LReadyPets, [], _RDonePets, [], _Fight) ->
    next_round;
pk2(_LDonePets, _LReadyPets, _RDonePets, _ReadyPets, _Fight) ->
    next_attack.

%% Eunit
-ifdef(TEST).

-endif.



