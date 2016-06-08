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

-include("mygame.hrl").

-record(fight_log, {left, right, winner, round_logs}).
-record(side_info, {name, pets = []}).
-record(state, {round_logs = [], round = 0}).
-record(round_log, {attacks = []}).
-record(pet, {pos, seq, hp, phy_attack, done, skill}).
-record(attack_log, {src, type, effect_list}).
-record(effect_log, {dst, hp_change, hp_remain}).
-record(c_effect, {type, value, area}).

-define(SKILL_NORMAL, 1).
-define(SKILL_BACKSTAB, 2).
-define(SKILL_HORIZONTAL, 3).
-define(SKILL_VERTICAL, 4).
-define(SKILL_ATTACK_ALL, 5).
-define(SKILL_HEAL_FRIENDS, 6).

-export([pk/2]).
-export([test/0]).

-type side_info() :: #side_info{}. 

-spec pk(Left, Right) -> {ok, FightLog} when
      Left :: side_info(),
      Right :: side_info(),
      FightLog :: #fight_log{}.
pk(Left, Right) ->
    {ok, Winner, RoundLogs} = do_pk(Left, Right),
    FightLog = #fight_log{left = Left, right = Right, winner = Winner, round_logs = lists:reverse(RoundLogs)},
    {ok, FightLog}.

%% API

%% Internel
-spec do_pk(Left, Right) -> {ok, Winner, RoundLogs} when
      Left :: side_info(),
      Right :: side_info(),
      Winner :: left | right | draw,
      RoundLogs :: list().
do_pk(Left, Right) ->
    #side_info{pets = LPets} = Left,
    #side_info{pets = RPets} = Right,
    do_pk2(LPets, RPets, #state{}). 

%% both all dead, draw
do_pk2([], [], State) ->
    #state{round_logs = RoundLogs} = State,
    {ok, draw, RoundLogs};
%% right all dead, left win
do_pk2(_LPets, [], State) ->
    #state{round_logs = RoundLogs} = State,
    {ok, left, RoundLogs};
%% left all dead, left win
do_pk2([], _RPets, State) ->
    #state{round_logs = RoundLogs} = State,
    {ok, right, RoundLogs};
%% new round
do_pk2(LPets, RPets, State) ->
    #state{round = Round, round_logs = RoundLogs} = State,
    NewRound = Round + 1,
    {NewLPets, NRPets, Attacks} = round_fight(LPets, RPets),
    NewRoundLog = #round_log{attacks = Attacks},
    NewRoundLogs = [NewRoundLog | RoundLogs],
    NewState = State#state{round = NewRound, round_logs = NewRoundLogs},
    do_pk2(NewLPets, NRPets, NewState). 

round_fight(LPets, RPets) ->
    LPetKvs = [{{left, Pet#pet.pos}, Pet#pet{done = false}} || Pet <- LPets],
    RPetKvs = [{{right, Pet#pet.pos}, Pet#pet{done = false}} || Pet <- RPets],
    AllPetDict = dict:from_list(LPetKvs ++ RPetKvs),
    round_fight2(AllPetDict, []).

round_fight2(AllPetDict, Attacks) ->
    AllPets = dict:to_list(AllPetDict),
    AllReadyPetKvs = [{K, Pet} || {K, Pet} <- AllPets, Pet#pet.done =/= true], 
    case AllReadyPetKvs of
        [] ->
            %% no ready pet, round end
            {LPets, RightPets} = lists:foldl(fun(I, {LAcc, RAcc}) ->
                                                     {{Side, _Pos}, Pet} = I,
                                                     case Side of
                                                         left ->
                                                             {[Pet | LAcc], RAcc};
                                                         right ->
                                                             {LAcc, [Pet | RAcc]}
                                                     end
                                             end, {[], []}, AllPets),
            {LPets, RightPets, lists:reverse(Attacks)};
        _ ->
            %% next fight
            {FightSide, FightPos} = get_next_fight_pos(AllReadyPetKvs),
            {NewAllPetDict, AddAttacks} = fight({FightSide, FightPos}, AllPetDict),
            round_fight2(NewAllPetDict, AddAttacks ++ Attacks)
    end.


get_next_fight_pos(AllReadyPetKvs) ->
    AllReadyPoses = [{Pet#pet.seq, Side, Pos} || {{Side, Pos}, Pet} <- AllReadyPetKvs],
    [{_Seq, FightSide, FightPos} | _] = lists:sort(AllReadyPoses),
    {FightSide, FightPos}.


fight(FightKey, AllPetDict) ->
    {ok, FightPet} = dict:find(FightKey, AllPetDict),
    NewFightPet = FightPet#pet{done = true},
    AllPetDict2 = dict:store(FightKey, NewFightPet, AllPetDict),
    case get_target_pet_key(FightKey, AllPetDict2) of
        no_target ->
            NewAllPetDict = AllPetDict2, 
            {NewAllPetDict, []};
        TargetKey ->
            Skill = FightPet#pet.skill,
            CEffects = get_skill_effects(Skill),
            lists:foldl(fun(I, {AllPetDictAcc, AttackAcc}) ->
                                {NewAllPetDictAcc, EffectLogs} = apply_effect(FightKey, TargetKey, I, AllPetDictAcc),
                                Attack = #attack_log{src = FightKey, type = I#c_effect.type, effect_list = EffectLogs},
                                NewAttackAcc = [Attack | AttackAcc],
                                {NewAllPetDictAcc, NewAttackAcc}
                        end, {AllPetDict2, []}, CEffects)
    end.

apply_effect(FightKey, TargetKey, CEffect, AllPetDict) ->
    #c_effect{area = Area, value = Value, type = Type} = CEffect,
    AffectPetKeys = get_area_pet_keys(Area, TargetKey, AllPetDict),
    {ok, FightPet} = dict:find(FightKey, AllPetDict),
    apply_effect2(Type, AffectPetKeys, Value, FightPet, AllPetDict).

apply_effect2(hurt, AffectPetKeys, Value, _FightPet, AllPetDict) ->
    lists:foldl(fun(I, {AllPetDictAcc, EffectAcc}) ->
                        {ok, AffectPet} = dict:find(I, AllPetDictAcc),
                        #pet{hp = THP} = AffectPet,
                        ReduceHp = Value,
                        NewTHP = erlang:max(0, THP - ReduceHp),
                        Effect = #effect_log{dst = I, hp_change = - ReduceHp, hp_remain = NewTHP},
                        NewEffectAcc = [Effect | EffectAcc],
                        case NewTHP > 0 of
                            true ->
                                NewAffectPet = AffectPet#pet{hp = NewTHP},
                                NewAllPetDictAcc = dict:store(I, NewAffectPet, AllPetDictAcc),
                                {NewAllPetDictAcc, NewEffectAcc};
                            _ ->
                                NewAllPetDictAcc = dict:erase(I, AllPetDictAcc),
                                {NewAllPetDictAcc, NewEffectAcc} 
                        end
                end, {AllPetDict, []}, AffectPetKeys);
apply_effect2(heal, AffectPetKeys, Value, _FightPet, AllPetDict) ->
    lists:foldl(fun(I, {AllPetDictAcc, EffectAcc}) ->
                        {ok, AffectPet} = dict:find(I, AllPetDictAcc),
                        #pet{hp = THP} = AffectPet,
                        AddHp = Value,
                        MaxHp = 99999999,
                        NewTHP = erlang:min(MaxHp, THP + AddHp),
                        Effect = #effect_log{dst = I, hp_change = AddHp, hp_remain = NewTHP},
                        NewEffectAcc = [Effect | EffectAcc],
                        case NewTHP > 0 of
                            true ->
                                NewAffectPet = AffectPet#pet{hp = NewTHP},
                                NewAllPetDictAcc = dict:store(I, NewAffectPet, AllPetDictAcc),
                                {NewAllPetDictAcc, NewEffectAcc};
                            _ ->
                                NewAllPetDictAcc = dict:erase(I, AllPetDictAcc),
                                {NewAllPetDictAcc, NewEffectAcc} 
                        end
                end, {AllPetDict, []}, AffectPetKeys).



get_target_pet_key(FightKey, AllPetDict) ->
    {_Side, Pos} = FightKey,
    AttackSeqs = case Pos rem 3 of
                     1 -> [1, 4, 7, 2, 5, 8, 3, 6, 9];
                     2 -> [2, 5, 8, 1, 4, 7, 3, 6, 9];
                     0 -> [3, 6, 9, 1, 4, 7, 2, 5, 8]
                 end,
    get_target_pet_key(FightKey, AllPetDict, AttackSeqs).

get_target_pet_key(_FightKey, _AllPetDict, []) ->
    no_target;
get_target_pet_key(FightKey, AllPetDict, [Pos | T]) ->
    {FSide, _FPos} = FightKey,
    OppoSide = get_oppo_side(FSide),
    case dict:find({OppoSide, Pos}, AllPetDict) of
        {ok, _TargetPet} ->
            {OppoSide, Pos};
        _ ->
            get_target_pet_key(FightKey, AllPetDict, T)
    end.

get_oppo_side(Side) ->
    case Side of
        left -> right;
        right -> left
    end.

get_area_pet_keys(direct, TargetKey, _AllPetDict) ->
    [TargetKey];
get_area_pet_keys(back, TargetKey, AllPetDict) ->
    {Side, Pos} = TargetKey,
    AttackSeqs = case Pos rem 3 of
                     1 -> [7, 4, 1];
                     2 -> [8, 5, 2];
                     0 -> [9, 6, 3]
                 end,
    [Key | _] = get_exists_keys(Side, AttackSeqs, AllPetDict),
    [Key];
get_area_pet_keys(horizontal, TargetKey, AllPetDict) ->
    {Side, Pos} = TargetKey,
    Poses = case Pos rem 3 of
                1 -> [1, 4, 7];
                2 -> [2, 5, 8];
                0 -> [3, 6, 9]
            end,
    get_exists_keys(Side, Poses, AllPetDict);
get_area_pet_keys(vertical, TargetKey, AllPetDict) ->
    {Side, Pos} = TargetKey,
    Poses = case (Pos - 1) div 3 of
                0 -> [1, 2, 3];
                1 -> [4, 5, 6];
                2 -> [7, 8, 9]
            end,
    get_exists_keys(Side, Poses, AllPetDict);
get_area_pet_keys(all_enemy, TargetKey, AllPetDict) ->
    {Side, _Pos} = TargetKey,
    Poses = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    get_exists_keys(Side, Poses, AllPetDict);
get_area_pet_keys(all_friend, TargetKey, AllPetDict) ->
    {Side, _Pos} = TargetKey,
    Poses = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    get_exists_keys(get_oppo_side(Side), Poses, AllPetDict).

get_exists_keys(Side, Poses, AllPetDict) ->
    lists:foldl(fun(I, Acc) ->
                       case dict:find({Side, I}, AllPetDict) of
                           {ok, _Pet} ->
                               [{Side, I} | Acc];
                           error ->
                               Acc
                       end
               end, [], Poses).

get_skill_effects(?SKILL_NORMAL) ->
    [#c_effect{type = hurt, value = 20, area = direct}];
get_skill_effects(?SKILL_BACKSTAB) ->
    [#c_effect{type = hurt, value = 25, area = back}];
get_skill_effects(?SKILL_HORIZONTAL) ->
    [#c_effect{type = hurt, value = 15, area = horizontal}];
get_skill_effects(?SKILL_VERTICAL) ->
    [#c_effect{type = hurt, value = 15, area = vertical}];
get_skill_effects(?SKILL_ATTACK_ALL) ->
    [#c_effect{type = hurt, value = 10, area = all_enemy}];
get_skill_effects(?SKILL_HEAL_FRIENDS) ->
    [#c_effect{type = hurt, value = 20, area = direct},
     #c_effect{type = heal, value = 15, area = all_friend}];
get_skill_effects(_SkillID) ->
    not_found.

test() ->
    Pet1 = #pet{pos = 1, seq = 1, hp = 100, phy_attack = 25, skill = ?SKILL_ATTACK_ALL},
    Pet2 = #pet{pos = 2, seq = 2, hp = 150, phy_attack = 20, skill = ?SKILL_BACKSTAB},
    Pet3 = #pet{pos = 3, seq = 3, hp = 75, phy_attack = 30, skill = ?SKILL_HEAL_FRIENDS},
    Left = #side_info{name = "Robot1", pets = [Pet1, Pet3]},
    Right = #side_info{name = "Robot2", pets = [Pet1, Pet2]},
    pk(Left, Right).

%% Eunit
-ifdef(TEST).

-endif.

