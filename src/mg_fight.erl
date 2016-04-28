%%%-------------------------------------------------------------------
%%% @author yuanxiaopeng(fishballian@live.com)
%%% @copyright (C) 2016, <mc>
%%% @doc
%%%
%%% @end
%%% Created : 2016-04-28 10:59:12.080598
%%%-------------------------------------------------------------------
-module(mg_fight).
-author("yuanxiaopeng").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%-record(r_actor, {id :: non_neg_integer(),
%                  hp :: non_neg_integer(),
%                  attack :: non_neg_integer(),
%                  defence :: non_neg_integer()}).
%
%-record(r_fight_record, {id :: non_neg_integer(),
%                         actor1 :: any(),
%                         actor2 :: any(),
%                         round_records :: list()}).
%
%%% API
%-export([fight/2]).
%
%%% @spec fight(Actor1 :: #r_actor{}, Actor2 :: #r_actor{}) -> #r_fight_record{}
%%% @doc fight
%fight(Actor1, Actor2) ->
%    RoundRecords = do_fight(Actor1, Actor2, 1, []),
%    #r_fight_record{actor1 = Actor1, actor2 = Actor2, round_records = RoundRecords}.
%
%%% Internel
%%% @spec do_fight(Actor1 :: #r_actor{}, Actor2 :: #r_actor{}, Round :: pos_integer(), RoundRecords :: list()) -> list()
%%% @doc do fight
%do_fight(Actor1, Actor2, Round, RoundRecords) ->
%   []. 



    

%% Eunit
-ifdef(TEST).

-endif.



