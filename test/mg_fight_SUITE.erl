%%%-------------------------------------------------------------------
%%% @author yuanxiaopeng
%%% @copyright (C) 2016, mc
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-14 12:04:52.368185
%%%-------------------------------------------------------------------
-module(mg_fight_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).
-export([testcase1/1]).

%% test cases
-export([
         %% TODO: test case names go here
        ]).

%%-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     testcase1
     %% TODO: Group names here e.g. {group, crud}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        %% TODO: group definitions here e.g.
        %% {crud, [], [
        %%          t_create_resource,
        %%          t_read_resource,
        %%          t_update_resource,
        %%          t_delete_resource
        %%         ]}

    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->

    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
testcase1(_Config) ->
    {ok, _} = mg_fight:test().

