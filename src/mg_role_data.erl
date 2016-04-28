%%%-------------------------------------------------------------------
%%% @author yuanxiaopeng(fishballian@live.com)
%%% @copyright (C) 2016, <mc>
%%% @doc
%%%
%%% @end
%%% Created : 2016-04-28 10:26:46.890584
%%%-------------------------------------------------------------------
-module(mg_role_data).
-author("yuanxiaopeng").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("role.hrl").

%% API
-export([get_role_base/1, set_role_base/2, get_role_attr/1, set_role_attr/2]).
%% @spec get_role_base(RoleID :: non_neg_integer()) -> #r_role_base{}
%% @doc get role base
get_role_base(RoleID) ->
    erlang:get({role_base, RoleID}).

%% @spec set_role_base(RoleID :: non_neg_integer(), Base :: #r_role_base{}) -> any()
%% @doc set role base 
set_role_base(RoleID, Base) ->
    erlang:put({role_base, RoleID}, Base).

%% @spec get_role_attr(RoleID :: non_neg_integer()) -> #r_role_attr{}
%% @doc get role attr
get_role_attr(RoleID) ->
    erlang:get({role_attr, RoleID}).

%% @spec set_role_attr(RoleID :: non_neg_integer(), Base :: #r_role_attr{}) -> any()
%% @doc set role attr 
set_role_attr(RoleID, Attr) ->
    erlang:put({role_attr, RoleID}, Attr).

%% Internel

%% Eunit
-ifdef(TEST).

-endif.



