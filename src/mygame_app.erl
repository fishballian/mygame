-module(mygame_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(mygame, 100,
                                   ranch_tcp, [{port, 5555}],
                                   mg_protocol, []
                                  ),
    {ok, Pid} = mchat:new_channel(),
    erlang:register(c1, Pid),
    mygame_sup:start_link().

stop(_State) ->
    ranch:stop_listener(mygame),
    ok.
