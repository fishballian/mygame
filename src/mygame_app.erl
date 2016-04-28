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
    mygame_sup:start_link().

    stop(_State) ->
    ok.