%% @author Mochi Media <dev@mochimedia.com>
%% @copyright test Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the test application.

-module(test_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for test.
start(_Type, _StartArgs) ->
    test_deps:ensure(),
    test_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for test.
stop(_State) ->
    ok.
