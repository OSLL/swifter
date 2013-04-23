%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc test.

-module(test).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the test server.
start() ->
    test_deps:ensure(),
    ensure_started(crypto),
    application:start(test).


%% @spec stop() -> ok
%% @doc Stop the test server.
stop() ->
    application:stop(test).
