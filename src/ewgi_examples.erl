%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(ewgi_examples).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the ewgi_examples server.
start() ->
    ewgi_examples_deps:ensure(),
    ensure_started(crypto),
    application:start(ewgi_examples).

%% @spec stop() -> ok
%% @doc Stop the ewgi_examples server.
stop() ->
    Res = application:stop(ewgi_examples),
    application:stop(crypto),
    Res.
