%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the ewgi_examples application.

-module(ewgi_examples_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ewgi_examples.
start(_Type, _StartArgs) ->
    ewgi_examples_deps:ensure(),
    ewgi_examples_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ewgi_examples.
stop(_State) ->
    ok.
