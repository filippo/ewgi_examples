%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc Hello world application and to_upper middleware

-module(ewgi_hello).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([hello_app/1, to_upper/1]).

hello_app({ewgi_context, Request, _Response}) ->
    ResponseHeaders = [{"Content-type", "text/plain"}],
    Response = {ewgi_response, {200, "OK"}, ResponseHeaders,
                [<<"Hello world!">>], undefined},
    {ewgi_context, Request, Response}.

to_upper(Ctx) ->
    Body = ewgi_api:response_message_body(Ctx),
    Body1 = [string:to_upper(erlang:binary_to_list(B)) || B <- Body],
    ewgi_api:response_message_body(Body1, Ctx).
