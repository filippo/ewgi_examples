%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting s.r.l.

%% @doc Web server for ewgi_examples.

-module(ewgi_examples_web).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([start/1, 
         stop/0, 
         loop/2,
         dispatcher/1]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    Mod = ewgi_mochiweb:new(fun ?MODULE:dispatcher/1),
    Mod:run(Req).

dispatcher(Ctx) ->
    dispatch(ewgi_api:path_info(Ctx), Ctx).

dispatch("/hello", Ctx) ->
    ewgi_hello:hello_app(Ctx);
dispatch("/HELLO", Ctx) ->
    ewgi_hello:to_upper(ewgi_hello:hello_app(Ctx));
dispatch("/postex", Ctx) ->
    ewgi_post:post_app(Ctx);
dispatch(_, Ctx) ->   
    ewgi_api:response_message_body("404 Not Found", 
                                   ewgi_api:response_status({404, "Not Found"}, Ctx)).
    
%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
