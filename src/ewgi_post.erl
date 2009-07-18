%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc Post example

-module(ewgi_post).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([post_app/1]).

post_app(Ctx) ->
    case ewgi_api:request_method(Ctx) of
        'GET' ->
            display_form(Ctx);
        'POST' ->
            display_form_data(Ctx)
    end.

display_form_data({ewgi_context, Request, _Response}=Ctx) ->
    Body = ewgi_api:remote_user_data(Ctx),
    ResponseHeaders = [{"Content-type", "text/plain"}],
    Response = {ewgi_response, 
                {200, "OK"}, 
                ResponseHeaders,
                [Body], undefined},
    {ewgi_context, Request, Response}.

display_form({ewgi_context, Request, _Response}) ->
    Body = <<"<form action=\"/postex\" method=\"post\">
Un: <input type=\"text\" name=\"un\" value=\"\"/>
<br/>
Pw: <input type=\"text\" name=\"pw\" value=\"\"/>
<br/><br/>
<input type=\"submit\" name=\"submit\" value=\"Login\"/>
</form>">>,
    ResponseHeaders = [{"Content-type", "text/html"}],
    Response = {ewgi_response, 
                {200, "OK"}, 
                ResponseHeaders,
                [Body], undefined},
    {ewgi_context, Request, Response}.

