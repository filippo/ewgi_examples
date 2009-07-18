%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc Post example

-module(ewgi_post).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([post_app/1]).

post_app(Ctx) ->
    Parser = post_parse_middleware(2097152, %% 2 MB maximum
                                   fun display_form_data/1,
                                   fun post_app_error/1),
    case ewgi_api:request_method(Ctx) of
        'GET' ->
            display_form(Ctx);
        'POST' ->
            Parser(Ctx)
    end.

post_app_error({ewgi_context, Request, _}) ->
    Response = {ewgi_response, {400, "BAD REQUEST"}, [],
                [<<"Maximum content-length exceeded.">>],
                undefined},
    {ewgi_context, Request, Response}.

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

%% MaxLength is the maximum size (in bytes) that the server will
%% receive from the client.  App should be the application called when
%% the parse is successful (or unnecessary).  ErrApp should be an
%% error application when the content length exceeds the maximum
%% specified limit.
post_parse_middleware(MaxLength, App, ErrApp)
  when is_integer(MaxLength), MaxLength > 0, is_function(App, 1) ->
    fun(Ctx) ->
            case ewgi_api:request_method(Ctx) of
                Method when Method =:= 'POST';
                            Method =:= 'PUT' ->
                    case ewgi_api:remote_user_data(Ctx) of
                        undefined ->
                            %% Check content-type first
                            Ct = ewgi_api:content_type(Ctx),
                            parse_post(Ctx, App, ErrApp, parse_ct(Ct), MaxLength);
                        _ ->
                            App(Ctx)
                    end;
                _ ->
                    App(Ctx)
            end
    end.

%% Parse content-type (ignoring additional vars for now)
%% Should look like "major/minor; var=val"
parse_ct(L) when is_list(L) ->
    case string:tokens(L, ";") of
        [H|_] ->
            H;
        _ ->
            undefined
    end.

parse_post(Ctx, App, ErrApp, "application/x-www-form-urlencoded", Max) ->
    case smak_ewgi:content_length(Ctx) of
        L when is_integer(L), L > Max ->
            ErrApp(Ctx);
        L when is_integer(L), L > 0 ->
            Input = read_input_string(Ctx, L),
            Vals = ewgi_api:parse_post(Input),
            Ctx1 = ewgi_api:remote_user_data(Vals, Ctx),
            App(Ctx1);
        _ ->
            App(Ctx)
    end;
parse_post(Ctx, App, _, _, _) ->
    %% Silently ignore other content-types
    App(Ctx).

read_input_string(Ctx, L) when is_integer(L), L > 0 ->
    R = ewgi_api:read_input(Ctx),
    iolist_to_binary(R(read_input_string_cb([]), L)).

read_input_string_cb(Acc) ->
    fun(eof) ->
            lists:reverse(Acc);
       ({data, B}) ->
            read_input_string_cb([B|Acc])
    end.
