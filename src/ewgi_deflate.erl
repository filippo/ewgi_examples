%% @author Filippo Pacini <filippo.pacini@gmail.com>
%% @copyright 2009 S.G. Consulting.

%% @doc deflate ewgi middleware.

-module(ewgi_deflate).
-author('Filippo Pacini <filippo.pacini@gmail.com>').

-export([handle/1]).

-define(ENCODABLE, ["text/plain", "text/html", "text/xml"]).

handle(Ctx) ->
    %% get the accept encoding header
    AcceptEnc = ewgi_api:get_header_value("accept-encoding", Ctx),
    Hdrs = ewgi_api:response_headers(Ctx),
    %% check gzip/deflate
    Ctx1 = case can_encode_response(Hdrs) of
               true ->
                   case parse_encoding(AcceptEnc) of
                       false ->
                           Ctx;
                       gzip ->
                           Body1 = zlib:gzip(ewgi_api:response_message_body(Ctx)),
                           Hdrs1 = [{"Content-encoding", "gzip"}|Hdrs],
                           ewgi_api:response_headers(
                             Hdrs1, 
                             ewgi_api:response_message_body(Body1, Ctx)
                            );
                       deflate ->
                           Body1 = zlib:compress(ewgi_api:response_message_body(Ctx)),
                           Hdrs1 = [{"Content-encoding", "deflate"}|Hdrs],
                           ewgi_api:response_headers(
                             Hdrs1, 
                             ewgi_api:response_message_body(Body1, Ctx)
                            )
                   end;
               false ->
                   Ctx
           end,
    Ctx1.

can_encode_response(Headers) ->
    ContentType = proplists:get_value("Content-type", Headers),
    ContentEnc = proplists:get_value("Content-encoding", Headers),
    can_encode_response1(ContentType, ContentEnc).

can_encode_response1(ContentType, undefined) ->
    lists:member(ContentType, ?ENCODABLE);
can_encode_response1(_ContentType, _ContentEncoding) ->
    %% the response is already encoded in some way so we don't do anything
    false.

parse_encoding(undefined) ->
    false;
parse_encoding(Encoding) ->
    %% FIXME: read the specs and see how to do this properly (check the q=X and the order)
    case string:str(Encoding, "gzip") of
        X when X>0 ->
            gzip;
        _ ->
            case string:str(Encoding, "deflate") of
                Y when Y>0 ->
                    deflate;
                _ ->
                    false
            end
    end.
