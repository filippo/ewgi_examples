%% @author Hunter Morris <hunter.morris@smarkets.com>
%% @author Filippo Pacini <filippo.pacini@gmail.com>

%% @doc ewgi stream file application
-module(ewgi_stream_file).

-export([serve_file/2]).

serve_file(Ctx, File) ->
    Mime = guess_mime(File),
    case file:open(File, [raw, binary]) of
        {ok, IoDevice} ->
            %% Set ChunkSize to an optimal value
            ChunkSize = 1024,
            Stream = iodevice_stream(IoDevice, ChunkSize),
            ewgi_api:response_status(
              {200, "OK"}, 
              ewgi_api:response_headers(
                [{"Content-type", Mime}], 
                ewgi_api:response_message_body(Stream, Ctx)
               )
             );
        _ ->
            %% Respond with 404...
            ewgi_api:response_message_body(
              "404 NOT FOUND", 
              ewgi_api:response_status({404, "NOT FOUND"}, Ctx)
             )
    end.
 
iodevice_stream(IoDevice, ChunkSize) ->
    fun() ->
            case file:read(IoDevice, ChunkSize) of
                eof ->
                    file:close(IoDevice),
                    {};
                {ok, Data} ->
                    {Data, iodevice_stream(IoDevice, ChunkSize)}
            end
    end.


%% @spec guess_mime(string()) -> string()
%% @doc  Guess the mime type of a file by the extension of its filename.
guess_mime(File) -> 
    %% Taken from webmachine.
    case filename:extension(File) of
	".html" ->
	    "text/html";
	".xhtml" ->
	    "application/xhtml+xml";
	".xml" ->
	    "application/xml";
	".css" ->
	    "text/css";
	".js" ->
	    "application/x-javascript";
	".jpg" ->
	    "image/jpeg";
	".jpeg" ->
	    "image/jpeg";
	".gif" ->
	    "image/gif";
	".png" ->
	    "image/png";
	".ico" ->
	    "image/x-icon";
	".swf" ->
	    "application/x-shockwave-flash";
	".zip" ->
	    "application/zip";
	".bz2" ->
	    "application/x-bzip2";
	".gz" ->
	    "application/x-gzip";
	".tar" ->
	    "application/x-tar";
	".tgz" ->
	    "application/x-gzip";
        ".htc" ->
            "text/x-component";
        _ ->
	    "text/plain"
    end.
