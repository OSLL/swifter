%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for test.

-module(test_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

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
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
			"v1.0" ->
                       		Token = auth(Req),
                        	Req:respond({204, [{"X-Auth-Token", Token}], []});
			"v1.0/" ++ Rest ->
				[Account, Container, Object] = string:tokens (Rest, "/"),
				Auth_t = Req:get_header_value("X-Auth-Token"),
				Sum = md5_hex(Account ++ Container ++ Object),
				Req:ok({200, [{"ETag", Sum},{"X-Auth-Token", Auth_t}],
                                     "Account: " ++ Account ++ "\n" ++
					"Container:" ++ Container ++ "\n" ++
					"Object:" ++ Object ++ "\n"});
                    _ ->
			Req:serve_file(Path, DocRoot)
                end;
	    'PUT' ->
                case Path of
			"v1.0/" ++ Rest ->
				[Account, Container, Object] = string:tokens (Rest, "/"),
				Auth_t = Req:get_header_value("X-Auth-Token"),
				Sum = md5_hex(Account ++ Container ++ Object),
				Req:respond({201, [{"ETag", Sum},{"X-Auth-Token", Auth_t}],
                                     "Account: " ++ Account ++ "\n" ++
					"Container:" ++ Container ++ "\n" ++
					"Object:" ++ Object ++ "\n"});
                    _ ->
                        Req:not_found()
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%% md5 hex
md5_hex(S) ->
	Md5_bin =  erlang:md5(S),
	Md5_list = binary_to_list(Md5_bin),
	lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$a + (N-10).

%%
%% Auth
%%
auth(Req) -> 
	User = Req:get_header_value("X-Auth-User"),
	Key = Req:get_header_value("X-Auth-Key"),
	md5_hex(User ++ Key).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
