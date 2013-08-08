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
    {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
    try
    case Req:get(method) of
	Method when Method =:= 'GET' ->
	case Path of
	    "v1.0" ->
		Token = auth(Req),
		Req:respond({204, [{"X-Auth-Token", Token}], []});
	    "v1.0/" ++ Rest ->
		try string:tokens (Rest, "/") of
		[Account, Container, Object] -> 
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account ++ Container ++ Object),
		    try riakc_pb_socket:get(Pid, list_to_binary(Account), list_to_binary(Container ++ "/" ++ Object)) of
		    {ok, Item} -> 
			Obj = binary_to_term(riakc_obj:get_value(Item)),
			Req:ok({200, [{"ETag", Sum},{"X-Auth-Token", Auth_t}], Obj})
		    catch
		      error:notfound ->
		    Req:respond({404, [{"Content-Type", "text/plain"}],
			    "not_found"})
		    end; 

		[Account, Container] ->
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account ++ Container),
		    Objects = list_objects(Pid, Account, Container),
		    {ok, Item} = riakc_pb_socket:get(Pid, list_to_binary(Account), list_to_binary(Container)),
		    Obj = binary_to_term(riakc_obj:get_value(Item)),
		    Req:ok({200, Obj, Objects});

		[Account] ->
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account),
		    Containers = list_containers(Pid, Account),
		    Req:ok({200, [{"ETag", Sum},{"X-Auth-Token", Auth_t}], Containers})

		catch
		_:_ -> Req:not_found()
		end;
	    _ ->
		Req:serve_file(Path, DocRoot)
	end;

	'PUT' ->
	case Path of
	    "v1.0/" ++ Rest ->
		try string:tokens (Rest, "/") of
		[Account, Container, Object] -> 
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account ++ Container ++ Object),
		    Data=Req:recv_body(),
		    Item = riakc_obj:new(list_to_binary(Account), list_to_binary(Container ++ "/" ++ Object), term_to_binary(Data)),
		    riakc_pb_socket:put(Pid, Item),
		    Req:respond({201, [{"ETag", Sum},{"X-Auth-Token", Auth_t}],"Object:" ++ Object ++ "\n"});

		[Account, Container] ->
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account ++ Container),
		    Headers = get_header_lists(Req:get(headers)),
		    Item = riakc_obj:new(list_to_binary(Account), list_to_binary(Container), term_to_binary(Headers)),
		    riakc_pb_socket:put(Pid, Item),
		    Req:respond({201, [{"ETag", Sum},{"X-Auth-Token", Auth_t}], "Account: " ++ Account ++ "\n" ++
			"Container:" ++ Container ++ "\n"})

		catch
		_:_ -> Req:not_found()
		end;
	    _ ->
		Req:not_found()
	end;

	'DELETE' ->
	case Path of
	    "v1.0/" ++ Rest ->
		try string:tokens (Rest, "/") of 
		[Account, Container, Object] -> 
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account ++ Container ++ Object),
		    try riakc_pb_socket:delete(Pid, list_to_binary(Account), list_to_binary(Container ++ "/" ++ Object)) of 
		    ok -> Req:respond({204, [{"ETag", Sum},{"X-Auth-Token", Auth_t}],"Object:" ++ Object ++ "\n"})
		    catch
		      _:_ ->
		    Req:respond(204, [], [])
		    end;

		[Account, Container] ->
		    Auth_t = Req:get_header_value("X-Auth-Token"),
		    Sum = md5_hex(Account ++ Container),
		    Obj = list_objects(Pid, Account, Container),
		    if Obj == [] -> 
			    riakc_pb_socket:delete(Pid, list_to_binary(Account), list_to_binary(Container)),
			    Req:respond({201, [{"ETag", Sum},{"X-Auth-Token", Auth_t}],
				"done" ++ "\n"});
			true ->
			    Req:respond({201, [{"ETag", Sum},{"X-Auth-Token", Auth_t}],
				"is not empty" ++ "\n"})
		    end

		catch
		_:_ -> Req:not_found()
		end;
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

%% Internal API204, [], []

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

list_objects(Pid, Account, Container) ->
    {ok, List} = riakc_pb_socket:list_keys(Pid, list_to_binary(Account)),
    F = fun(A) -> binary_to_list(A) end,
    Lst = lists:map(F, List),
    Filtered_list = tokens(Container, Lst),
    add_carriage_return(Filtered_list).
    
add_carriage_return(List) -> add_carriage_return(List, []).
add_carriage_return([], List) -> lists:reverse(List);
add_carriage_return([Hd|Tl], List) -> add_carriage_return(Tl, [Hd++"\n"|List]).

tokens(Container, List)->tokens(Container, List, []).
tokens(Container, [], List) -> List;
tokens(Container, [H|T], List) -> 
    case string:tokens(H, "/") of
    [Container|Object] -> tokens(Container, T, [Object|List]);
    _ -> tokens(Container, T, List)
    end.
    
list_containers(Pid, Account) ->
    {ok, List} = riakc_pb_socket:list_keys(Pid, list_to_binary(Account)),
    F = fun(A) -> binary_to_list(A) end,
    Lst = lists:map(F, List),
    Filtered_list = lists:usort(container_tokens(Lst)),
    add_carriage_return(Filtered_list).
  
container_tokens(List)->container_tokens(List, []).
container_tokens([], List) -> List;
container_tokens([H|T], List) ->
    case string:tokens(H, "/") of
    [Container|Object] -> container_tokens(T, [Container|List]);
    _ -> container_tokens(T, List)
    end.
    
get_header_lists(Headers) ->
    New_headers = mochiweb_headers:to_list(Headers),
    lists:nthtail(0, New_headers).