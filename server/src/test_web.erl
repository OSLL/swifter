%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for test.

-module(test_web).

-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).
%% External API

start(Options) ->
    start_auth(),
    erlmc:start(),
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
                User = Req:get_header_value("X-Auth-User"),
                Pass = Req:get_header_value("X-Auth-Key"),
                test_users:register(User, Pass),
                {Key,Token} = test_users:auth(User, Pass),
                erlmc:set(Token, list_to_binary(Key), 24*60*60),
                Req:respond({204, [{"X-Auth-Token", Token}], []});
            "v1.0/" ++ Rest ->
                try string:tokens (Rest, "/") of
                [Account, Container, Object] -> 
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            try riakc_pb_socket:get(Pid, list_to_binary(Account), list_to_binary(Container ++ "/" ++ Object)) of
                                {ok, Item} -> 
                                    Obj = binary_to_term(riakc_obj:get_value(Item)),
                                    Req:ok({200, [{"X-Auth-Token", Token}], Obj})
                            catch
                                error:notfound ->
                                    Req:respond({404, [{"Content-Type", "text/plain"}],"not_found"})
                            end
                    end;

                [Account, Container] ->
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            Objects = list_objects(Pid, Account, Container),
                            try riakc_pb_socket:get(Pid, list_to_binary(Account), list_to_binary(Container)) of
                                {ok, Item} -> 
                                    Obj = binary_to_term(riakc_obj:get_value(Item)),
                                    Req:ok({200, Obj, Objects});
                                {error,notfound} ->
                                    Req:ok({200, [{"X-Auth-Token", Token}], Objects})
                            catch
                                error:notfound ->
                                    Req:respond({404, [{"Content-Type", "text/plain"}],"not_found"})
                            end
                    end;

                [Account] ->
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            Containers = list_containers(Pid, Account),
                            Req:ok({200, [{"X-Auth-Token", Token}], Containers})
                    end
                    
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
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            Data=Req:recv_body(),
                            Item = riakc_obj:new(list_to_binary(Account), list_to_binary(Container ++ "/" ++ Object), term_to_binary(Data)),
                            riakc_pb_socket:put(Pid, Item),
                            Req:respond({201, [{"X-Auth-Token", Token}],"Object:" ++ Object ++ "\n"})
                    end;
                    
                [Account, Container] ->
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            Headers = get_header_lists(Req:get(headers)),
                            Item = riakc_obj:new(list_to_binary(Account), list_to_binary(Container), term_to_binary(Headers)),
                            riakc_pb_socket:put(Pid, Item),
                            Req:respond({201, [{"X-Auth-Token", Token}], "Account: " ++ Account ++ "\n" ++
                                "Container:" ++ Container ++ "\n"})
                    end
                    
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
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            try riakc_pb_socket:delete(Pid, list_to_binary(Account), list_to_binary(Container ++ "/" ++ Object)) of 
                                ok -> 
                                    Req:respond({204, [{"X-Auth-Token", Token}],"Object:" ++ Object ++ "\n"})
                            catch
                                _:_ ->
                                Req:respond(204, [], [])
                            end
                    end;
                    
                [Account, Container] ->
                    Token = Req:get_header_value("X-Auth-Token"),
                    case check_auth(Token, Account) of
                        false ->
                            Req:respond({204, [{"Content-Type", "text/plain"}],"error AUTH"});
                        true ->
                            Obj = list_objects(Pid, Account, Container),
                            io:format("Obj: ~w~n", [Obj]),
                            if Obj == [] ; Obj == [[10]] -> 
                                    riakc_pb_socket:delete(Pid, list_to_binary(Account), list_to_binary(Container)),
                                    Req:respond({201, [{"X-Auth-Token", Token}],
                                        "done" ++ "\n"});
                                true ->
                                    Req:respond({201, [{"X-Auth-Token", Token}],"is not empty" ++ "\n"})
                            end
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

start_auth() ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    test_users:start(),
    test_users:create().

check_auth(Token, Account) ->
    if
        Token == undefined ->
            false;
        true ->
            Key = binary_to_list(erlmc:get(Token)),
            if
                Account =/= Key ->
                    false;
                true ->
                    true
            end
    end.
