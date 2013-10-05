-module(test_users).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("tables.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([list/0, get/1, register/2, change_password/2, auth/2]).
-export([create/0, find/1]).

list() -> gen_server:call(?MODULE, list).
register(Username, Password) -> gen_server:call(?MODULE, {register, Username, Password}).
get(Identifier) -> gen_server:call(?MODULE, {get_user, Identifier}).
change_password(Username, NewPassword) -> 
    gen_server:call(?MODULE, {change_pass, Username, NewPassword}).
auth(Username, Password) -> 
    Pid = self(),
    Auth = fun() -> User = find(Username),
		    true = salt(User#user.salt, Password) =:= User#user.password,
		    Pid ! User
	   end,
    AuthProc = spawn(Auth),
    receive
	User -> exit(AuthProc, thank_you),
		Id = crypto:rand_bytes(32),
		Token = salt(Id, Username),
		{User#user.username, Token}
    after 2000 -> 
	    false
    end.

handle_call(list, _From, State) -> 
    {reply, db:list(user, [#user.username]), State};
handle_call({get_user, Identifier}, _From, State) ->
    Res = case find(Identifier) of
	      false -> false;
	      User -> {User#user.username}
	  end,
    {reply, Res, State};
handle_call({register, Username, Password}, _From, State) -> 
    Res = case find(Username) of
	      false -> Salt = crypto:rand_bytes(32),
		       User = #user{username=Username, password=salt(Salt, Password), salt=Salt},
		       db:transaction(fun() -> mnesia:write(User) end),
		       {Username};
	      _ -> already_exists
	  end,
    {reply, Res, State};
handle_call({change_pass, Username, NewPassword}, _From, State) -> 
 	 {reply, update(Username, NewPassword), State}.

%%%%%%%%%%%%%%%%%%%% database related
salt(Salt, String) -> 
    common:binary_to_hex(erlsha2:sha256([Salt, String])).

create() -> 
    mnesia:create_table(user, [{type, ordered_set}, {disc_copies, [node()]}, {attributes, record_info(fields, user)}]).

find(Name) -> db:find(user, #user.username, Name).

%%%%%%%%%%%%%%%%%%%% generic actions
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%% gen_server handlers
init([]) -> {ok, []}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


update(Username, NewPassword) ->
    mnesia:transaction(fun() ->
        case mnesia:wread({user, Username}) of
            [User] ->
            	Salt = crypto:rand_bytes(32),
                Password = salt(Salt, NewPassword),
                User2 = User#user{password = Password, salt = Salt},
                mnesia:write(User2);
            _ ->
                mnesia:abort("No such person")
        end
    end).