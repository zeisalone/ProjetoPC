-module(server).
-export([start/1, stop/0]).
-import(jogofinal, [start_match/3]).
-import(map, [to_list/1]).

start(Port) -> register ( ?MODULE, spawn(fun() -> server(Port) end)).

stop() -> ?MODULE ! stop.

server(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, [{packet, line}, {reuseaddr, true}]),
    AccountManager = spawn(fun() -> account_manager(#{}) end),
    MatchmakingManager = spawn(fun() -> matchmaking_manager(#{}) end),
    spawn(fun() -> acceptor(LSocket, AccountManager, MatchmakingManager) end),
    receive
        stop -> ok
    end.

acceptor(LSocket, AccountManager, MatchmakingManager) ->
    R = gen_tcp:accept(LSocket),
    case R of
        {ok, Socket} -> 
            spawn(fun() -> acceptor(LSocket, AccountManager, MatchmakingManager) end),
            handle_authentication(Socket, AccountManager, MatchmakingManager);
        {error,closed} -> 
            ok
    end.

get_player_level(Wins) ->
    (Wins div 2) + 1.

handle_authentication(Socket, AccountManager, MatchmakingManager) ->
    receive
        {tcp, Socket, Data} ->
            case Data of
                "login:" ++ Rest ->
                    Msg = string:strip(Rest, right, $\n),
                    [Username, Password] = string:tokens(Msg, ","),
                    AccountManager ! {request_login, self(), Username, Password},
                    handle_authentication(Socket, AccountManager, MatchmakingManager);
                "create_account:" ++ Rest ->
                    Msg = string:strip(Rest, right, $\n),
                    [Username, Password] = string:tokens(Msg, ","),
                    AccountManager ! {request_new_account, self(), Username, Password},
                    handle_authentication(Socket, AccountManager, MatchmakingManager)
            end;

        {login_access_granted, Username, Wins} -> 
            Msg = "login:ok," ++ Username ++ "\n",
            gen_tcp:send(Socket, Msg),
            handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins);

        login_access_denied ->
            gen_tcp:send(Socket, "login:denied\n"), %% username or password incorrect
            handle_authentication(Socket, AccountManager, MatchmakingManager);

        account_created ->
            gen_tcp:send(Socket, "registration:ok\n"), 
            handle_authentication(Socket, AccountManager, MatchmakingManager);

        account_not_created -> 
            gen_tcp:send(Socket, "registration:denied\n"), %% username already exists
            handle_authentication(Socket, AccountManager, MatchmakingManager)
    end.

handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins) ->
    receive
        {tcp, Socket, Data} ->
            case Data of
                "play:ok\r\n" ->
                    Level = get_player_level(Wins),
                    MatchmakingManager ! {find_match, Level, self()},
                    handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins);

                "leaderboard:request\r\n" ->
                    AccountManager ! {request_registered_users, self()},
                    handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins);

                "delete_account:" ++ Rest ->
                    Password = string:strip(Rest, right, $\n),
                    AccountManager ! {request_account_deletion, self(), Username, Password},
                    handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins);

                "leave:ok\r\n" ->
                    handle_authentication(Socket, AccountManager, MatchmakingManager);

                "logout:ok\r\n" ->
                    handle_authentication(Socket, AccountManager, MatchmakingManager);

                _ -> 
                    handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins)
            end;

        {match_found, Match} ->
            gen_tcp:send(Socket, "queue:match_found\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        account_deleted ->
            gen_tcp:send(Socket, "account_deletion:ok\n"),
            handle_authentication(Socket, AccountManager, MatchmakingManager);

        account_not_deleted ->
            gen_tcp:send(Socket, "account_deletion:denied\n"),
            handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins);

        {registeredUsers, RegisteredUsers} -> %% for leaderboard
            List = maps:to_list(RegisteredUsers),
            SortedList = lists:sort(fun({_, {_, Wins1}}, {_, {_, Wins2}}) -> Wins1 > Wins2 end, List),
            Top10 = lists:sublist(SortedList, 1, 10),
            send_leaderboard_data(Socket, Top10, 1),
            handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins)
    end.

send_leaderboard_data(Socket, [{Username, {_, Wins}} | Tail], N) -> 
    Message = "top" ++ integer_to_list(N) ++ ":" ++ Username ++ "," ++ integer_to_list(Wins) ++ "\n",
    gen_tcp:send(Socket, Message),
    send_leaderboard_data(Socket, Tail, N+1);

send_leaderboard_data(_, [], 11) ->
    ok;

send_leaderboard_data(Socket, [], N) ->
    Message = "top" ++ integer_to_list(N) ++ ":" ++ "None,0\n",
    gen_tcp:send(Socket, Message),
    send_leaderboard_data(Socket, [], N+1).

handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins) ->
    receive
        {tcp, Socket, Data} ->
            case Data of
                "input_move:" ++ Rest ->
                    Message = string:strip(Rest, right, $\n),
                    case Message of
                        "turn_right\r" ->
                            Match ! {spin_right, self()},
                            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

                        "turn_left\r" ->
                            Match ! {spin_left, self()},
                            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);
                        
                        "move\r" ->
                            Match ! {move, self()},
                            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

                        "start_move\r" ->
                            Match ! {start_move, self()},
                            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);
                        
                        "stopping_move\r" ->
                            Match ! {stopping_move, self()},
                            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins)
                    end
            end;

        {initial_setup, PosPlayerX, PosPlayerY, {DirectionPlayerX, DirectionPlayerY},
                        PosEnemyX , PosEnemyY , {DirectionEnemyX , DirectionEnemyY }} ->

            Message = integer_to_list(PosPlayerX) ++ "," ++ integer_to_list(PosPlayerY) ++ ","
                    ++ float_to_list(DirectionPlayerX) ++ "," ++ float_to_list(DirectionPlayerY) ++ ","
                    ++ integer_to_list(PosEnemyX) ++ "," ++ integer_to_list(PosEnemyY) ++ ","
                    ++ float_to_list(DirectionEnemyX) ++ "," ++ float_to_list(DirectionEnemyY) ++ ",",
            gen_tcp:send(Socket, "initial_setup:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {point_player, PosPlayerX, PosPlayerY, {DirectionPlayerX, DirectionPlayerY},
                        PosEnemyX , PosEnemyY , {DirectionEnemyX , DirectionEnemyY }} ->

            Message = integer_to_list(PosPlayerX) ++ "," ++ integer_to_list(PosPlayerY) ++ ","
                    ++ float_to_list(DirectionPlayerX) ++ "," ++ float_to_list(DirectionPlayerY) ++ ","
                    ++ integer_to_list(PosEnemyX) ++ "," ++ integer_to_list(PosEnemyY) ++ ","
                    ++ float_to_list(DirectionEnemyX) ++ "," ++ float_to_list(DirectionEnemyY) ++ ",",
            gen_tcp:send(Socket, "point_player:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {point_enemy, PosPlayerX, PosPlayerY, {DirectionPlayerX, DirectionPlayerY},
                        PosEnemyX , PosEnemyY , {DirectionEnemyX , DirectionEnemyY }} ->

            Message = integer_to_list(PosPlayerX) ++ "," ++ integer_to_list(PosPlayerY) ++ ","
                    ++ float_to_list(DirectionPlayerX) ++ "," ++ float_to_list(DirectionPlayerY) ++ ","
                    ++ integer_to_list(PosEnemyX) ++ "," ++ integer_to_list(PosEnemyY) ++ ","
                    ++ float_to_list(DirectionEnemyX) ++ "," ++ float_to_list(DirectionEnemyY) ++ ",",
            gen_tcp:send(Socket, "point_enemy:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {new_pos_player, PosX, PosY} ->
            Message = float_to_list(PosX) ++ "," ++ float_to_list(PosY),
            gen_tcp:send(Socket, "new_pos_player:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {new_pos_enemy, PosX, PosY} ->
            Message = float_to_list(PosX) ++ "," ++ float_to_list(PosY),
            gen_tcp:send(Socket, "new_pos_enemy:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {new_direction_player, {DirectionX, DirectionY}} ->
            Message = float_to_list(DirectionX) ++ "," ++ float_to_list(DirectionY),
            gen_tcp:send(Socket, "new_direction_player:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {new_direction_enemy, {DirectionX, DirectionY}} ->
            Message = float_to_list(DirectionX) ++ "," ++ float_to_list(DirectionY),
            gen_tcp:send(Socket, "new_direction_enemy:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {vermelho, PosX, PosY, Radius} ->
            Message = integer_to_list(PosX) ++ "," ++ integer_to_list(PosY) ++ "," ++ integer_to_list(Radius),
            gen_tcp:send(Socket, "new_item_red:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {verde, PosX, PosY, Radius} ->
            Message = integer_to_list(PosX) ++ "," ++ integer_to_list(PosY) ++ "," ++ integer_to_list(Radius),
            gen_tcp:send(Socket, "new_item_green:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {azul, PosX, PosY, Radius} ->
            Message = integer_to_list(PosX) ++ "," ++ integer_to_list(PosY) ++ "," ++ integer_to_list(Radius),
            gen_tcp:send(Socket, "new_item_blue:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        {apaga, PosX, PosY} ->
            Message = integer_to_list(PosX) ++ "," ++ integer_to_list(PosY),
            gen_tcp:send(Socket, "delete_item:" ++ Message ++ "\n"),
            handle_match(Socket, Match, AccountManager, MatchmakingManager, Username, Wins);

        winner ->
            AccountManager ! {request_user_wins_update, Username},
            gen_tcp:send(Socket, "match_ended:win\n"),
            handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins + 1);
        
        loser ->
            gen_tcp:send(Socket, "match_ended:loss\n"),
            handle_lobby(Socket, AccountManager, MatchmakingManager, Username, Wins)
    end.

account_manager(RegisteredUsers) ->
    receive
        {request_registered_users, From} ->
            From ! {registeredUsers, RegisteredUsers},
            account_manager(RegisteredUsers);

        {request_login, From, Username, Password} ->
            case login(RegisteredUsers, Username, Password) of
                {correct_password, Wins} ->
                    From ! {login_access_granted, Username, Wins},
                    account_manager(RegisteredUsers);
                
                incorrect_username_or_password ->
                    From ! login_access_denied,
                    account_manager(RegisteredUsers)
            end;

        {request_new_account, From, Username, Password} ->
            case create_account(RegisteredUsers, Username, Password) of
                
                {account_created, UpdatedRegisteredUsers} ->
                    From ! account_created,
                    account_manager(UpdatedRegisteredUsers);

                username_already_exists ->
                    From ! account_not_created,
                    account_manager(RegisteredUsers)
            end;

        {request_account_deletion, From, Username, Password} ->
            case delete_account(RegisteredUsers, Username, Password) of
                
                {correct_password, UpdatedRegisteredUsers} ->
                    From ! account_deleted,
                    account_manager(UpdatedRegisteredUsers);
                
                incorrect_password ->
                    From ! account_not_deleted,
                    account_manager(RegisteredUsers);
                
                username_not_found ->
                    %% It is not supposed to happen
                    From ! account_not_deleted,
                    account_manager(RegisteredUsers)
            end;

        {request_user_wins_update, Username} ->
            case update_user_wins(RegisteredUsers, Username) of
                
                {ok, UpdatedRegisteredUsers} ->
                    account_manager(UpdatedRegisteredUsers);
                
                username_not_found ->
                    %% It is not supposed to happen 
                    account_manager(RegisteredUsers)
            end
    end.

matchmaking_manager(QueuedPlayers)->
    receive
        {find_match, Level, User1Pid}-> 
            case maps:find(Level, QueuedPlayers) of
                
                {ok, User2Pid} -> %% There is a maximum of one player waiting per level
                    Match = spawn(fun()-> jogofinal:start_match(#{}, #{}, true) end),
                    
                    User1Pid ! {match_found, Match},
                    User2Pid ! {match_found, Match},
                    Match ! {start, User1Pid, User2Pid},
                    
                    matchmaking_manager (maps:remove(Level, QueuedPlayers));
                
                error -> 
                    matchmaking_manager(maps:put(Level, User1Pid, QueuedPlayers))
            end
    end.

login(RegisteredUsers, Username, InputPassword) ->
    case maps:find(Username, RegisteredUsers) of
        {ok, {RegisteredPassword, Wins}} ->
            case InputPassword == RegisteredPassword of
                true ->
                    {correct_password, Wins};
                false ->
                    incorrect_username_or_password
            end;
        error ->
            incorrect_username_or_password
    end.

create_account(RegisteredUsers, Username, Password) ->
    case maps:is_key(Username, RegisteredUsers) of
        true ->
            username_already_exists;
        false ->
            {account_created, maps:put(Username, {Password, 0}, RegisteredUsers)}
    end.

delete_account(RegisteredUsers, Username, InputPassword) ->
    case maps:find(Username, RegisteredUsers) of
        {ok, {RegisteredPassword, _}} ->
            case InputPassword == RegisteredPassword of
                true ->
                    {correct_password, maps:remove(Username, RegisteredUsers)};
                false ->
                    incorrect_password
            end;
        error ->
            username_not_found
    end.

update_user_wins(RegisteredUsers, Username) ->
    case maps:find(Username, RegisteredUsers) of
        {ok, {Password, Wins}} ->
            {ok, maps:update(Username, {Password, Wins + 1}, RegisteredUsers)};
        error ->
            username_not_found
    end.