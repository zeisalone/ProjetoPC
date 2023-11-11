-module(jogofinal).
-export([start_match/3]).
-import(math,[cos/1, sin/1, pi/0, sqrt/1, pow/2, acos/1]).
-import(timer,[minutes/1,seconds/1,send_after/3,sleep/1]).
-import(rand, [uniform/1]).

consumo()->
    receive
        { {User,Posx,Posy}, Items, Match}->
            Fun = fun(_,{_,PosxI,PosyI,Raio}) -> colide({Posx,Posy,30},{PosxI,PosyI,Raio}) end,
            Res = maps:map(Fun,Items),
            Items_List = true_only(maps:to_list(Res)),
            case (Items_List /= []) of
                true ->
                    Match ! {consumiu,User,Items_List,self()};

                false-> ok
            end,
        consumo()
    end.

true_only( [{Item,true}|T] )->[Item | true_only( T )];
true_only( [{_,false}|T]   )-> true_only(T);
true_only( [] )->[].

items(Counter)->
    receive
        {item,Match}->
            
            Item = rand:uniform(3),
            if
                Item =:= 1 -> Mesg = {verde,  Counter,  self()};
                Item =:= 2 -> Mesg = {vermelho, Counter, self()};
                Item =:= 3 -> Mesg = {azul,  Counter,   self()}
            end,
        timer:send_after(timer:seconds(30),Match,Mesg),
        items(Counter+1)
    end.

field()->
    receive
    {  [{UserA,{PosxA,PosyA,_,_,_,_,_,_}},{UserB,{PosxB,PosyB,_,_,_,_,_,_}}] ,    Match}->
        
        case (PosxA > 700 orelse PosyA > 700) orelse ( (PosxA < 0 orelse PosyA < 0) ) of
            true-> Match!{over,UserB,UserA,self()};
            false-> case (PosxB > 700 orelse PosyB > 700) orelse (PosxB < 0 orelse PosyB < 0) of
                    true->Match!{over,UserA,UserB,self()};
                    false -> ok
                    end
        end,
        field()
    end.

pontos()->
    receive
       { [{UserA,{PosxA,PosyA,_,_,_,AnguloA,_,_}},{UserB,{PosxB,PosyB,_,_,_,AnguloB,_,_}}] ,    Match}-> 
        Jogador = colideAtras({UserA,PosxA,PosyA,direcao(AnguloA)}, {UserB,PosxB,PosyB,direcao(AnguloB)}),
        case( Jogador == nao) of
            false->Match ! {pontos,Jogador,self()};
            true -> ok
        end,
        pontos()
    end.

colideAtras({UserA, PosxA, PosyA, DirecaoA}, {UserB, PosxB, PosyB, DirecaoB}) ->
    case colide({PosxA, PosyA, 30}, {PosxB, PosyB, 30}) of    
        false -> nao;
        
        true -> 
            VetorCentros = vector({PosxA, PosyA}, {PosxB, PosyB}),
            
            AnguloA = math:acos(dot_product(DirecaoA, VetorCentros) / (norme(DirecaoA) * norme(VetorCentros))),
            AnguloB = math:acos(dot_product(DirecaoB, VetorCentros) / (norme(DirecaoB) * norme(VetorCentros))),

            NewAnguloA = normalize_angle(AnguloA),
            NewAnguloB = normalize_angle(AnguloB),

            case NewAnguloA > (math:pi() / 2) of
                true -> {UserB, UserA};
                false ->
                    case NewAnguloB < (math:pi() / 2) of
                        true -> {UserA, UserB};
                        false -> nao
                    end
            end
    end.

normalize_angle(Angle) ->
    case Angle > (math:pi() * 2) of
        true  -> Angle - (trunc(Angle / (math:pi() * 2)) * (math:pi() * 2));
        false -> Angle
    end.

dot_product({PosxA,PosyA},{PosxB,PosyB})-> (PosxA * PosxB) + (PosyA * PosyB).

norme({Posx,Posy})-> math:sqrt( math:pow(Posx,2) + math:pow(Posy,2) ).

vector({PosxA,PosyA},{PosxB,PosyB})->{PosxB-PosxA, PosyB-PosyA}.

colide({PosxA,PosyA,RaioA},{PosxB,PosyB,RaioB})-> distancia({PosxA,PosyA},{PosxB,PosyB}) =< (RaioA + RaioB).

distancia({PosxA,PosyA},{PosxB,PosyB})-> math:sqrt( math:pow((PosxB - PosxA),2)  + math:pow((PosyB - PosyA),2) ) .

timerAzul()->
    receive
        {item_azul_cooldown,User,Match}->
            timer:send_after(timer:seconds(1), Match, {diminuiAceleracao, User, self()}),
            timer:send_after(timer:seconds(2), Match, {diminuiAceleracao, User, self()}),
            timer:send_after(timer:seconds(3), Match, {diminuiAceleracao, User, self()}),
            timer:send_after(timer:seconds(4), Match, {diminuiAceleracao, User, self()})
    end.

timerVerde()->
    receive
        {item_verde_cooldown, User, Match}->
            timer:send_after(500, Match, {diminuiVelocidade, User}),
            timer:send_after(1000, Match, {diminuiVelocidade, User}),
            timer:send_after(1500, Match, {diminuiVelocidade, User}),
            timer:send_after(2000, Match, {diminuiVelocidade, User})
    end.

timerMatch()->
    receive
        {conta_tempo, Match}->
            timer:send_after(timer:minutes(2), Match,tempo),
    timerMatch()
    end.

posIni()->
    PosxA = rand:uniform(100) + 50,
    PosyA = rand:uniform(100) + 50,
            
    PosxB = 600 - PosxA,
    PosyB = 600 - PosyA,

    {X_UserA, Y_UserA} = vector({PosxA, PosyA}, {300, 300}),
    AnguloA = abs(angulo_inicial(X_UserA, Y_UserA)),

    {X_UserB, Y_UserB} = vector({PosxB, PosyB}, {300, 300}),
    AnguloB = angulo_inicial(X_UserB, Y_UserB),

    {PosxA,PosyA,AnguloA,PosxB,PosyB,AnguloB}.

direcao(Angulo)->{ ( 30 * math:cos(Angulo) ), ( 30 * math:sin(Angulo) )}. %% assumimos que o raio do player Ã© 30

traslate(Posx,Posy,{VetorX,VetorY},VelocidadeLinear)->{Posx + (VetorX * VelocidadeLinear), Posy + (VetorY * VelocidadeLinear)}.

rotate(Vel,Angulo)   -> Angulo  + Vel.
rotateNeg(Vel,Angulo)-> Angulo  - Vel.

velocidade(AcelaracaoLinear,VelocidadeLinear)->   VelocidadeLinear  + AcelaracaoLinear.

velocidadeNeg(AcelaracaoLinear,VelocidadeLinear)-> VelocidadeLinear - AcelaracaoLinear.

angulo_inicial(X, Y) ->
    AnguloRad = math:atan2(Y, X),
    normalize_angle(AnguloRad).

start_match(GameState,ItemState,Timer)->
    
    Field       = spawn(fun()-> field() end),
    Pontos      = spawn(fun()-> pontos() end),
    Item        = spawn(fun()-> items(0) end),
    Consumo     = spawn(fun()-> consumo() end),
    TimeMaster  = spawn(fun()-> timerMatch() end),

    match(GameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster).

match(GameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster)->
    
    IniAcelaracaoLinear  = 0.01,
    IniVelocidadeAngular = 0.27,

    LimAcelaracaoLinear  = 0.01 * 2,
    LimVelocidadeAngular = 0.27 * 2,
    
    receive
        {start, UserA, UserB}->
           {PosxA,PosyA,AnguloA,PosxB,PosyB,AnguloB} = posIni(),
            
            VelocidadeLinear = 0.0,

            DirecaoA = direcao(AnguloA),
            DirecaoB = direcao(AnguloB),
            Ponto = 0,

            UserA ! {initial_setup, PosxA,PosyA,DirecaoA, PosxB,PosyB,DirecaoB},
            UserB ! {initial_setup, PosxB,PosyB,DirecaoB, PosxA,PosyA,DirecaoA},
            TimeMaster ! {conta_tempo, self()},
            Item ! {item, self()},

            Moving = false,

            Jogador1 = {PosxA,PosyA, IniVelocidadeAngular, IniAcelaracaoLinear, VelocidadeLinear, AnguloA, Moving, Ponto},
            Jogador2 = {PosxB,PosyB, IniVelocidadeAngular, IniAcelaracaoLinear, VelocidadeLinear, AnguloB, Moving, Ponto},

            NewGameState = #{UserA => Jogador1, UserB =>Jogador2},
                        
            match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo, TimeMaster);
        
        {move,User} ->

           {Posx,Posy,VelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo, Moving, Ponto} = maps:get(User,GameState),
           
           {New_Posx, New_Posy} = traslate(Posx, Posy, direcao(Angulo),VelocidadeLinear ),
           
           case (Moving)of 
                true -> NewVelocidadelinear = velocidade(AcelaracaoLinear,VelocidadeLinear);
                false->
                    case (VelocidadeLinear > 0) of
                        true -> NewVelocidadelinear = velocidadeNeg(AcelaracaoLinear,VelocidadeLinear);
                        false-> NewVelocidadelinear = 0.0
                    end           
            end,
           NewGameState = maps:update(User,{New_Posx,New_Posy,VelocidadeAngular,AcelaracaoLinear,NewVelocidadelinear,Angulo, Moving, Ponto},GameState),

           [User1, User2] = maps:keys(GameState),

            case (User =:= User1) of
                
                true->
                    User1   ! {new_pos_player, New_Posx, New_Posy},
                    User2   ! {new_pos_enemy, New_Posx, New_Posy};

                false ->
                    User2   ! {new_pos_player, New_Posx, New_Posy},
                    User1   ! {new_pos_enemy, New_Posx, New_Posy}
            end,

           Field   ! {maps:to_list(GameState),self()},
           Pontos  ! {maps:to_list(GameState),self()},
           Consumo ! { {User,New_Posx, New_Posy}, ItemState, self() },

           match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
        
        {stopping_move,User} ->
            {Posx,Posy,VelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo,_, Ponto} = maps:get(User,GameState), 
            
            NewGameState = maps:update(User,{Posx, Posy,VelocidadeAngular,AcelaracaoLinear,VelocidadeLinear,Angulo,false,Ponto},GameState),
            
            match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
        
        {start_move,User} ->
            
           {Posx,Posy,VelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo,_, Ponto} = maps:get(User,GameState), 
            
            NewGameState = maps:update(User,{Posx, Posy,VelocidadeAngular,AcelaracaoLinear,VelocidadeLinear,Angulo,true,Ponto},GameState),
            
            match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
        
        {spin_right,User}->

            {Posx, Posy, VelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto} = maps:get(User,GameState),
            
            New_Angulo = rotate(VelocidadeAngular,Angulo),
            NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular,AcelaracaoLinear, VelocidadeLinear,New_Angulo,Moving,Ponto},GameState),
            
            [User1, User2] = maps:keys(GameState),

            case (User =:= User1) of
                
                true->
                    User1 ! {new_direction_player, direcao(New_Angulo)},
                    User2 ! {new_direction_enemy, direcao(New_Angulo)};
                
                false->
                    User2 ! {new_direction_player, direcao(New_Angulo)},
                    User1 ! {new_direction_enemy, direcao(New_Angulo)}
            end,
            match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
        
        {spin_left,User}->
            {Posx,Posy,VelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto} = maps:get(User,GameState),

            New_Angulo = rotateNeg(VelocidadeAngular,Angulo),
            NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular,AcelaracaoLinear, VelocidadeLinear,New_Angulo,Moving,Ponto},GameState),
            
            [User1, User2] = maps:keys(GameState),

            case (User =:= User1) of
                
                true->
                    User1 ! {new_direction_player, direcao(New_Angulo)},
                    User2 ! {new_direction_enemy, direcao(New_Angulo)};
                
                false->
                    User2 ! {new_direction_player, direcao(New_Angulo)},
                    User1 ! {new_direction_enemy, direcao(New_Angulo)}

            end,
            match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);

        {over,WinnerUser,LoserUser,Field}->
            WinnerUser ! winner,
            LoserUser  ! loser;
        
        {vermelho, Counter, Item} ->
            PosxI = rand:uniform(500) + 50,
            PosyI = rand:uniform(500) + 50,
            Raio = 10,

            [User1, User2] = maps:keys(GameState),
            
            User1 ! {vermelho, PosxI,PosyI,Raio},
            User2 ! {vermelho, PosxI,PosyI,Raio},
            Item ! {item, self()},

            NewItemState =  maps:put(Counter, {vermelho,PosxI,PosyI,Raio}, ItemState),
            match(GameState, NewItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
        
        {verde,Counter, Item} ->
            PosxI = rand:uniform(500) + 50,
            PosyI = rand:uniform(500) + 50,
            Raio = 10,

            [User1, User2] = maps:keys(GameState),
            
            User1 ! {verde, PosxI,PosyI,Raio},
            User2 ! {verde, PosxI,PosyI,Raio},
            Item ! {item, self()},

            NewItemState =  maps:put(Counter, {verde,PosxI,PosyI,Raio}, ItemState),
            match(GameState,NewItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);

        {azul, Counter, Item}->
            PosxI = rand:uniform(500) + 50,
            PosyI = rand:uniform(500) + 50,
            Raio = 10,

            [User1, User2] = maps:keys(GameState),
            
            User1 ! {azul, PosxI,PosyI,Raio},
            User2 ! {azul, PosxI,PosyI,Raio},
            Item ! {item, self()},

           NewItemState =  maps:put(Counter, {azul,PosxI,PosyI,Raio}, ItemState),
           match(GameState, NewItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
        

        {consumiu,User,Items,Consumo}->
            Item_Pid = lists:nth(1, Items),

            NewItemState = lists:foldl(fun(Key, Acc) -> maps:remove(Key, Acc) end, ItemState, Items),

            {Cor,PosxI,PosyI,_} = maps:get( Item_Pid,ItemState),
            
            {Posx,Posy, VelocidadeAngular,AcelaracaoLinear, VelocidadeLinear, Angulo, Moving,Ponto} = maps:get(User,GameState),
            case (Cor) of

                azul->
                    TimeAzul  = spawn(fun()-> timerAzul() end),        
                    
                    [User1, User2] = maps:keys(GameState),
                    
                    User1 ! {apaga, PosxI,PosyI},
                    User2 ! {apaga, PosxI,PosyI},    
                    
                    case (AcelaracaoLinear >= LimAcelaracaoLinear)of

                        true->
                            NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular, LimAcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto},GameState),
                            match(NewGameState, NewItemState, Timer,Field,Pontos,Item,Consumo,TimeMaster);

                        false ->
                            TimeAzul !  {item_azul_cooldown,User,self()},
                            NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular, AcelaracaoLinear + 0.01, VelocidadeLinear, Angulo,Moving,Ponto},GameState),
                            match(NewGameState, NewItemState, Timer,Field,Pontos,Item,Consumo,TimeMaster)
                    end;
                
                vermelho->
                    [User1, User2] = maps:keys(GameState),
                    
                    User1 ! {apaga, PosxI,PosyI},
                    User2 ! {apaga, PosxI,PosyI},

                    NewGameState = maps:update (User,{Posx,Posy, IniVelocidadeAngular, IniAcelaracaoLinear,IniAcelaracaoLinear, Angulo, Moving, Ponto},GameState),
                    match(NewGameState, NewItemState, Timer,Field,Pontos,Item,Consumo,TimeMaster);

                verde->
                    TimeVerde  = spawn(fun()-> timerVerde() end),

                    [User1, User2] = maps:keys(GameState),
                    
                    User1 ! {apaga, PosxI,PosyI},
                    User2 ! {apaga, PosxI,PosyI},

                    case (VelocidadeAngular >= LimVelocidadeAngular)of
                        
                        true->
                            NewGameState = maps:update(User,{Posx,Posy,LimVelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo, Moving, Ponto},GameState),
                            match(NewGameState, NewItemState, Timer,Field,Pontos,Item,Consumo,TimeMaster);
                        
                        false->
                            TimeVerde !  {item_verde_cooldown, User, self()},
                            NewGameState = maps:update (User,{Posx,Posy, VelocidadeAngular + 0.27,AcelaracaoLinear,VelocidadeLinear, Angulo,Moving, Ponto},GameState),
                            match(NewGameState, NewItemState, Timer,Field,Pontos,Item,Consumo,TimeMaster)
                        end
                end;

        {diminuiAceleracao,User}->
            {Posx,Posy, VelocidadeAngular,AcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto} = maps:get(User,GameState),
            case (AcelaracaoLinear =< IniAcelaracaoLinear) of
                true -> 
                    NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular, IniAcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto},GameState),
                    match(NewGameState, ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
                false  ->
                    NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular, AcelaracaoLinear - 0.0025, VelocidadeLinear, Angulo,Moving,Ponto},GameState),
                    match(NewGameState,ItemState,Timer,Moving,Pontos,Item,Consumo,TimeMaster)
            end;
        
        {diminuiVelocidade, User}->
            {Posx,Posy, VelocidadeAngular,AcelaracaoLinear, VelocidadeLinear, Angulo, Moving, Ponto} = maps:get(User,GameState),
            case (VelocidadeAngular =< IniVelocidadeAngular) of
                true ->
                    NewGameState = maps:update(User,{Posx,Posy,IniVelocidadeAngular, AcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto},GameState),
                    match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
                false ->
                    NewGameState = maps:update(User,{Posx,Posy,VelocidadeAngular -0.0675, AcelaracaoLinear, VelocidadeLinear, Angulo,Moving,Ponto},GameState),
                    match(NewGameState,ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster)
            end;

        {pontos,{UserPoint,UserNoPoint},Pontos}->
            case(Timer)of
                    true->
                        {PosxA,PosyA,AnguloA,PosxB,PosyB,AnguloB} = posIni(),
                        
                        {_,_,_,_,_,_,_,PontoA} = maps:get(UserPoint,GameState),
                        {_,_,_,_,_,_,_,PontoB} = maps:get(UserNoPoint,GameState),

                        DirecaoA = direcao(AnguloA),
                        DirecaoB = direcao(AnguloB),

                        UserPoint   ! {point_player,PosxA,PosyA,DirecaoA, PosxB,PosyB,DirecaoB},
                        UserNoPoint ! {point_enemy,PosxB,PosyB,DirecaoB, PosxA,PosyA,DirecaoA},

                        Jogador1 = {PosxA,PosyA, IniVelocidadeAngular,IniAcelaracaoLinear, 0, AnguloA, false, PontoA+1},
                        Jogador2 = {PosxB,PosyB, IniVelocidadeAngular,IniAcelaracaoLinear, 0, AnguloB, false, PontoB},

                        NewGameState = #{UserPoint => Jogador1, UserNoPoint =>Jogador2},

                        match(NewGameState, ItemState,Timer,Field,Pontos,Item,Consumo,TimeMaster);
                    
                    false->
                        UserPoint ! winner,
                        UserNoPoint ! loser
                end;

        tempo->
               [{UserA,{_,_,_,_,_,_,_,PontoA}},{UserB,{_,_,_,_,_,_,_,PontoB}}]  = maps:to_list(GameState),
               case PontoA > PontoB of
                    true ->
                        UserA ! winner,
                        UserB ! loser;
                    false->
                        case PontoB > PontoA of
                            true -> 
                                UserB ! winner,
                                UserA ! loser;
                            false -> match(GameState, ItemState,false,Field,Pontos,Item,Consumo,TimeMaster)

                        end
                end
    end.
