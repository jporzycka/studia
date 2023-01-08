-module(z5).
-export([start_client/1,start_server/0,client/1,server/0]).

start_client(Arg) ->
    PID_C = spawn(z5, client,[Arg]),
    io:format("Start klienta, PID = ~p~n", [PID_C]).

start_server() ->
    PID_S = spawn(z5, server,[]),
    io:format("Start serwera, PID = ~p~n", [PID_S]),
    register(server, PID_S).

client(Arg) ->
    server ! Arg.

server() ->
    receive
        koniec -> io:format("koniec~n");
        Msg ->
            if
                is_number(Msg) ->
                    io:format("liczba~n"),
                    server();
                true ->
                    if
                        is_list(Msg) ->
                            io:format("lista~n"),
                            server();
                        true ->
                            io:format("zla wiadomosc")
                    end
            end
    end.
            