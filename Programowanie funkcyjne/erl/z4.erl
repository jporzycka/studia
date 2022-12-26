-module(z4).
-export([client/2,server/0,start/0]).

start() ->
    PID_S = spawn(z4, server,[]),
    PID_C = spawn(z4, client, [PID_S,[1,2,3,4]]),
    io:format("Start serwera, PID = ~p~n Start klienta, PID = ~p~n", [PID_S, PID_C]).

client(PID_S,IntList) ->
    [Int|Rest] = IntList,
    if 
        Rest == [] ->
            PID_S ! {self(),Int},
            receive 
                Msg ->
                    io:format("~p~n",[Msg])
            end;
        true ->
            PID_S ! {self(),Int},
            receive 
                Msg ->
                    io:format("~p~n",[Msg]),
                    client(PID_S,Rest)
            end
    end.

server() ->
    receive
        {PID, Int} ->
            PID ! Int*Int,
            server()
        after 300000000 ->
            io:format("serwer konczy dzialanie")
    end.
