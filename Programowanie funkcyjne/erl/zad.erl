-module(zad).
-export([rownanieKwadratowe/3]).

rownanieKwadratowe(A,B,C) ->
    Delta = B*B-4*A*C,
    if Delta < 0 ->
        brakRozwiazan;
    true ->
        if Delta == 0 ->
            B*B/2*A;
        true ->
            {(B*B - math:sqrt(Delta))/2*A,(B*B + math:sqrt(Delta))/2*A}
        end
    end.