-module(loop).
-export([while/1]).

while([]) -> 
	ok;

while([H|T]) ->
	io:fwrite("~w~n", [H]),
	while(T).
