-module(testTwo).
-export([testTwo/2]).

testTwo(A,B) ->
	if
		A < B ->
			io:fwrite("A < B~n");
		A == B ->
			io:fwrite("A == B~n");
		true ->
			io:fwrite("A > B~n")
	end.
