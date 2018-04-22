-module(demo).
-export([listOp/2]).

listOp(L, Type) ->
	if
		Type == sum ->
			lists:sum(L);

		Type == prod ->
			lists:foldl(fun (X, P) -> X * P end, 1, L);

		Type == squareEach ->
			lists:map(fun (X) -> X * X end, L);

		true ->
			"Operation not supported"
	end.

