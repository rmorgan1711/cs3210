-module(fib).
-export([fib/1, test/1]).

fib(N) -> fibPass(N, 0, 1).

fibPass(0, Result, _Next) -> Result;

fibPass(Iter, Result, Next) when Iter > 0 ->
	fibPass(Iter-1, Next, Result + Next).

test(Fibnum) ->
	Time = element(1, timer:tc(fib,fib,[Fibnum])) * 10.0e-6,
	io:fwrite("~w took ~.5f seconds.~n", [Fibnum, Time]).

