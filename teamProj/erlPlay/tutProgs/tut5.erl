-module(tut5).
-export([format_temps/1]).

%% Only this function is exported
format_temps([]) ->
	ok;
format_temps([City | Rest]) ->
	print_temp(convert_to_celsius(City)),
	format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) -> % No conversion needed
	{Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) -> % Do the conversion
	{Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
	io:format("~-15s ~.1f c~n", [Name, Temp+0.0]).

