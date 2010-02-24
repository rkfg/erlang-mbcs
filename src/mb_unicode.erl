-module(mb_unicode).
-export([init/1, decode/3, encode/3]).

init(Mod) when is_atom(Mod) ->
    ok.

encode(Mod, Unicode, Options) when is_atom(Mod), is_list(Unicode), is_list(Options) ->
	{mb, ?MODULE, Encoding, _, _} = Mod:codecs_config(),
    Binary = unicode:characters_to_binary(Unicode, unicode, Encoding),
	case lists:member(list, Options) of
		true ->
			erlang:binary_to_list(Binary);
		false ->
			Binary
	end.

decode(Mod, Binary, Options) when is_atom(Mod), is_binary(Binary), is_list(Options) ->
	{mb, ?MODULE, Encoding, _, _} = Mod:codecs_config(),
    unicode:characters_to_list(Binary, Encoding).
