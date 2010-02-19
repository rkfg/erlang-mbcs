-module(mb_utf8).
-export([init/0, decode/1, decode/2, encode/1, encode/2]).

init() ->
    ok.

encode(Unicode) when is_list(Unicode) ->
    encode(Unicode, [strict]).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
    unicode:characters_to_binary(Unicode, utf8).

decode(Binary) when is_bitstring(Binary) ->
    decode(Binary, [strict]).

decode(Binary, Options) when is_bitstring(Binary), is_list(Options) ->
    unicode:characters_to_list(Binary, utf8).
