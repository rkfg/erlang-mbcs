-module(mb_utf16le).
-export([config/0, init/0, decode/1, decode/2, encode/1, encode/2]).

config() ->
	{mb, {utf16,little}}.

init() ->
	mb_unicode:init(?MODULE).

encode(Unicode) when is_list(Unicode) ->
    mb_unicode:encode(?MODULE, Unicode).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	mb_unicode:encode(?MODULE, Unicode, Options).

decode(Binary) when is_binary(Binary) ->
    mb_unicode:decode(?MODULE, Binary).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	mb_unicode:decode(?MODULE, Binary, Options).
	