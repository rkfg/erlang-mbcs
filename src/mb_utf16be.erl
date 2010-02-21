-module(mb_utf16be).
-export([encodings/0, codecs_config/0, init/0, decode/1, decode/2, encode/1, encode/2]).

encodings() ->
	[utf16be].

codecs_config() ->
	{mb, {utf16,big}}.

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
	