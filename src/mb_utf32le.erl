-module(mb_utf32le).
-export([encodings/0, codecs_config/0, init/0, decode/2, encode/2]).

encodings() ->
	[utf32le].

codecs_config() ->
	{mb, {utf32,little}}.

init() ->
	mb_unicode:init(?MODULE).
encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	mb_unicode:encode(?MODULE, Unicode, Options).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	mb_unicode:decode(?MODULE, Binary, Options).
	