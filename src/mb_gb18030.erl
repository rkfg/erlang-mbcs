-module(mb_gb18030).
-export([encodings/0, init/0, decode/1, decode/2, encode/1, encode/2]).

encodings() ->
	[gb18030].

init() ->
	ok.

encode(Unicode) when is_list(Unicode) ->
    {error, not_implement}.

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	{error, not_implement}.

decode(Binary) when is_binary(Binary) ->
    {error, not_implement}.

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	{error, not_implement}.
	