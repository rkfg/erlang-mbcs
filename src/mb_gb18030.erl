-module(mb_gb18030).
-export([encodings/0, codecs_config/0, init/0, decode/2, encode/2]).

encodings() ->
	[gb18030].
	
codecs_config() ->
	{mb, mb_codecs_gb18030, "GB18030.CONF", "GB18030.BIN"}.

init() ->
	ok.
	
encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	{error, not_implement}.

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	{error, not_implement}.
	