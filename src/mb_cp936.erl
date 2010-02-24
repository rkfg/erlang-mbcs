-module(mb_cp936).
-export([encodings/0, codecs_config/0, init/0, decode/2, encode/2]).

encodings() ->
	[cp936, cp_936, gbk].

codecs_config() ->
	{mb, mb_codecs_dbcs_cp936, "CP936.CONF", "CP936.BIN"}.
	
init() ->
	mb_dbcs:init(?MODULE).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	mb_dbcs:encode(?MODULE, Unicode, Options).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	mb_dbcs:decode(?MODULE, Binary, Options).
	