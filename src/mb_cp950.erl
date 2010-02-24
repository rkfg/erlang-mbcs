-module(mb_cp950).
-export([encodings/0, codecs_config/0, init/0, decode/2, encode/2]).

encodings() ->
	[cp950, cp_950, big5].

codecs_config() ->
	{mb, mb_codecs_dbcs_cp950, "CP950.CONF", "CP950.BIN"}.

init() ->
	mb_dbcs:init(?MODULE).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	mb_dbcs:encode(?MODULE, Unicode, Options).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	mb_dbcs:decode(?MODULE, Binary, Options).
	