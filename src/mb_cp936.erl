-module(mb_cp936).
-export([encodings/0, codecs_config/0, init/0, decode/2, encode/2]).

encodings() ->
	[cp936, gbk].

codecs_config() ->
	{mb, mb_dbcs, mb_codecs_dbcs_cp936, "CP936.CONF", "CP936.BIN"}.
	
init() ->
	{mb, MbImpMod, _, _, _} = codecs_config(),
	ok = MbImpMod:init(?MODULE).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	{mb, MbImpMod, _, _, _} = codecs_config(),
	MbImpMod:encode(?MODULE, Unicode, Options).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	{mb, MbImpMod, _, _, _} = codecs_config(),
	MbImpMod:decode(?MODULE, Binary, Options).
	