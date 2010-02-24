-module(mb_cp874).
-export([encodings/0, codecs_config/0, init/0, decode/2, encode/2]).

encodings() ->
	[cp874, cp_874, iso8859_11, iso_8859_11].

codecs_config() ->
	{mb, mb_sbcs, mb_codecs_dbcs_cp874, "CP874.CONF", "CP874.BIN"}.

init() ->
	{mb, MbImpMod, _, _, _} = codecs_config(),
	MbImpMod:init(?MODULE).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	{mb, MbImpMod, _, _, _} = codecs_config(),
	MbImpMod:encode(?MODULE, Unicode, Options).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	{mb, MbImpMod, _, _, _} = codecs_config(),
	MbImpMod:decode(?MODULE, Binary, Options).
