-module(mb_cp949).
-export([encodings/0, codecs_config/0, init/0, decode/1, decode/2, encode/1, encode/2]).

encodings() ->
	[cp949, cp_949].

codecs_config() ->
	{mb, mb_codecs_dbcs_cp949, "CP949.CONF", "CP949.BIN"}.

init() ->
	mb_dbcs:init(?MODULE).

encode(Unicode) when is_list(Unicode) ->
    mb_dbcs:encode(?MODULE, Unicode).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	mb_dbcs:encode(?MODULE, Unicode, Options).

decode(Binary) when is_binary(Binary) ->
    mb_dbcs:decode(?MODULE, Binary).

decode(Binary, Options) when is_binary(Binary), is_list(Options) ->
	mb_dbcs:decode(?MODULE, Binary, Options).
	