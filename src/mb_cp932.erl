-module(mb_cp932).
-export([config/0, init/0, decode/1, decode/2, encode/1, encode/2]).

config() ->
	{mb, mb_codecs_dbcs_cp932, "CP932.CONF", "CP932.BIN"}.

init() ->
	mb_dbcs:init(?MODULE).

encode(Unicode) when is_list(Unicode) ->
    mb_dbcs:encode(?MODULE, Unicode).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	mb_dbcs:encode(?MODULE, Unicode, Options).

decode(Binary) when is_bitstring(Binary) ->
    mb_dbcs:decode(?MODULE, Binary).

decode(Binary, Options) when is_bitstring(Binary), is_list(Options) ->
	mb_dbcs:decode(?MODULE, Binary, Options).
	