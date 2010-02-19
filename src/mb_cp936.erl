-module(mb_cp936).
-export([init/0, decode/1, decode/2, encode/1, encode/2]).
%-compile(export_all).

-define(PROCESS_DICT_ATOM, mb_codecs_dbcs_cp936).
-define(TXTNAME, "CP936.CONF").
-define(BINNAME, "CP936.BIN").

-include("mb_dbcs.erl").
