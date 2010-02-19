-module(mb_cp950).
-export([init/0, decode/1, decode/2, encode/1, encode/2]).

-define(PROCESS_DICT_ATOM, mb_codecs_dbcs_cp950).
-define(TXTNAME, "CP950.CONF").
-define(BINNAME, "CP950.BIN").

-include("mb_dbcs.erl").
