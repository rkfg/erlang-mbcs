%%
%% %CopyrightBegin%
%% 
%% Copyright Xiangyu LU(luxiangyu@msn.com) 2009-2010. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(mb).
-export([init/0, encode/2, encode/3, decode/2, decode/3]).
-include_lib("mb/include/mb.hrl").

-define(CODECS, mb_codecs).
-define(MB_ENCODE_OPTIONS_DEFAULT, [{return, binary}, {error, strict}, {error_replace_char, $?}, {bom, false}]).
-define(MB_DECODE_OPTIONS_DEFAULT, [{return, binary}, {error, strict}, {error_replace_char, 16#FFFD}]).

%% @spec init() -> ok
%%
%% @doc Load all codecs to process dict memory, Return ok.

-spec init() -> ok.

init() ->
    Modules = [
                mb_mbcs, 
                mb_unicode,
                mb_gb18030
              ],
    lists:foreach(fun(Mod) ->
                ok = Mod:init()
            end,
            Modules),
    CodecsDict = lists:foldl(fun(Mod, Dict) ->
                                lists:foldl(fun(Enc, DictAcc) ->
                                                dict:store(Enc, Mod, DictAcc)
                                            end,
                                            Dict,
                                            Mod:encodings())
                            end,
                            dict:new(),
                            Modules),
    erlang:put(?CODECS, CodecsDict),
    ok.

%%---------------------------------------------------------------------------

%% @spec parse_options(Options, OptionsDefault) -> {ok, MBOptions} | {error, Reason}
%%
%% @doc Parse Options List to Option Dict, Return {ok, MBOptions} or {error, Reason}.

-spec parse_options(Options::options(), OptionsDefault::list()) -> {ok, #mb_options{}} | {error, tuple()}.

parse_options(Options, OptionsDefault) when is_list(Options), is_list(OptionsDefault) ->
    parse_options1(OptionsDefault ++ Options, #mb_options{}).

parse_options1([], MBOptions=#mb_options{}) ->
    {ok, MBOptions};
parse_options1([{return, binary} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{return=binary});
parse_options1([{return, list} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{return=list});
parse_options1([{error, ignore} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{error=ignore});
parse_options1([{error, strict} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{error=strict});
parse_options1([{error, replace} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{error=replace});
parse_options1([{replace, Char} | Tail], MBOptions=#mb_options{}) when is_integer(Char)->
    parse_options1(Tail, MBOptions#mb_options{error=replace, error_replace_char=Char});
parse_options1([{error_replace_char, Char} | Tail], MBOptions=#mb_options{}) when is_integer(Char)->
    parse_options1(Tail, MBOptions#mb_options{error_replace_char=Char});
parse_options1([{bom, true} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{bom=true});
parse_options1([{bom, false} | Tail], MBOptions=#mb_options{}) ->
    parse_options1(Tail, MBOptions#mb_options{bom=false});
parse_options1([UnknownOption | _], #mb_options{}) ->
    {error, {cannot_encode, [{reason, unknown_option}, {option, UnknownOption}]}}.

%% ---------------------------------------------------------------------

%% @spec encode(Unicode::unicode(), Encoding::encoding()) -> binary()
%%
%% @doc Equivalent to encode(Unicode, Encoding, []).
%%
%% @see encode/3

-spec encode(Unicode::unicode(), Encoding::encoding()) -> binary().

encode(Unicode, Encoding) when is_list(Unicode), is_atom(Encoding) ->
    encode(Unicode, Encoding, []).

%% @spec encode(Unicode::unicode(), Encoding::encoding(), Options::options()) -> binary() | string()
%%
%% @doc Return a Binary or String.
%%
%% @see encode/2

-spec encode(Unicode::unicode(), Encoding::encoding(), Options::options()) -> binary() | string().

encode(Unicode, Encoding, Options) when is_list(Unicode), is_atom(Encoding), is_list(Options) ->
    case erlang:get(?CODECS) of
        undefined ->
            {error, {cannot_encode, [{reson, illegal_process_dict}, {process_dict, ?CODECS}, {detail, "maybe you should call mb:init() first"}]}};
        CodecsDict ->
            case dict:find(Encoding, CodecsDict) of
                {ok, Mod} ->
                    case parse_options(Options, ?MB_ENCODE_OPTIONS_DEFAULT) of
                        {ok, MBOptions} ->
                            Mod:encode(Unicode, Encoding, MBOptions);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                error ->
                    {error, {cannot_encode, [{reson, illegal_encoding}, {encoding, Encoding}]}}
            end
    end.

%% ---------------------------------------------------------------------

%% @spec decode(StringOrBinary::string()|binary(), Encoding::encoding()) -> unicode()
%%
%% @doc Equivalent to decode(StringOrBinary, Encoding, []).
%%
%% @see decode/3

-spec decode(StringOrBinary::string()|binary(), Encoding::encoding()) -> unicode().

decode(String, Encoding) when is_list(String), is_atom(Encoding) ->
    decode(String, Encoding, []);
decode(Binary, Encoding) when is_binary(Binary), is_atom(Encoding) ->
    decode(Binary, Encoding, []).

%% @spec decode(StringOrBinary::string()|binary(), Encoding::encoding(), Options::options()) -> unicode()
%%
%% @doc Return a Unicode.
%%
%% @see decode/2

-spec decode(StringOrBinary::string()|binary(), Encoding::encoding(), Options::options()) -> unicode().

decode(String, Encoding, Options) when is_list(String), is_atom(Encoding), is_list(Options) ->
    case catch list_to_binary(String) of
        {'EXIT',{badarg, _}} ->
            {error, {cannot_decode, [{reson, illegal_list}, {list, String}]}};
        Binary ->
            decode(Binary, Encoding, Options)
    end;
decode(Binary, Encoding, Options) when is_binary(Binary), is_atom(Encoding), is_list(Options) ->
    case erlang:get(?CODECS) of
        undefined ->
            {error, {cannot_decode, [{reson, illegal_process_dict}, {process_dict, ?CODECS}, {detail, "maybe you should call mb:init() first"}]}};
        CodecsDict ->
            case dict:find(Encoding, CodecsDict) of
                {ok, Mod} ->
                    case parse_options(Options, ?MB_DECODE_OPTIONS_DEFAULT) of
                        {ok, MBOptions} ->
                            Mod:decode(Binary, Encoding, MBOptions);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                error ->
                    {error, {cannot_decode, [{reson, illegal_encoding}, {encoding, Encoding}]}}
            end
    end.
