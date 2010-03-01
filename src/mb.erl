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
-define(CODECS, mb_codecs).
-define(ENCODE_OPTIONS_DEFAULT, [{output, binary}, {error, strict}, {error_replace_char, $?}, {bom, false}]).
-define(DECODE_OPTIONS_DEFAULT, [{output, binary}, {error, strict}, {error_replace_char, 16#FFFD}]).

%%---------------------------------------------------------------------------

-type unicode()  :: [non_neg_integer()].
-type encoding() :: 'cp874' | 'iso_8859_11' | 'cp1250' | 'cp1251' | 'cp1252' | 'cp1253' | 'cp1254' | 'cp1255' | 'cp1256' | 'cp1257' | 'cp1258' | 'cp932' | 'cp936' | 'gbk' | 'cp949' | 'cp950' | 'big5' | 'utf8' | 'utf16' | 'utf16le' | 'utf16be' | 'utf32' | 'utf32le' | 'utf32be'.
-type option()   :: 'list' | 'binary' | 'ignore' | 'strict' | 'replace' | {replace, non_neg_integer()} | 'bom' | {bom, true} | {bom, false}.
-type options()  :: [option()].

%%---------------------------------------------------------------------------

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
    CodecsDict = dict:from_list(
                    lists:append([[{Enc, Mod} || Enc <- Mod:encodings()] || Mod <- Modules])
                ),
    erlang:put(?CODECS, CodecsDict),
    ok.

%%---------------------------------------------------------------------------

%% @spec parse_options(Options, OptionsDefault) -> {ok, OptionDict} | {error, Reason}
%%
%% @doc Parse Options List to Option Dict, Return {ok, OptionDict} or {error, Reason}.

-spec parse_options(Options::options(), OptionsDefault::list()) -> {ok, dict()} | {error, tuple()}.

parse_options(Options, OptionsDefault) when is_list(Options), is_list(OptionsDefault) ->
    parse_options1(Options, dict:from_list(OptionsDefault)).

parse_options1([], OptionDict) ->
    {ok, OptionDict};
parse_options1([Option | OptionsTail], OptionDict) ->
    case Option of
        binary ->
            parse_options1(OptionsTail, dict:store(output, binary, OptionDict));
        list ->   
            parse_options1(OptionsTail, dict:store(output, list, OptionDict));
        ignore -> 
            parse_options1(OptionsTail, dict:store(error, ignore, OptionDict));
        strict -> 
            parse_options1(OptionsTail, dict:store(error, strict, OptionDict));
        replace -> 
            parse_options1(OptionsTail, dict:store(error, replace, OptionDict));
        {replace, Char} when is_integer(Char) -> 
            parse_options1(OptionsTail, dict:store(error_replace_char, Char, dict:store(error, replace, OptionDict)));
        bom -> 
            parse_options1(OptionsTail, dict:store(bom, true, OptionDict));
        {bom, true} -> 
            parse_options1(OptionsTail, dict:store(bom, true, OptionDict));
        {bom, false}->
            parse_options1(OptionsTail, dict:store(bom, false, OptionDict));
        UnknownOption ->
            {error, {cannot_encode, [{reason, unknown_option}, {option, UnknownOption}]}}
    end.

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
            case catch dict:fetch(Encoding, CodecsDict) of
                {'EXIT',{badarg, _}} ->
                    {error, {cannot_encode, [{reson, illegal_encoding}, {encoding, Encoding}]}};
                Mod ->
                    case parse_options(Options, ?ENCODE_OPTIONS_DEFAULT) of
                        {ok, OptionDict} ->
                            Mod:encode(Unicode, Encoding, OptionDict);
                        {error, Reason} ->
                            {error, Reason}
                    end 
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
            {error, {cannot_encode, [{reson, illegal_process_dict}, {process_dict, ?CODECS}, {detail, "maybe you should call mb:init() first"}]}};
        CodecsDict ->
            case catch dict:fetch(Encoding, CodecsDict) of
                {'EXIT',{badarg, _}} ->
                    {error, {cannot_encode, [{reson, illegal_encoding}, {encoding, Encoding}]}};
                Mod ->
                    case parse_options(Options, ?DECODE_OPTIONS_DEFAULT) of
                        {ok, OptionDict} ->
                            Mod:decode(Binary, Encoding, OptionDict);
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.
