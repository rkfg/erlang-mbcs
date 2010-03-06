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
-define(MB_ENCODE_OPTIONS_DEFAULT, [{return, binary}, 
                                    {error, strict}, 
                                    {error_replace_char, $?}, 
                                    {bom, false}]).
-define(MB_DECODE_OPTIONS_DEFAULT, [{return, binary}, 
                                    {error, strict}, 
                                    {error_replace_char, 16#FFFD}]).

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
                    lists:append(
                        [
                            [{Enc, Mod} || Enc <- Mod:encodings()] 
                                || Mod <- Modules
                        ])
                    ),
    erlang:put(?CODECS, CodecsDict),
    ok.

%%---------------------------------------------------------------------------

%% @spec parse_options(Options, Default) -> {ok, Profile} | {error, Reason}
%%
%% @doc Parse Options List to Option Dict,
%%      Return {ok, Profile} | {error, Reason}.

-spec parse_options(options(), options()) -> {ok, #mb_profile{}} | {error, tuple()}.

parse_options(Options, OptionsDefault) 
  when is_list(Options), is_list(OptionsDefault) ->
    parse_options1(OptionsDefault ++ Options, #mb_profile{}).

parse_options1([], Profile=#mb_profile{}) ->
    {ok, Profile};
parse_options1([{return, binary} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{return=binary});
parse_options1([{return, list} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{return=list});
parse_options1([{error, ignore} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{error=ignore});
parse_options1([{error, strict} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{error=strict});
parse_options1([{error, replace} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{error=replace});
parse_options1([{replace, Char} | Tail], Profile=#mb_profile{}) 
  when is_integer(Char)->
    parse_options1(Tail, Profile#mb_profile{error=replace, error_replace_char=Char});
parse_options1([{error_replace_char, Char} | Tail], Profile=#mb_profile{})
  when is_integer(Char)->
    parse_options1(Tail, Profile#mb_profile{error_replace_char=Char});
parse_options1([{bom, true} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{bom=true});
parse_options1([{bom, false} | Tail], Profile=#mb_profile{}) ->
    parse_options1(Tail, Profile#mb_profile{bom=false});
parse_options1([UnknownOption | _], #mb_profile{}) ->
    {error, {unknown_option, [{option, UnknownOption}]}}.

%% ---------------------------------------------------------------------

%% @spec encode(Unicode, Encoding) -> binary() | string() | {error, Reason}
%%
%% @equiv encode(Unicode, Encoding, [])
%%
%% @see encode/3

-spec encode(unicode(), encoding()) -> binary() | string() | {error, tuple()}.

encode(Unicode, Encoding) when is_list(Unicode), is_atom(Encoding) ->
    encode(Unicode, Encoding, []).

%% @spec encode(Unicode, Encoding, Options) -> binary() | string() | {error, Reason}
%%
%%	    Unicode  = unicode()
%%	    Encoding =  'cp037'
%%                | 'cp437'
%%                | 'cp500'
%%                | 'cp737'
%%                | 'cp775'
%%                | 'cp850'
%%                | 'cp852'
%%                | 'cp855'
%%                | 'cp857'
%%                | 'cp860'
%%                | 'cp861'
%%                | 'cp862'
%%                | 'cp863'
%%                | 'cp864'
%%                | 'cp865'
%%                | 'cp866'
%%                | 'cp869'
%%                | 'cp874'
%%                | 'cp875'
%%                | 'cp932'
%%                | 'cp936'
%%                | 'gbk'
%%                | 'cp949'
%%                | 'cp950'
%%                | 'big5'
%%                | 'cp1026'
%%                | 'cp1250'
%%                | 'cp1251'
%%                | 'cp1252'
%%                | 'cp1253'
%%                | 'cp1254'
%%                | 'cp1255'
%%                | 'cp1256'
%%                | 'cp1257'
%%                | 'cp1258'
%%                | 'cp10000'
%%                | 'cp10006'
%%                | 'cp10007'
%%                | 'cp10029'
%%                | 'cp10079'
%%                | 'cp10081'
%%                | 'utf8' 
%%                | 'utf16' 
%%                | 'utf16le' 
%%                | 'utf16be' 
%%                | 'utf32' 
%%                | 'utf32le' 
%%                | 'utf32be'
%%      Options  = [Option]
%%      Option =  {return, list} 
%%              | {return, binary}
%%              | {error, strict} 
%%              | {error, ignore}
%%              | {error, replace} 
%%              | {replace, non_neg_integer()}
%%              | {bom, true} 
%%              | {bom, false}
%%
%% @doc Return a Binary or String.
%%
%% @see encode/2

-spec encode(unicode(), encoding(), options()) -> binary() | string() | {error, tuple()}.

encode(Unicode, Encoding, Options) 
  when is_list(Unicode), is_atom(Encoding), is_list(Options) ->
    case erlang:get(?CODECS) of
        undefined ->
            {error, {illegal_process_dict, [{process_dict, ?CODECS}, 
                                            {detail, "need mb:init()"}]}};
        CodecsDict ->
            case dict:find(Encoding, CodecsDict) of
                {ok, Mod} ->
                    case parse_options(Options, ?MB_ENCODE_OPTIONS_DEFAULT) of
                        {ok, Profile} ->
                            Mod:encode(Unicode, Encoding, Profile);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                error ->
                    {error, {illegal_encoding, [{encoding, Encoding}, 
                                                {line, ?LINE}]}}
            end
    end.

%% ---------------------------------------------------------------------

%% @spec decode(StringOrBinary, Encoding) -> unicode() | {error, Reason}
%%
%% @equiv decode(StringOrBinary, Encoding, [])
%%
%% @see decode/3

-spec decode(string()|binary(), encoding()) -> unicode() | {error, tuple()}.

decode(String, Encoding) when is_list(String), is_atom(Encoding) ->
    decode(String, Encoding, []);
decode(Binary, Encoding) when is_binary(Binary), is_atom(Encoding) ->
    decode(Binary, Encoding, []).

%% @spec decode(StringOrBinary, Encoding, Options) -> unicode()
%%
%%	    StringOrBinary  = string()|binary()
%%	    Encoding =  'cp037'
%%                | 'cp437'
%%                | 'cp500'
%%                | 'cp737'
%%                | 'cp775'
%%                | 'cp850'
%%                | 'cp852'
%%                | 'cp855'
%%                | 'cp857'
%%                | 'cp860'
%%                | 'cp861'
%%                | 'cp862'
%%                | 'cp863'
%%                | 'cp864'
%%                | 'cp865'
%%                | 'cp866'
%%                | 'cp869'
%%                | 'cp874'
%%                | 'cp875'
%%                | 'cp932'
%%                | 'cp936'
%%                | 'gbk'
%%                | 'cp949'
%%                | 'cp950'
%%                | 'big5'
%%                | 'cp1026'
%%                | 'cp1250'
%%                | 'cp1251'
%%                | 'cp1252'
%%                | 'cp1253'
%%                | 'cp1254'
%%                | 'cp1255'
%%                | 'cp1256'
%%                | 'cp1257'
%%                | 'cp1258'
%%                | 'cp10000'
%%                | 'cp10006'
%%                | 'cp10007'
%%                | 'cp10029'
%%                | 'cp10079'
%%                | 'cp10081'
%%                | 'utf8' 
%%                | 'utf16' 
%%                | 'utf16le' 
%%                | 'utf16be' 
%%                | 'utf32' 
%%                | 'utf32le' 
%%                | 'utf32be'
%%      Options  = [Option]
%%      Option =  {return, list} 
%%              | {return, binary}
%%              | {error, strict} 
%%              | {error, ignore}
%%              | {error, replace} 
%%              | {replace, non_neg_integer()}
%%              | {bom, true} 
%%              | {bom, false}
%%
%% @doc Return a Unicode.
%%
%% @see decode/2

-spec decode(string()|binary(), encoding(), options()) -> unicode() | {error, tuple()}.

decode(String, Encoding, Options) 
  when is_list(String), is_atom(Encoding), is_list(Options) ->
    case catch list_to_binary(String) of
        {'EXIT',{badarg, _}} ->
            {error, {illegal_list, [{list, String}, {line, ?LINE}]}};
        Binary ->
            decode(Binary, Encoding, Options)
    end;
decode(Binary, Encoding, Options) 
  when is_binary(Binary), is_atom(Encoding), is_list(Options) ->
    case erlang:get(?CODECS) of
        undefined ->
            {error, {illegal_process_dict, [{process_dict, ?CODECS}, 
                                            {detail, "need mb:init()"}]}};
        CodecsDict ->
            case dict:find(Encoding, CodecsDict) of
                {ok, Mod} ->
                    case parse_options(Options, ?MB_DECODE_OPTIONS_DEFAULT) of
                        {ok, Profile} ->
                            Mod:decode(Binary, Encoding, Profile);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                error ->
                    {error, {illegal_encoding, [{encoding, Encoding}, 
                                                {line, ?LINE}]}}
            end
    end.
