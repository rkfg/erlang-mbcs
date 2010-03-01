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
-export([init/0, decode/2, decode/3, encode/2, encode/3]).
-define(CODECS, mb_codecs).

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
    mb_mbcs:init(),
	mb_unicode:init(),
	mb_gb18030:init(),
	CodecsPropList= fun(Mod) ->
						[{X, Mod} || X <- Mod:encodings()]
					end,
	CodecsDict = dict:from_list(CodecsPropList(mb_mbcs) ++ CodecsPropList(mb_unicode) ++ CodecsPropList(mb_gb18030)),
	erlang:put(?CODECS, CodecsDict),
	ok.

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
					Mod:encode(Unicode, Encoding, Options)
			end
	end.

%% ---------------------------------------------------------------------

%% @spec decode(String::string()|binary(), Encoding::encoding()) -> unicode()
%%
%% @doc Equivalent to decode(String, Encoding, []).
%%
%% @see decode/3

-spec decode(String::string()|binary(), Encoding::encoding()) -> unicode().

decode(String, Encoding) when is_list(String), is_atom(Encoding) ->
    decode(String, Encoding, []);
decode(Binary, Encoding) when is_binary(Binary), is_atom(Encoding) ->
    decode(Binary, Encoding, []).

%% @spec decode(String::string()|binary(), Encoding::encoding(), Options::options()) -> unicode()
%%
%% @doc Return a Unicode.
%%
%% @see decode/2

-spec decode(String::string()|binary(), Encoding::encoding(), Options::options()) -> unicode().

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
					Mod:decode(Binary, Encoding, Options)
			end
	end.
