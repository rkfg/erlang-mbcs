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
-export([modules/0, init/0, decode/2, decode/3, encode/2, encode/3]).
-define(CODECS, mb_codecs).

%%---------------------------------------------------------------------------

-type unicode()  :: [non_neg_integer()].
-type encoding() :: 'cp874' | 'cp_874' | 'iso8859_11' | 'iso_8859_11' | 'cp932'| 'cp_932' | 'cp936' | 'cp_936'| 'gbk' | 'cp949' | 'cp_949'| 'cp950' | 'cp_950' | 'big5' | 'utf8' | 'utf16' | 'utf16le' | 'utf16be' | 'utf32' | 'utf32le' | 'utf32be'.
-type option()   :: 'list' | 'binary' | 'ignore' | 'strict' | 'replace' | {replace, non_neg_integer()}.
-type options()  :: [option()].

%%---------------------------------------------------------------------------

%% @spec modules() -> [module].
%%
%% @doc Return [module].
%%

-spec modules() -> [atom()].

modules() ->
	[
	mb_cp874,
	mb_cp936,   
	mb_cp932,
	mb_cp949,
	mb_cp950,
	mb_gb18030,
	mb_utf8,    
	mb_utf16,   
	mb_utf16le, 
	mb_utf16be, 
	mb_utf32,
	mb_utf32le, 
	mb_utf32be
	].
	
%%---------------------------------------------------------------------------

%% @spec init() -> ok
%%
%% @doc Load all codecs to process dict memory, Return ok.
%%
%% @see init/1

-spec init() -> ok.

init() ->
    lists:foreach(fun(Mod) ->
						Mod:init()
					end,
					modules()),
	CodecsDict = lists:foldl(fun(Mod, Dict) ->
								Encodings = Mod:encodings(),
								lists:foldl(fun(Encoding, D) ->
												dict:store(Encoding, Mod, D)
											end,
											Dict,
											Encodings)
							end,
							dict:new(),
							modules()),
	erlang:put(?CODECS, CodecsDict),
	ok.

%% ---------------------------------------------------------------------

%% @spec encode(Unicode::unicode(), Encoding::encoding()) -> binary()
%%
%% @doc Equivalent to encode(Unicode, Encoding, [strict]).
%%
%% @see encode/3

-spec encode(Unicode::unicode(), Encoding::encoding()) -> binary().

encode(Unicode, Encoding) when is_list(Unicode), is_atom(Encoding) ->
    encode(Unicode, Encoding, [strict]).

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
					Mod:encode(Unicode, Options)
			end
	end.

%% ---------------------------------------------------------------------

%% @spec decode(String::string()|binary(), Encoding::encoding()) -> unicode()
%%
%% @doc Equivalent to decode(String, Encoding, [strict]).
%%
%% @see decode/3

-spec decode(String::string()|binary(), Encoding::encoding()) -> unicode().

decode(String, Encoding) when is_list(String), is_atom(Encoding) ->
    decode(String, Encoding, [strict]);
decode(Binary, Encoding) when is_binary(Binary), is_atom(Encoding) ->
    decode(Binary, Encoding, [strict]).

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
					Mod:decode(Binary, Options)
			end
	end.
