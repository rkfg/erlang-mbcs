%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%%---------------------------------------------------------------------------

-type unicode()  :: [non_neg_integer()].
-type encoding() :: 'cp874' | 'iso_8859_11' | 'cp1250' | 'cp1251' | 'cp1252' | 'cp1253' | 'cp1254' | 'cp1255' | 'cp1256' | 'cp1257' | 'cp1258' | 'cp932' | 'cp936' | 'gbk' | 'cp949' | 'cp950' | 'big5' | 'utf8' | 'utf16' | 'utf16le' | 'utf16be' | 'utf32' | 'utf32le' | 'utf32be'.
-type option()   :: {return, list} | {return, binary} | {error, strict} | {error, ignore} | {error, replace} | {error_replace_char, non_neg_integer()} | {bom, true} | {bom, false}.
-type options()  :: [option()].

%%---------------------------------------------------------------------------

-record(mb_profile, {
	  return             :: atom(),	            % file name
	  error	             :: atom(),             % error method
	  error_replace_char :: non_neg_integer(),	% error replace char
	  bom                :: boolean(),          % encode bom
      codecs             :: tuple()             % dict/sets tuple.
	 }).
