%% 
%% @author Xiangyu LU
%% created on 2010-02-20
%% 
%% Some simple functional test cases, test the mb implementation
-module(mb_test).
-export([encode/3, decode/3]).
-include_lib("eunit/include/eunit.hrl").

encode(Unicode, Encoding, Options) ->
	mb:encode(Unicode, Encoding, Options).
	
decode(String, Encoding, Options) ->
	mb:decode(String, Encoding, Options).


mb_test_() ->
	ok = mb:init(),
	Unicode = "\x{4f60}\x{597d}",    % Unicode = "ÄãºÃ"
	GBKString  = "\xc4\xe3\xba\xc3",    % String  = "ÄãºÃ"
	[?_assert(encode(Unicode, gbk, [list]) =:= GBKString),
     ?_assert(decode(GBKString, gbk, []) =:= Unicode)
    ].
