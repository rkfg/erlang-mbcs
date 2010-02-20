%% 
%% @author Xiangyu LU
%% created on 2010-02-20
%% 
%% Some simple functional test cases, test the mb implementation
-module(mb_test).

%Tests
-export([test/0]).

%% @spec test() -> ok
%%
%% @doc Run the testcase.

test() ->
	mb:init(),
	Unicode = "\x{4f60}\x{597d}",    % Unicode = "ÄãºÃ"
	String  = "\xc4\xe3\xba\xc3",    % String  = "ÄãºÃ"
	String  = mb:encode(Unicode, gbk, [list]),
	Unicode = mb:decode(String,  gbk),
	ok.
