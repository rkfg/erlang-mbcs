-module(mb_mbcs).
-export([encodings/0, init/0, decode/3, encode/3]).

-include_lib("mb/include/mb.hrl").

-record(mbcs_codecs, {
	  undefined    :: set(),	% undefine bytes
	  leadbytes	   :: set(),    % dbcs lead bytes
	  mbtable      :: dict(),	% multiple byte to wide character table
	  wctable      :: dict()    % wide character to multiple byte table
	 }).

encodings() ->
    [
    cp037,
    cp437,
    cp500,
    cp737,
    cp775,
    cp850,
    cp852,
    cp855,
    cp857,
    cp860,
    cp861,
    cp862,
    cp863,
    cp864,
    cp865,
    cp866,
    cp869,
    cp874,
    cp875,
    cp932,
    cp936,
    gbk,
    cp949,
    cp950,
    big5,
    cp1026,
    cp1250,
    cp1251,
    cp1252,
    cp1253,
    cp1254,
    cp1255,
    cp1256,
    cp1257,
    cp1258,
    cp10000,
    cp10006,
    cp10007,
    cp10029,
    cp10079,
    cp10081
    ].

codecs_info(cp037) ->
    {mb_codecs_cp037, "CP037.CONF", "CP037.BIN"};
codecs_info(cp437) ->
    {mb_codecs_cp437, "CP437.CONF", "CP437.BIN"};
codecs_info(cp500) ->
    {mb_codecs_cp500, "CP500.CONF", "CP500.BIN"};
codecs_info(cp737) ->
    {mb_codecs_cp737, "CP737.CONF", "CP737.BIN"};
codecs_info(cp775) ->
    {mb_codecs_cp775, "CP775.CONF", "CP775.BIN"};
codecs_info(cp850) ->
    {mb_codecs_cp850, "CP850.CONF", "CP850.BIN"};
codecs_info(cp852) ->
    {mb_codecs_cp852, "CP852.CONF", "CP852.BIN"};
codecs_info(cp855) ->
    {mb_codecs_cp855, "CP855.CONF", "CP855.BIN"};
codecs_info(cp857) ->
    {mb_codecs_cp857, "CP857.CONF", "CP857.BIN"};
codecs_info(cp860) ->
    {mb_codecs_cp860, "CP860.CONF", "CP860.BIN"};
codecs_info(cp861) ->
    {mb_codecs_cp861, "CP861.CONF", "CP861.BIN"};
codecs_info(cp862) ->
    {mb_codecs_cp862, "CP862.CONF", "CP862.BIN"};
codecs_info(cp863) ->
    {mb_codecs_cp863, "CP863.CONF", "CP863.BIN"};
codecs_info(cp864) ->
    {mb_codecs_cp864, "CP864.CONF", "CP864.BIN"};
codecs_info(cp865) ->
    {mb_codecs_cp865, "CP865.CONF", "CP865.BIN"};
codecs_info(cp866) ->
    {mb_codecs_cp866, "CP866.CONF", "CP866.BIN"};
codecs_info(cp869) ->
    {mb_codecs_cp869, "CP869.CONF", "CP869.BIN"};
codecs_info(cp874) ->
    {mb_codecs_cp874, "CP874.CONF", "CP874.BIN"};
codecs_info(cp875) ->
    {mb_codecs_cp875, "CP875.CONF", "CP875.BIN"};
codecs_info(cp932) ->
    {mb_codecs_cp932, "CP932.CONF", "CP932.BIN"};
codecs_info(cp936) ->
    {mb_codecs_cp936, "CP936.CONF", "CP936.BIN"};
codecs_info(gbk) ->
    {mb_codecs_cp936, "CP936.CONF", "CP936.BIN"};
codecs_info(cp949) ->
    {mb_codecs_cp949, "CP949.CONF", "CP949.BIN"};
codecs_info(cp950) ->
    {mb_codecs_cp950, "CP950.CONF", "CP950.BIN"};
codecs_info(big5) ->
    {mb_codecs_cp950, "CP950.CONF", "CP950.BIN"};
codecs_info(cp1026) ->
    {mb_codecs_cp037, "CP1026.CONF", "CP1026.BIN"};
codecs_info(cp1250) ->
    {mb_codecs_cp1250, "CP1250.CONF", "CP1250.BIN"};
codecs_info(cp1251) ->
    {mb_codecs_cp1251, "CP1251.CONF", "CP1251.BIN"};
codecs_info(cp1252) ->
    {mb_codecs_cp1252, "CP1252.CONF", "CP1252.BIN"};
codecs_info(cp1253) ->
    {mb_codecs_cp1253, "CP1253.CONF", "CP1253.BIN"};
codecs_info(cp1254) ->
    {mb_codecs_cp1254, "CP1254.CONF", "CP1254.BIN"};
codecs_info(cp1255) ->
    {mb_codecs_cp1255, "CP1255.CONF", "CP1255.BIN"};
codecs_info(cp1256) ->
    {mb_codecs_cp1256, "CP1256.CONF", "CP1256.BIN"};
codecs_info(cp1257) ->
    {mb_codecs_cp1257, "CP1257.CONF", "CP1257.BIN"};
codecs_info(cp1258) ->
    {mb_codecs_cp1258, "CP1258.CONF", "CP1258.BIN"};
codecs_info(cp10000) ->
    {mb_codecs_cp10000, "CP10000.CONF", "CP10000.BIN"};
codecs_info(cp10006) ->
    {mb_codecs_cp10006, "CP10006.CONF", "CP10006.BIN"};
codecs_info(cp10007) ->
    {mb_codecs_cp10007, "CP10007.CONF", "CP10007.BIN"};
codecs_info(cp10029) ->
    {mb_codecs_cp10029, "CP10029.CONF", "CP10029.BIN"};
codecs_info(cp10079) ->
    {mb_codecs_cp10079, "CP10079.CONF", "CP10079.BIN"};
codecs_info(cp10081) ->
    {mb_codecs_cp10081, "CP10081.CONF", "CP10081.BIN"}.


init(Encoding) ->
    {Processdict, Confname, Binname} = codecs_info(Encoding), 
    Path = code:priv_dir(mb),
    Confpath = filename:join(Path, Confname),
    Binpath  = filename:join(Path, Binname),
    case filelib:is_file(Binpath) of
        true ->
            case erlang:get(Processdict) of
                undefined ->
                    {ok, Binary} = file:read_file(Binpath),
                    undefined = erlang:put(Processdict, binary_to_term(Binary)),
                    ok;
                Any when is_tuple(Any) ->
                    ok
            end;
        false ->
            {ok, [PropList]} = file:consult(Confpath),
            Undefined = sets:from_list(proplists:get_value(undefined,PropList)),
            LeadBytes = sets:from_list(proplists:get_value(leadbytes,PropList)),
            DecodeList = proplists:get_value(mapping,PropList),
            MBTable = dict:from_list(DecodeList),
            WCTable = dict:from_list([{Value, Key} || {Key, Value} <- DecodeList]),
            MbcsCodecs = #mbcs_codecs{undefined=Undefined, leadbytes=LeadBytes, mbtable=MBTable, wctable=WCTable},
            ok = file:write_file(Binpath, term_to_binary(MbcsCodecs)),
            init(Encoding)
    end.
    
init() ->
    lists:foreach(fun(Encoding) ->
                    ok = init(Encoding)
                end,
                encodings()).

encode(Unicode, Encoding, Profile=#mb_profile{}) when is_list(Unicode), is_atom(Encoding) ->
    {ProcessDict, _, _} = codecs_info(Encoding),
    case erlang:get(ProcessDict) of
        Codecs when is_record(Codecs, mbcs_codecs) ->
            encode1(Unicode, Profile#mb_profile{codecs=Codecs}, 1, []);
        _OtherDict ->
            {error, {illegal_process_dict, [{process_dict, ProcessDict}, {detail, "maybe you should call mb:init() first"}]}}
    end.

encode1([], #mb_profile{return=Return}, _, String) when is_list(String) ->
    ReturnString = lists:reverse(String),
    case Return of
        list   -> ReturnString;
        binary -> erlang:list_to_binary(ReturnString)
    end;
encode1([Code | RestCodes], Profile=#mb_profile{error=Error, error_replace_char=ErrorReplaceChar, codecs=#mbcs_codecs{wctable=EncodeDict}}, Pos, String) when is_integer(Pos), is_list(String) ->
    case dict:find(Code, EncodeDict) of
        {ok, MultibyteChar} ->
            case MultibyteChar > 16#FF of
                false ->
                    encode1(RestCodes, Profile, Pos+1, [MultibyteChar | String]);
                true ->
                    encode1(RestCodes, Profile, Pos+1, [MultibyteChar band 16#FF, MultibyteChar bsr 8 | String])
            end;
        error ->
            case Error of
                ignore ->
                    encode1(RestCodes, Profile, Pos+1, String);
                replace ->
                    encode1(RestCodes, Profile, Pos+1, [ErrorReplaceChar | String]);
                strict ->
                    {error, {unmapping_unicode, [{unicode, Code}, {pos, Pos}]}}
            end
    end.

decode(Binary, Encoding, Profile=#mb_profile{}) when is_binary(Binary), is_atom(Encoding) ->
    {ProcessDict, _, _} = codecs_info(Encoding), 
    case erlang:get(ProcessDict) of
        Codecs when is_record(Codecs, mbcs_codecs) ->
            decode1(Binary, Profile#mb_profile{codecs=Codecs}, 1, []);
        _OtherDict ->
            {error, {illegal_process_dict, [{process_dict, ProcessDict}, {detail, "maybe you should call mb:init() first"}]}}
    end.

decode1(<<>>, _, _, Unicode) when is_list(Unicode) ->
    lists:reverse(Unicode);
decode1(<<LeadByte:8, Rest/binary>>, Profile=#mb_profile{error=Error, error_replace_char=ErrorReplaceChar, codecs=#mbcs_codecs{undefined=DecodeUndefinedSet, leadbytes=DecodeLeadbytesSet, mbtable=DecodeDict}}, Pos, Unicode) when is_integer(Pos), is_list(Unicode) ->
    case sets:is_element(LeadByte, DecodeUndefinedSet) of
        true ->
            case Error of
                ignore ->
                    decode1(Rest, Profile, Pos+1, Unicode);
                replace ->
                    decode1(Rest, Profile, Pos+1, [ErrorReplaceChar | Unicode]);
                strict ->
                    {error, {undefined_character, [{character, LeadByte}, {pos, Pos}]}}
            end;
        false ->
            case sets:size(DecodeLeadbytesSet) =/= 0 andalso sets:is_element(LeadByte, DecodeLeadbytesSet) of
                false ->
                    case dict:find(LeadByte, DecodeDict) of
                        {ok, Code} ->
                            decode1(Rest, Profile, Pos+1, [Code | Unicode]);
                        error ->
                            case Error of
                                ignore ->
                                    decode1(Rest, Profile, Pos+1, Unicode);
                                replace ->
                                    decode1(Rest, Profile, Pos+1, [ErrorReplaceChar | Unicode]);
                                strict ->
                                    {error, {unmapping_character, [{character, LeadByte}, {pos, Pos}]}}
                            end
                    end;
                true ->
                    case erlang:bit_size(Rest) of
                        0 ->
                            case Error of
                                ignore ->
                                    decode1(Rest, Profile, Pos+1, Unicode);
                                replace ->
                                    decode1(Rest, Profile, Pos+1, [ErrorReplaceChar | Unicode]);
                                strict ->
                                    {error, {incomplete_multibyte_sequence, [{leadbyte, LeadByte}, {pos, Pos}]}}
                            end;
                        _Any ->
                            <<FollowByte:8, Rest1/binary>> = Rest,
                            MultibyteChar = LeadByte bsl 8 bor FollowByte,
                            case dict:find(MultibyteChar, DecodeDict) of
                                {ok, Code} ->
                                    decode1(Rest1, Profile, Pos+2, [Code | Unicode]);
                                error ->
                                    case Error of
                                        ignore ->
                                            decode1(Rest1, Profile, Pos+2, Unicode);
                                        replace ->
                                            decode1(Rest1, Profile, Pos+2, [ErrorReplaceChar | Unicode]);
                                        strict ->
                                            {error, {unmapping_multibyte_character, [{multibyte_character, MultibyteChar}, {pos, Pos}]}}
                                    end
                            end
                    end
            end
    end.
