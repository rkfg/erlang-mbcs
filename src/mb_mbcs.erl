-module(mb_mbcs).
-export([encodings/0, init/0, decode/3, encode/3]).

-include_lib("mb/include/mb.hrl").

encodings() ->
    [
    cp874,
    cp932,
    cp936,
    gbk,
    cp949,
    cp950,
    big5,
    cp1250,
    cp1251,
    cp1252,
    cp1253,
    cp1254,
    cp1255,
    cp1256,
    cp1257,
    cp1258
    ].
    
codecs_info(cp874) ->
    {mb_codecs_cp874, "CP874.CONF", "CP874.BIN"};
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
    {mb_codecs_cp1258, "CP1258.CONF", "CP1258.BIN"}.

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
            DecodeUndefinedSet = sets:from_list(proplists:get_value(undefined,PropList)),
            DecodeLeadByteSet = sets:from_list(proplists:get_value(leadbytes,PropList)),
            DecodeList = proplists:get_value(mapping,PropList),
            DecodeDict = dict:from_list(DecodeList),
            EncodeDict = dict:from_list([{Value, Key} || {Key, Value} <- DecodeList]),
            ok = file:write_file(Binpath, term_to_binary({{DecodeUndefinedSet, DecodeLeadByteSet, DecodeDict}, {EncodeDict}})),
            init(Encoding)
    end.
    
init() ->
    lists:foreach(fun(Encoding) ->
                    ok = init(Encoding)
                end,
                encodings()).

encode(Unicode, Encoding, Profile=#mb_profile{}) when is_list(Unicode), is_atom(Encoding) ->
    {PROCESS_DICT_ATOM, _CONF_NAME, _BIN_NAME} = codecs_info(Encoding),
    case erlang:get(PROCESS_DICT_ATOM) of
        {_, {EncodeDict}} ->
            encode1(Unicode, Profile#mb_profile{codecs={EncodeDict}}, 1, []);
        _OtherDict ->
            {error, {cannot_encode, [{reson, illegal_process_dict}, {process_dict, PROCESS_DICT_ATOM}, {detail, "maybe you should call mb:init() first"}]}}
    end.

encode1([], #mb_profile{return=Return}, _, String) when is_list(String) ->
    ReturnString = lists:reverse(String),
    case Return of
        list   -> ReturnString;
        binary -> erlang:list_to_binary(ReturnString)
    end;
encode1([Code | RestCodes], Profile=#mb_profile{error=Error, error_replace_char=ErrorReplaceChar, codecs={EncodeDict}}, Pos, String) when is_integer(Pos), is_list(String) ->
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
                    {error, {cannot_encode, [{reason, unmapping_unicode}, {unicode, Code}, {pos, Pos}]}}
            end
    end.

decode(Binary, Encoding, Profile=#mb_profile{}) when is_binary(Binary), is_atom(Encoding) ->
    {PROCESS_DICT_ATOM, _CONF_NAME, _BIN_NAME} = codecs_info(Encoding), 
    case erlang:get(PROCESS_DICT_ATOM) of
        {{DecodeUndefinedSet, DecodeLeadByteSet, DecodeDict}, _} ->
            decode1(Binary, Profile#mb_profile{codecs={DecodeUndefinedSet, DecodeLeadByteSet, DecodeDict}}, 1, []);
        _OtherDict ->
            {error, {cannot_decode, [{reson, illegal_process_dict}, {process_dict, PROCESS_DICT_ATOM}, {detail, "maybe you should call mb:init() first"}]}}
    end.

decode1(<<>>, _, _, Unicode) when is_list(Unicode) ->
    lists:reverse(Unicode);
decode1(<<LeadByte:8, Rest/binary>>, Profile=#mb_profile{error=Error, error_replace_char=ErrorReplaceChar, codecs={DecodeUndefinedSet, DecodeLeadbytesSet, DecodeDict}}, Pos, Unicode) when is_integer(Pos), is_list(Unicode) ->
    case sets:is_element(LeadByte, DecodeUndefinedSet) of
        true ->
            case Error of
                ignore ->
                    decode1(Rest, Profile, Pos+1, Unicode);
                replace ->
                    decode1(Rest, Profile, Pos+1, [ErrorReplaceChar | Unicode]);
                strict ->
                    {error, {cannot_decode, [{reason, undefined_character}, {character, LeadByte}, {pos, Pos}]}}
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
                                    {error, {cannot_decode, [{reason, unmapping_character}, {character, LeadByte}, {pos, Pos}]}}
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
                                    {error, {cannot_decode, [{reason, incomplete_multibyte_sequence}, {leadbyte, LeadByte}, {pos, Pos}]}}
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
                                            {error, {cannot_decode, [{reason, unmapping_multibyte_character}, {multibyte_character, MultibyteChar}, {pos, Pos}]}}
                                    end
                            end
                    end
            end
    end.
