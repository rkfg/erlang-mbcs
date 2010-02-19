% Note: Do not load this module, juest -include("mb_dbcs.erl").

-define(MB_MODULE, mb).

-record(encode_profile, {
      encode_dict        :: dict(),            % encode mapping dict
      output             :: atom(),            % output format, binary or list
      error              :: atom(),            % error option
	  error_replace_char :: char()             % error replace char
     }).
-define(ENCODE_ERROR_REPLACE_CHAR, $?).        % default replace char

-record(decode_profile, {
      undefined_set      :: set(),             % undefined char set
      leadbytes_set      :: set(),             % lead bytes set
      decode_dict        :: dict(),            % decode mapping dict
      error              :: atom(),            % error option
	  error_replace_char :: non_neg_integer()  % error replace char
     }).
-define(DECODE_ERROR_REPLACE_CHAR, 16#FFFD).   % default replace char

init() ->
    Path = code:priv_dir(?MB_MODULE),
    Txtname = filename:join(Path, ?TXTNAME),
    Binname = filename:join(Path, ?BINNAME),
    case filelib:is_file(Binname) of
        true ->
            {ok, Binary} = file:read_file(Binname),
            erlang:put(?PROCESS_DICT_ATOM, binary_to_term(Binary)),
            ok;
        false ->
            {ok, [PropList]} = file:consult(Txtname),
            DecodeUndefinedSet = sets:from_list(proplists:get_value(undefined,PropList)),
            DecodeLeadByteSet = sets:from_list(proplists:get_value(leadbytes,PropList)),
            DecodeList = proplists:get_value(mapping,PropList),
            DecodeDict = dict:from_list(DecodeList),
            EncodeDict = dict:from_list([{Value, Key} || {Key, Value} <- DecodeList]),
            ok = file:write_file(Binname, term_to_binary({{DecodeUndefinedSet, DecodeLeadByteSet, DecodeDict}, {EncodeDict}})),
            init()
    end.

process_encode_options(Options) when is_list(Options) ->
	OptionDefeault = [{output, binary}, {error, strict}, {error_replace_char, ?ENCODE_ERROR_REPLACE_CHAR}],
	process_encode_options1(Options, dict:from_list(OptionDefeault)).
	
process_encode_options1([], OptionDict) ->
	{ok, OptionDict};
process_encode_options1([Option | OptionsTail], OptionDict) ->
	case Option of
		binary ->
			process_encode_options1(OptionsTail, dict:store(output, binary, OptionDict));
		list ->   
			process_encode_options1(OptionsTail, dict:store(output, list, OptionDict));
		ignore -> 
			process_encode_options1(OptionsTail, dict:store(error, ignore, OptionDict));
		strict -> 
			process_encode_options1(OptionsTail, dict:store(error, strict, OptionDict));
		replace -> 
			process_encode_options1(OptionsTail, dict:store(error, replace, OptionDict));
		{replace, Char} when is_integer(Char) -> 
			process_encode_options1(OptionsTail, dict:store(error_replace_char, Char, dict:store(error, replace, OptionDict)));
		UnknownOption ->
			{error, {cannot_encode, [{reason, unknown_option}, {option, UnknownOption}]}}
	end.
 
encode(Unicode) when is_list(Unicode) ->
    encode(Unicode, [strict]).

encode(Unicode, Options) when is_list(Unicode), is_list(Options) ->
	case process_encode_options(Options) of
		{ok, OptionDict} ->
			case erlang:get(?PROCESS_DICT_ATOM) of
				{_, {EncodeDict}} ->
					EncodeProfile = #encode_profile{encode_dict        = EncodeDict,
													output             = dict:fetch(output, OptionDict),
													error              = dict:fetch(error, OptionDict),
													error_replace_char = dict:fetch(error_replace_char, OptionDict)},
					encode1(Unicode, EncodeProfile, 1, []);
				ProcessDict ->
					{error, {cannot_encode, [{reson, illegal_process_dict}, {process_dict, ProcessDict}, {detail, "maybe you should call mb:init() first"}]}}
			end;
		{error, Reason} ->
			{error, Reason}
	end.    

encode1([], EncodeProfile, _, String) when is_record(EncodeProfile, encode_profile), is_list(String) ->
    OutputString = lists:reverse(String),
    case EncodeProfile#encode_profile.output of
        list   -> OutputString;
        binary -> erlang:list_to_binary(OutputString)
    end;
encode1([Code | RestCodes], #encode_profile{encode_dict=EncodeDict,error=Error, error_replace_char=ErrorReplaceChar}=EncodeProfile, Pos, String) when is_integer(Pos), is_list(String) ->
    case catch dict:fetch(Code, EncodeDict) of
        {'EXIT',{badarg, _}} ->
            case Error of
                ignore ->
                    encode1(RestCodes, EncodeProfile, Pos+1, String);
                replace ->
                    encode1(RestCodes, EncodeProfile, Pos+1, [ErrorReplaceChar | String]);
                strict ->
                    {error, {cannot_encode, [{reason, unmapping_unicode}, {unicode, Code}, {pos, Pos}]}}
            end;
        MultibyteChar ->
            case MultibyteChar > 16#FF of
                false ->
                    encode1(RestCodes, EncodeProfile, Pos+1, [MultibyteChar | String]);
                true ->
                    encode1(RestCodes, EncodeProfile, Pos+1, [MultibyteChar band 16#FF, MultibyteChar bsr 8 | String])
            end
    end.
	
process_decode_options(Options) when is_list(Options) ->
	OptionDefeault = [{error, strict}, {error_replace_char, ?DECODE_ERROR_REPLACE_CHAR}],
	process_decode_options1(Options, dict:from_list(OptionDefeault)).

process_decode_options1([], OptionDict) ->
	{ok, OptionDict};
process_decode_options1([Option | OptionsTail], OptionDict) ->
	case Option of
		strict ->
			process_decode_options1(OptionsTail, dict:store(error, strict, OptionDict));
		ignore -> 
			process_decode_options1(OptionsTail, dict:store(error, ignore, OptionDict));
		replace -> 
			process_decode_options1(OptionsTail, dict:store(error, replace, OptionDict));
		{replace, Char} when is_integer(Char) -> 
			process_decode_options1(OptionsTail, dict:store(error_replace_char, Char, dict:store(error, replace, OptionDict)));
		UnknownOption ->
			{error, {cannot_decode, [{reason, unknown_option}, {option, UnknownOption}]}}	
	end.

decode(Binary) when is_bitstring(Binary) ->
    decode(Binary, [strict]).

decode(Binary, Options) when is_bitstring(Binary), is_list(Options) ->
	case process_decode_options(Options) of
		{ok, OptionDict} ->
			case erlang:get(?PROCESS_DICT_ATOM) of
				{{DecodeUndefinedSet, DecodeLeadByteSet, DecodeDict}, _} ->
					DecodeProfile = #decode_profile{undefined_set      = DecodeUndefinedSet, 
													leadbytes_set      = DecodeLeadByteSet, 
													decode_dict        = DecodeDict, 
													error              = dict:fetch(error, OptionDict),
													error_replace_char = dict:fetch(error_replace_char, OptionDict)},
					decode1(Binary, DecodeProfile, 1, []);
				ProcessDict ->
					{error, {cannot_decode, [{reson, illegal_process_dict}, {process_dict, ProcessDict}, {detail, "maybe you should call mb:init() first"}]}}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

decode1(<<>>, _, _, Unicode) when is_list(Unicode) ->
    lists:reverse(Unicode);
decode1(<<LeadByte:8, Rest/bitstring>>, #decode_profile{undefined_set=UndefinedSet, leadbytes_set=LeadbytesSet, decode_dict=DecodeDict, error=Error, error_replace_char=ErrorReplaceChar}=DecodeProfile, Pos, Unicode) when is_integer(Pos), is_list(Unicode) ->
    case sets:is_element(LeadByte, UndefinedSet) of
        true ->
            case Error of
                ignore ->
                    decode1(Rest, DecodeProfile, Pos+1, Unicode);
                replace ->
                    decode1(Rest, DecodeProfile, Pos+1, [ErrorReplaceChar | Unicode]);
                strict ->
                    {error, {cannot_decode, [{reason, undefined_character}, {char, LeadByte}, {pos, Pos}]}}
            end;
        false ->
            case sets:is_element(LeadByte, LeadbytesSet) of
                false ->
                    case catch dict:fetch(LeadByte, DecodeDict) of
                        {'EXIT',{badarg, _}} ->
                            case Error of
                                ignore ->
                                    decode1(Rest, DecodeProfile, Pos+1, Unicode);
                                replace ->
                                    decode1(Rest, DecodeProfile, Pos+1, [ErrorReplaceChar | Unicode]);
                                strict ->
                                    {error, {cannot_decode, [{reason, unmapping_character}, {char, LeadByte}, {pos, Pos}]}}
                            end;
                        Code ->
                            decode1(Rest, DecodeProfile, Pos+1, [Code | Unicode])
                    end;
                true ->
                    case erlang:bit_size(Rest) =:= 0 of
                        false ->
                            <<FollowByte:8, Rest1/bitstring>> = Rest,
                            MultibyteChar = LeadByte bsl 8 bor FollowByte,
                            case catch dict:fetch(MultibyteChar, DecodeDict) of
                                {'EXIT',{badarg, _}} ->
                                    case Error of
                                        ignore ->
                                            decode1(Rest1, DecodeProfile, Pos+2, Unicode);
                                        replace ->
                                            decode1(Rest1, DecodeProfile, Pos+2, [ErrorReplaceChar | Unicode]);
                                        strict ->
                                            {error, {cannot_decode, [{reason, unmapping_multibyte_char}, {multibyte_char, MultibyteChar}, {pos, Pos}]}}
                                    end;
                                Code ->
                                    decode1(Rest1, DecodeProfile, Pos+2, [Code | Unicode])
                            end;
                        true ->
                            case Error of
                                ignore ->
                                    decode1(Rest, DecodeProfile, Pos+1, Unicode);
                                replace ->
                                    decode1(Rest, DecodeProfile, Pos+1, [ErrorReplaceChar | Unicode]);
                                strict ->
                                    {error, {cannot_decode, [{reason, incomplete_multibyte_sequence}, {leadbyte, LeadByte}, {pos, Pos}]}}
                            end
                    end
            end
    end.
