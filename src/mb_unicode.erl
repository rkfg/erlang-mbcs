-module(mb_unicode).
-export([encodings/0, init/0, decode/3, encode/3]).

encodings() ->
	[
	utf8,
	utf16,
	utf16le,
	utf16be,
	utf32,
	utf32le,
	utf32be
	].
	
codecs_info(utf8) ->
	{utf8};
codecs_info(utf16) ->
	{utf16};
codecs_info(utf16le) ->
	{{utf16, little}};
codecs_info(utf16be) ->
	{{utf16, big}};
codecs_info(utf32) ->
	{utf32};
codecs_info(utf32le) ->
	{{utf32, little}};
codecs_info(utf32be) ->
	{{utf32, big}}.

init() ->
    ok.
	
process_encode_options(Options) when is_list(Options) ->
	OptionDefeault = [{output, binary}, {bom, false}],
	process_encode_options1(Options, dict:from_list(OptionDefeault)).

process_encode_options1([], OptionDict) ->
	{ok, OptionDict};
process_encode_options1([Option | OptionsTail], OptionDict) ->
	case Option of
		binary ->
			process_encode_options1(OptionsTail, dict:store(output, binary, OptionDict));
		list ->   
			process_encode_options1(OptionsTail, dict:store(output, list, OptionDict));
		bom -> 
			process_encode_options1(OptionsTail, dict:store(bom, true, OptionDict));
		{bom, true} -> 
			process_encode_options1(OptionsTail, dict:store(bom, true, OptionDict));
		{bom, false}->
			process_encode_options1(OptionsTail, dict:store(bom, false, OptionDict));
		UnknownOption ->
			{error, {cannot_encode, [{reason, unknown_option}, {option, UnknownOption}]}}
	end.

encode(Unicode, Encoding, Options) when is_list(Unicode), is_atom(Encoding), is_list(Options) ->
	case process_encode_options(Options) of
		{ok, OptionDict} ->
			{NewEncoding} = codecs_info(Encoding),
			NewUnicode  =   case Unicode of
								[16#FEFF, RestCodes] ->
									case dict:fetch(bom, OptionDict) of
										true ->
											Unicode;
										false ->
											RestCodes
									end;
								Unicode ->
									case dict:fetch(bom, OptionDict) of
										true ->
											[16#FEFF, Unicode];
										false ->
											Unicode
									end
							end,
			Binary = unicode:characters_to_binary(NewUnicode, unicode, NewEncoding),
			case dict:fetch(output, OptionDict) of
				list ->
					erlang:binary_to_list(Binary);
				binary ->
					Binary
			end;
		{error, Reason} ->
			{error, Reason}
	end.

decode(Binary, Encoding, Options) when is_binary(Binary), is_atom(Encoding), is_list(Options) ->
	{NewEncoding} = codecs_info(Encoding),
    unicode:characters_to_list(Binary, NewEncoding).
