-module(mb_unicode).
-export([init/1, decode/3, encode/3]).

init(Mod) when is_atom(Mod) ->
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

encode(Mod, Unicode, Options) when is_atom(Mod), is_list(Unicode), is_list(Options) ->
	case process_encode_options(Options) of
		{ok, OptionDict} ->
			{mb, ?MODULE, Encoding, _, _} = Mod:codecs_config(),
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
			Binary = unicode:characters_to_binary(NewUnicode, unicode, Encoding),
			case dict:fetch(output, OptionDict) of
				list ->
					erlang:binary_to_list(Binary);
				binary ->
					Binary
			end;
		{error, Reason} ->
			{error, Reason}
	end.

decode(Mod, Binary, Options) when is_atom(Mod), is_binary(Binary), is_list(Options) ->
	{mb, ?MODULE, Encoding, _, _} = Mod:codecs_config(),
    unicode:characters_to_list(Binary, Encoding).
