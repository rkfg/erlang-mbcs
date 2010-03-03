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

encode(Unicode, Encoding, OptionDict) when is_list(Unicode), is_atom(Encoding), is_tuple(OptionDict) ->
    {NewEncoding} = codecs_info(Encoding),
    NewUnicode  =   case Unicode of
                        [16#FEFF, RestCodes] ->
                            case dict:find(bom, OptionDict) of
                                {ok, true} ->
                                    Unicode;
                                {ok, false} ->
                                    RestCodes
                            end;
                        Unicode ->
                            case dict:find(bom, OptionDict) of
                                {ok, true} ->
                                    [16#FEFF, Unicode];
                                {ok, false} ->
                                    Unicode
                            end
                    end,
    Binary = unicode:characters_to_binary(NewUnicode, unicode, NewEncoding),
    case dict:find(return, OptionDict) of
        {ok, list} ->
            erlang:binary_to_list(Binary);
        {ok, binary} ->
            Binary
    end.

decode(Binary, Encoding, OptionDict) when is_binary(Binary), is_atom(Encoding), is_tuple(OptionDict) ->
    {NewEncoding} = codecs_info(Encoding),
    unicode:characters_to_list(Binary, NewEncoding).
