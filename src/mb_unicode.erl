-module(mb_unicode).
-export([encodings/0, init/0, decode/3, encode/3]).

-include_lib("mb/include/mb.hrl").

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

encode(Unicode, Encoding, #mb_profile{return=Return, bom=Bom}) when is_list(Unicode), is_atom(Encoding) ->
    {NewEncoding} = codecs_info(Encoding),
    NewUnicode  =   case Unicode of
                        [16#FEFF, RestCodes] ->
                            case Bom of
                                true ->
                                    Unicode;
                                false ->
                                    RestCodes
                            end;
                        Unicode ->
                            case Bom of
                                true ->
                                    [16#FEFF, Unicode];
                                false ->
                                    Unicode
                            end
                    end,
    Binary = unicode:characters_to_binary(NewUnicode, unicode, NewEncoding),
    case Return of
        list ->
            erlang:binary_to_list(Binary);
        binary ->
            Binary
    end.

decode(Binary, Encoding, #mb_profile{}) when is_binary(Binary), is_atom(Encoding) ->
    {NewEncoding} = codecs_info(Encoding),
    unicode:characters_to_list(Binary, NewEncoding).
