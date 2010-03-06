-module(mb_gb18030).
-export([encodings/0, init/0, decode/3, encode/3]).

encodings() ->
    [gb18030].
    
codecs_info(gb18030) ->
    {mb_codecs_gb18030, "GB18030.CONF", "GB18030.BIN"}.

init() ->
    ok.
    
encode(Unicode, Encoding, Options) 
  when is_list(Unicode), is_atom(Encoding), is_list(Options) ->
    {mb_codecs_gb18030, "GB18030.CONF", "GB18030.BIN"} = codecs_info(gb18030),
    {error, not_implement}.

decode(Binary, Encoding, Options) 
  when is_binary(Binary), is_atom(Encoding), is_list(Options) ->
    {mb_codecs_gb18030, "GB18030.CONF", "GB18030.BIN"} = codecs_info(gb18030),
    {error, not_implement}.
