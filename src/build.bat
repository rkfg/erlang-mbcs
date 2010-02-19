@path D:\erl5.7.4\bin;%Path%
@for %%I in (*.erl) do ( 
    if not "%%I" == "mb_dbcs.erl" (
        erlc +debug_info -o ../ebin %%I)
    )
)
pause