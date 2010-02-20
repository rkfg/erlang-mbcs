@path D:\erl5.7.4\bin;%Path%
@for %%I in (*.erl) do (
    erlc +debug_info -o ../ebin %%I)
)
pause