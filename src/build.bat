@path D:\erl5.7.5\bin;%Path%
@del /f /q /s ..\ebin\*.beam ..\priv\*.BIN .\erl_crash.dump
@for %%I in (*.erl) do (
    erlc +debug_info -o ../ebin %%I)
)
erl -noshell -s mb_test test -s init stop
pause