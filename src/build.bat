@path D:\erl5.7.5\bin;%Path%
@del /f /q /s ..\ebin\*.beam ..\ebin\*.app .\erl_crash.dump >NUL
@for %%I in (*.erl) do (
    erlc +debug_info -o ../ebin %%I)
)
copy /y mbcs.app ..\ebin
erl -noshell -s mbcs_test test -s init stop
pause