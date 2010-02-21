@path D:\erl5.7.4\bin;%Path%
@del /f /q /s ..\ebin\*.beam
@for %%I in (*.erl) do (
    erlc +debug_info -o ../ebin %%I)
)
erl -noshell -s mb init -s init stop
pause