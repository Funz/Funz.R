
set PATH=%PATH%;C:\Program Files\R\R-4.1.0\bin
start /b R.exe CMD BATCH %*
for /F "TOKENS=1,2,*" %%a in ('tasklist /FI "IMAGENAME eq R.exe"') do set PID_R=%%b
echo %PID_R% > PID

:loop
tasklist | find " %PID_R% " >nul
if not errorlevel 1 (
    timeout /t 1 >nul
    goto :loop
)

del /f PID

findstr "Err" *.Rout
if  errorlevel 1 (
    exit 1
)
