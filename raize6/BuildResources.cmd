@echo off

set ResComp="C:\Program Files\Embarcadero\RAD Studio\7.0\Bin\RC.exe"

echo.
echo Compiler: %ResComp%
echo.
echo.


echo ## Compiling RCDemoResources.rc...
echo.
%ResComp% -foRCDemoResources.res RCDemoResources.rc
echo.


if "%1" == "Auto" goto Done
pause
                                
:Done

