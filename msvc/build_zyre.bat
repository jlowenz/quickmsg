@echo off
set VSDIR=vs2013
set VSVER=12
set PLATFORM=x64
set LIBSODIUM=libsodium\builds\msvc\%VSDIR%
set LIBZMQ=libzmq\builds\msvc
set CZMQ=czmq\builds\msvc
set ZYRE=zyre\builds\msvc
set tools=Microsoft Visual Studio %VSVER%.0\VC\vcvarsall.bat
set environment="%programfiles(x86)%\%tools%"

if not defined DevEnvDir ( call %environment% )

if "%1"=="" (set BUILD=All) else (set BUILD=%1)

if "%BUILD%"=="All" (set all=1) else (set all=0)
if "%BUILD%"=="libsodium" (set bsodi=1) else (set bsodi=0)
if "%BUILD%"=="libzmq" (set bzmq=1) else (set bzmq=0)
if "%BUILD%"=="czmq" (set bczmq=1) else (set bczmq=0)
if "%BUILD%"=="zyre" (set bzyre=1) else (set bzyre=0)

set /A "runsodium = %all% | %bsodi%"
if %runsodium% equ 1 (
pushd %LIBSODIUM%
call :compile libsodium.sln "libsodium"
popd
)

set /A "runzmq = %all% | %bzmq%"
if %runzmq% equ 1 (
pushd %LIBZMQ%
call .\configure.bat
pushd %VSDIR%
call :compile libzmq.sln "libzmq"
popd
popd
)

set /A "runczmq = %all% | %bczmq%"
if %runczmq% equ 1 (
pushd %CZMQ%\%VSDIR%
call :compile czmq.sln "czmq"
popd
)

set /A "runzyre = %all% | %bzyre%"
if %runzyre% equ 1 (
echo Changing to %ZYRE%\%VSDIR%
pushd %ZYRE%\%VSDIR%
call :compile zyre.sln "zyre"
popd
)

echo Done
exit /B 0

:compile
set sln=%1
set nom=%2
set log=build_%VSDIR%_%nom%.log
echo Building %nom% : %PLATFORM% Dyn Debug
msbuild /m /v:n /p:Configuration=DynDebug /p:Platform=%PLATFORM% %sln%
echo Building %nom% : %PLATFORM% Dyn Release
msbuild /m /v:n /p:Configuration=DynRelease /p:Platform=%PLATFORM% %sln%
echo Building %nom% : %PLATFORM% Static Debug
msbuild /m /v:n /p:Configuration=StaticDebug /p:Platform=%PLATFORM% %sln% 
echo Building %nom% : %PLATFORM% Static Release
msbuild /m /v:n /p:Configuration=StaticRelease /p:Platform=%PLATFORM% %sln%
exit /B 0

