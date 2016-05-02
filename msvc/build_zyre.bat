set VSDIR=vs2013
set VSVER=12
set PLATFORM=x64
set LIBSODIUM=libsodium\builds\msvc
set LIBZMQ=libzmq\builds\msvc
set tools=Microsoft Visual Studio %VSVER%.0\VC\vcvarsall.bat
set environment="%programfiles(x86)%\%tools%"

if not defined DevEnvDir ( call %environment% )

pushd %LIBSODIUM%

pushd %LIBSODIUM%
set sln=libsodium.sln 
call .\buildbase.bat ..\%VSDIR%\libsodium.sln 12
popd

pushd %LIBZMQ%
call .\configure.bat
pushd %VSDIR%
set sln=libzmq.sln
set log=build_%VSDIR%.log
echo Building %PLATFORM% Dyn/Static Debug/Release
msbuild /m /v:n /p:Configuration=DynDebug /p:Platform=%PLATFORM% %sln% > %log%
msbuild /m /v:n /p:Configuration=DynRelease /p:Platform=%PLATFORM% %sln% >> %log%
msbuild /m /v:n /p:Configuration=StaticDebug /p:Platform=%PLATFORM% %sln% >> %log%
msbuild /m /v:n /p:Configuration=StaticRelease /p:Platform=%PLATFORM% %sln% >> %log%
popd
popd


