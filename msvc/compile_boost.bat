set boostdir=%1
echo %boostdir%
set addr=64
set PATH=.;%PATH%
pushd %boostdir%
bjam address-model=%addr% link=static runtime-link=static variant=release -j8
bjam address-model=%addr% link=static runtime-link=static variant=debug -j8
rem bjam address-model=%addr% link=shared runtime-link=static variant=release -j8
rem bjam address-model=%addr% link=shared runtime-link=static variant=debug -j8
popd
