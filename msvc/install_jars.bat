set jar_dir=%1
call mvn install:install-file -DgroupId=jlowenz -DartifactId=quickmsg -Dversion=1.0 -Dpackaging=jar -Dfile="%jar_dir%\quickmsg.jar"
call mvn install:install-file -DgroupId=jlowenz -DartifactId=quickmsg-native -Dversion=1.0 -Dpackaging=jar -Dfile="%jar_dir%\quickmsg-native.jar"