@REM SBT launcher script
@REM 
@REM Envioronment:
@REM JAVA_HOME - location of a JDK home dir (mandatory)
@REM SBT_OPTS  - JVM options (optional)


@setlocal

@echo off
set SBT_HOME=%~dp0
set ERROR_CODE=0

rem We use the value of the JAVACMD environment variable if defined
set _JAVACMD=%JAVACMD%

if "%_JAVACMD%"=="" (
  if not "%JAVA_HOME%"=="" (
    if exist "%JAVA_HOME%\bin\java.exe" set "_JAVACMD=%JAVA_HOME%\bin\java.exe"
  )
)

if "%_JAVACMD%"=="" set _JAVACMD=java

rem We use the value of the JAVA_OPTS environment variable if defined
set _JAVA_OPTS=%JAVA_OPTS%
if "%_JAVA_OPTS%"=="" set _JAVA_OPTS=-Dhttp.proxyHost=proxy.fh-wedel.de -Dhttp.proxyPort=3128 -Drun.mode=production -Dsbt.log.format=true -server -Xms1536m -Xmx1536m -XX:MaxPermSize=384m -XX:ReservedCodeCacheSize=192m -XX:+UseAdaptiveSizePolicy -XX:+UseParallelGC -XX:+UseParallelOldGC

:run

"%_JAVACMD%" %_JAVA_OPTS% %SBT_OPTS% -cp "%SBT_HOME%jansi.jar;%SBT_HOME%sbt-launch.jar;%SBT_HOME%classes" SbtJansiLaunch %*
if ERRORLEVEL 1 goto error
goto end

:error
set ERROR_CODE=1

:end

@endlocal

exit /B %ERROR_CODE%