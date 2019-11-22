@echo off

:start
ping -n 2 114.114.114.114 | C:\Windows\System32\find "TTL=" > nul

if errorlevel 1 (
    echo 离线，等待连接...
    rasdial pppppoe test test
) else (
    echo.
    echo 已经连接上。
    C:\Windows\System32\TIMEOUT 60
)

goto:start
