@echo off

:start
ping -n 2 114.114.114.114 | C:\Windows\System32\find "TTL=" > nul

if errorlevel 1 (
    echo ���ߣ��ȴ�����...
    rasdial pppppoe test test
) else (
    echo.
    echo �Ѿ������ϡ�
    C:\Windows\System32\TIMEOUT 60
)

goto:start
