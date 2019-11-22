@echo off

echo =========================================
echo From Local to Vultr 16 DCs Ping Test.beta
echo 从本地到 vultr 的16个机房的 ping 延迟测试脚本
echo =========================================
pause

echo=
echo =============================
echo Seattle - 西雅图 美国
ping wa-us-ping.vultr.com -n 10

echo=
echo =============================
echo Los Angeles - 洛杉矶 美国
ping lax-ca-us-ping.vultr.com -n 10

echo=
echo =============================
echo Silicon Valley - 硅谷 美国
ping sjo-ca-us-ping.vultr.com -n 10

echo=
echo =============================
echo New York - 纽约 美国
ping nj-us-ping.vultr.com -n 10

echo=
echo =============================
echo Chicago - 芝加哥 美国
ping il-us-ping.vultr.com -n 10

echo=
echo =============================
echo Dallas - 达拉斯 美国
ping tx-us-ping.vultr.com -n 10

echo=
echo =============================
echo Atlanta - 亚特兰大 美国
ping ga-us-ping.vultr.com -n 10

echo=
echo =============================
echo Miami - 迈阿密 美国
ping fl-us-ping.vultr.com -n 10

echo=
pause
echo =============================
echo New Jersey - 新泽西 美国
ping nj-us-ping.vultr.com -n 10

echo=
echo =============================
echo Amsterdam - 安姆斯特丹 荷兰
ping ams-nl-ping.vultr.com -n 10

echo=
echo =============================
echo Paris - 巴黎 法国
ping par-fr-ping.vultr.com -n 10

echo=
echo =============================
echo Frankfurt - 法兰克福 德国
ping fra-de-ping.vultr.com -n 10

echo=
echo =============================
echo London - 伦敦 英国
ping lon-gb-ping.vultr.com -n 10

echo=
echo =============================
echo Sydney - 悉尼 澳大利亚
ping syd-au-ping.vultr.com -n 10

echo=
echo =============================
echo Singapore - 新加坡
ping sgp-ping.vultr.com -n 10

echo=
echo Tokyo - 东京 日本
ping hnd-jp-ping.vultr.com -n 10

echo=
pause
