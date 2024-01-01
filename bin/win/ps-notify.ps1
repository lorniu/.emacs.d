param($title, $message, $timeout)

# 加载 Winform 程序集,使用 Out-Null 抑制输出
[system.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms') | Out-Null

# 创建 NotifyIcon 对象
$balloon = New-Object System.Windows.Forms.NotifyIcon
$path = Get-Process -id $pid | Select-Object -ExpandProperty Path
$icon = [System.Drawing.Icon]::ExtractAssociatedIcon($path)
$balloon.Icon = $icon
$balloon.BalloonTipIcon = "Info"
$balloon.BalloonTipTitle = "$title"
$balloon.BalloonTipText = "$message"
$balloon.Visible = $true

# 显示气球提示框，不管设多大，最多显示 5 秒
$balloon.ShowBalloonTip($timeout * 100)
