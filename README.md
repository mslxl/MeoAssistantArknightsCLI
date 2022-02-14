# MeoAssistantArknightsCLI

[MeoAssistantArknights](https://github.com/MistEO/MeoAssistantArknights) 的命令行用户界面。

练习下 Haskell ~~用蛤丝科 FFI 实在太怪了~~


当前动作 (抽出时间来一定会写个命令行的)：
``` haskell
M.connect "192.168.56.101:5555"
    <> M.wakeup
    <> M.fight def
    <> M.recruit def {maaRecruitMaxTimes = 4}
    <> M.infrast def
    <> M.visit
    <> M.mall True
    <> M.award
    <> M.start
```

### 构建方式：

``` bash
cd tools
bash ./init.sh #编译 maa 及第三方库
cd ..
cabal build
```
