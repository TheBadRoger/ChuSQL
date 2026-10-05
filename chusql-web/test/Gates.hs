{-# LANGUAGE OverloadedStrings #-}

module Gates (gateSpec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec

-- 门禁测试：安装打包脚本、发布流水线、文档结构里必须出现的内容。

-- | 按 UTF-8 读取文件
readUtf8 :: FilePath -> IO Text
readUtf8 path = TE.decodeUtf8 <$> BS.readFile path

-- | 门禁用例入口
gateSpec :: Spec
gateSpec = do
    describe "Installer scripts (scripts/)" $ do
        it "install.sh releases files, writes settings.toml, sets PATH and removes the package" $ do
            installer <- readUtf8 (".." </> "scripts" </> "install.sh")
            installer `shouldSatisfy` T.isInfixOf "libchusql_core_storage"
            installer `shouldSatisfy` T.isInfixOf "settings.toml"
            installer `shouldSatisfy` T.isInfixOf "PATH"
            installer `shouldSatisfy` T.isInfixOf "rm -rf"
        -- 一份脚本三种来源：包内（旁边有 bin/）、在线（管道进来时 $0 是 sh，按平台取 release 资源）、
        -- 显式归档或源码。去掉任何一条，从 GitHub 装的用法就断了。
        it "install.sh picks the package by platform and can fall back to source" $ do
            installer <- readUtf8 (".." </> "scripts" </> "install.sh")
            installer `shouldSatisfy` T.isInfixOf "bin/libchusql_core_storage"
            installer `shouldSatisfy` T.isInfixOf "uname -s"
            installer `shouldSatisfy` T.isInfixOf "uname -m"
            installer `shouldSatisfy` T.isInfixOf "releases/latest/download"
            -- 一个平台一个包（web 和 cli 都在里面那一个归档里），按平台标签找，没有组件名那一层
            installer `shouldSatisfy` T.isInfixOf "chusql-$label.$ext"
            installer `shouldSatisfy` T.isInfixOf "chusql.$ext"
            -- 释放按这次的选择走：没选的组件一个文件都不落地
            installer `shouldSatisfy` T.isInfixOf "cp \"$pack/bin/csql\""
            installer `shouldSatisfy` T.isInfixOf ".sha256"
            installer `shouldSatisfy` T.isInfixOf "--url"
            installer `shouldSatisfy` T.isInfixOf "--from-source"
            installer `shouldSatisfy` T.isInfixOf "stack build --fast"
            -- 版本可以指定、也可以先列出来：latest 先去 GitHub API 里挑「最新正式版且带本平台包」，
            -- 自建站点（GHE / 镜像 / 本地夹具）走 /api/v3，取包也换同一套基地址。
            installer `shouldSatisfy` T.isInfixOf "--list-versions"
            installer `shouldSatisfy` T.isInfixOf "api/v3"
            installer `shouldSatisfy` T.isInfixOf "CHUSQL_GITHUB_BASE"
            -- 版本接口未认证只有 60 次/小时，两个脚本都要能用 token 抬高限额
            installer `shouldSatisfy` T.isInfixOf "CHUSQL_GITHUB_TOKEN"
        -- Windows 侧要跟 install.sh 对齐：旁边没有包就去发行站下 zip，版本可指定、可列出来，
        -- 自建站点同样靠 CHUSQL_GITHUB_BASE 换基地址（api/v3 + 下载路径一起换）。
        it "install.ps1 downloads the package, picks a version and can list versions" $ do
            installer <- readUtf8 (".." </> "scripts" </> "install.ps1")
            installer `shouldSatisfy` T.isInfixOf "bin\\chusql_core_storage.dll"
            -- 与 install.sh 同一套命名：一个平台一个包，没有组件名那一层
            installer `shouldSatisfy` T.isInfixOf "chusql-$label.zip"
            installer `shouldSatisfy` T.isInfixOf "'chusql.zip'"
            installer `shouldSatisfy` T.isInfixOf "releases/latest/download"
            installer `shouldSatisfy` T.isInfixOf "-Version"
            installer `shouldSatisfy` T.isInfixOf "-ListVersions"
            installer `shouldSatisfy` T.isInfixOf ".sha256"
            installer `shouldSatisfy` T.isInfixOf "api/v3"
            installer `shouldSatisfy` T.isInfixOf "CHUSQL_GITHUB_BASE"
            -- 版本接口未认证只有 60 次/小时，两个脚本都要能用 token 抬高限额
            installer `shouldSatisfy` T.isInfixOf "CHUSQL_GITHUB_TOKEN"
            installer `shouldSatisfy` T.isInfixOf "Expand-Archive"
            -- PowerShell 7 的两个坑：Content 可能是字节数组（.sha256 会变成一串数字），
            -- 函数 return 数组会被折成「装数组的单个对象」（版本挑选会拿到整张 tag 表）。
            -- 两处都要绕开，否则校验和版本选择会静默出错。
            installer `shouldSatisfy` T.isInfixOf "Get-ResponseText"
            installer `shouldSatisfy` T.isInfixOf "-is [byte[]]"
            installer `shouldSatisfy` T.isInfixOf "ConvertFrom-Json"
        -- 装什么由人当场选：逐个问、问答用英文、默认装 csql 不装 Web。管理员口令不在配置里，
        -- 装机时是必答项：有终端就当场问两遍，没有就给 --password / -Password 或
        -- CHUSQL_ADMIN_PASSWORD，口令只经 stdin 传给 csql-bootstrap。两个脚本必须一致。
        it "both installers ask which components to install, in the same words" $ do
            sh <- readUtf8 (".." </> "scripts" </> "install.sh")
            ps <- readUtf8 (".." </> "scripts" </> "install.ps1")
            let cliAsk = "Install the command line client (csql)? [Y/n]"
                webAsk = "Install the web front end (browser UI)? [y/N]"
            mapM_
                ( \src -> do
                    src `shouldSatisfy` T.isInfixOf cliAsk
                    src `shouldSatisfy` T.isInfixOf webAsk
                    src `shouldSatisfy` T.isInfixOf "The administrator account is "
                    src `shouldSatisfy` T.isInfixOf "  the password cannot be empty"
                    src `shouldSatisfy` T.isInfixOf "the two entries differ, try again"
                    src `shouldSatisfy` T.isInfixOf "--password-stdin"
                    src `shouldSatisfy` T.isInfixOf "CHUSQL_ADMIN_PASSWORD"
                    -- 管理员不能没有口令：两个脚本都不许再留空口令这条路
                    src `shouldSatisfy` (not . T.isInfixOf "passwordless")
                    -- cli 的问题必须出现在 web 之前（文档里的用法示例也按这个顺序写）
                    T.length (fst (T.breakOn cliAsk src)) `shouldSatisfy` (< T.length (fst (T.breakOn webAsk src)))
                )
                [sh, ps]
            sh `shouldSatisfy` T.isInfixOf "--password PASSWORD"
            sh `shouldSatisfy` T.isInfixOf "--interactive"
            sh `shouldSatisfy` T.isInfixOf "both"
            ps `shouldSatisfy` T.isInfixOf "-Password"
            ps `shouldSatisfy` T.isInfixOf "-Interactive"
            ps `shouldSatisfy` T.isInfixOf "both"
            -- 到底装的哪个版本要打出来：latest 解析来的和显式指定的，两处都得有。
            sh `shouldSatisfy` T.isInfixOf "echo \"  version    $version\""
            sh `shouldSatisfy` T.isInfixOf "note \"version    $version\""
            ps `shouldSatisfy` T.isInfixOf "  version    {0}"
        -- 本机打发行包：文件名叫法和 CI 出的资产一致（install.sh / install.ps1 按同一套平台标签取），
        -- 落在 releases/ 下（不入库），并自带 sha256。缺任何一样，自己打的包安装脚本就认不出来。
        it "the packaging scripts write the same asset names as the release pipeline" $ do
            sh <- readUtf8 (".." </> "scripts" </> "package.sh")
            ps <- readUtf8 (".." </> "scripts" </> "package.ps1")
            mapM_
                ( \src -> do
                    src `shouldSatisfy` T.isInfixOf "releases"
                    src `shouldSatisfy` T.isInfixOf "install.sh"
                    src `shouldSatisfy` T.isInfixOf "install.ps1"
                    -- 卸载脚本跟安装脚本同路进包：装完就得能在包根上卸干净
                    src `shouldSatisfy` T.isInfixOf "uninstall.sh"
                    src `shouldSatisfy` T.isInfixOf "uninstall.ps1"
                )
                [sh, ps]
            sh `shouldSatisfy` T.isInfixOf "tar -czf"
            sh `shouldSatisfy` T.isInfixOf "sha256"
            -- 一个平台一个包：名字里不再有组件那一层，web 与 cli 都在同一个归档里，
            -- 装哪个由包内脚本问；脚本自己也要按两种选择各验一遍（没选的不许落地）
            sh `shouldSatisfy` T.isInfixOf "chusql-$platform"
            sh `shouldSatisfy` T.isInfixOf "chusql-web/static"
            sh `shouldSatisfy` T.isInfixOf "bin/csql"
            sh `shouldSatisfy` T.isInfixOf "for comp in cli web"
            -- 打包脚本自己不再有组件开关：--component 只留在包内安装脚本里（给非交互用），
            -- 这里的自检要靠「它自己的选项表里没有组件项」来证明，不能一口咬定全文不出现这三个字。
            sh `shouldSatisfy` T.isInfixOf "try --no-build, --out-dir DIR, --platform LABEL"
            sh `shouldSatisfy` (not . T.isInfixOf "--component)")
            ps `shouldSatisfy` T.isInfixOf "Compress-Archive"
            ps `shouldSatisfy` T.isInfixOf "Get-FileHash"
            ps `shouldSatisfy` T.isInfixOf "chusql-$Platform"
            ps `shouldSatisfy` T.isInfixOf "chusql-web\\static"
            ps `shouldSatisfy` T.isInfixOf "bin\\csql.exe"
            -- 配置模板跟组件目录走：包内目录叫 resources，不再是 scripts
            ps `shouldSatisfy` T.isInfixOf "resources\\settings.toml.windows"
            ps `shouldSatisfy` (not . T.isInfixOf "scripts\\settings.toml")
            sh `shouldSatisfy` T.isInfixOf "resources/settings.toml.linux"
            sh `shouldSatisfy` (not . T.isInfixOf "scripts/settings.toml")
            ps `shouldSatisfy` T.isInfixOf "Test-Installed $name 'cli'"
            ps `shouldSatisfy` (not . T.isInfixOf "$Component")

            ignore <- readUtf8 (".." </> ".gitignore")
            ignore `shouldSatisfy` T.isInfixOf "releases/"
        -- 静态目录固定在安装目录下，启动器必须先切到安装目录；少了这一句，从别处执行
        -- csql-web 就会因为找不到 static/ 直接退出（Windows 的 .ps1 靠 -WorkingDirectory）。
        it "csql-web.sh switches to the install dir before starting" $ do
            launcher <- readUtf8 (".." </> "scripts" </> "csql-web.sh")
            launcher `shouldSatisfy` T.isInfixOf "cd \"$home_dir\""
            launcher `shouldSatisfy` T.isInfixOf "chusql-server"
            -- 日志目录由 [storage] log_files 决定，不再是硬编码的 logs
            launcher `shouldSatisfy` T.isInfixOf "log_files"
        -- 默认数据目录按平台惯例算，两处必须说同一件事：Rust 代码与配置示例。
        -- 模板路径占位符在安装时填入绝对路径。
        it "the default data dir follows the platform, and code and template agree" $ do
            rust <- readUtf8 (".." </> "chusql-core" </> "storage" </> "src" </> "config.rs")
            rust `shouldSatisfy` T.isInfixOf "default_data_dir"
            rust `shouldSatisfy` T.isInfixOf "LOCALAPPDATA"
            rust `shouldSatisfy` T.isInfixOf "XDG_DATA_HOME"
            rust `shouldSatisfy` (not . T.isInfixOf "DEFAULT_DATA_DIR")

            exampleFile <- readUtf8 (".." </> "chusql-core" </> "storage" </> "chusql-core-storage.toml.example")
            -- 示例文件说自己列的值就是默认值：data_dir 不能是能生效的一行
            exampleFile `shouldSatisfy` (not . T.isInfixOf "\ndata_dir =")
            exampleFile `shouldSatisfy` T.isInfixOf "%LOCALAPPDATA%"
            exampleFile `shouldSatisfy` T.isInfixOf "XDG_DATA_HOME"

            template <- readUtf8 (".." </> "resources" </> "settings.toml.linux")
            -- 模板那行是安装时被 sed / -replace 替换的占位，必须保持能生效
            template `shouldSatisfy` T.isInfixOf "\ndata_dir = "
            -- 日志跟数据分家：启动器与安装脚本按 log_files 决定日志落哪个目录
            template `shouldSatisfy` T.isInfixOf "log_files = \"@LOG_DIR@\""
            -- 静态目录与管理员名都不再写进配置：前者写死 static，后者由 --user 给（默认 root）；
            -- 监听地址是活的配置项（[server] listen_host / [web] listen_host），模板里可以留空靠内置默认
            template `shouldSatisfy` (not . T.isInfixOf "static_dir =")
            template `shouldSatisfy` (not . any ((== "user") . T.strip . fst . T.breakOn "=") . T.lines)
            template `shouldSatisfy` T.isInfixOf ("sudo_auth_user = " <> T.pack (show ("" :: String)))
        -- 管道传输退休后，两侧都不该再算套接字路径、也不该再认管名。
        it "neither side computes a socket path or a pipe name any more" $ do
            haskellIpc <- readUtf8 (".." </> "chusql-core" </> "engine" </> "src" </> "ChuSQL" </> "Core" </> "Engine" </> "Storage" </> "IPC.hs")
            haskellIpc `shouldSatisfy` (not . T.isInfixOf "XDG_RUNTIME_DIR")
            haskellIpc `shouldSatisfy` (not . T.isInfixOf ".sock")
            haskellIpc `shouldSatisfy` (not . T.isInfixOf "pipe")
            rustModules <- readUtf8 (".." </> "chusql-core" </> "storage" </> "src" </> "lib.rs")
            rustModules `shouldSatisfy` (not . T.isInfixOf "endpoint")
            storageConfig <- readUtf8 (".." </> "chusql-core" </> "storage" </> "src" </> "config.rs")
            storageConfig `shouldSatisfy` (not . T.isInfixOf "pipe_name")
        it "the Linux pipeline builds both toolchains and runs every test suite" $ do
            pipeline <- readUtf8 (".." </> ".github" </> "workflows" </> "linux.yml")
            pipeline `shouldSatisfy` T.isInfixOf "ubuntu-latest"
            pipeline `shouldSatisfy` T.isInfixOf "cargo test --release"
            pipeline `shouldSatisfy` T.isInfixOf "cargo clippy"
            pipeline `shouldSatisfy` T.isInfixOf "stack test --fast"
            -- 打包与安装冒烟只在发版流水线里做（tag 触发 / 手动 dispatch），这条流水线不再出包
            pipeline `shouldSatisfy` (not . T.isInfixOf "package.ps1")
            pipeline `shouldSatisfy` (not . T.isInfixOf "smoke-linux.sh")
            -- 仓库里没有 rustfmt.toml，历史代码不是按当前 rustfmt 排的：格式化不是门禁
            pipeline `shouldSatisfy` (not . T.isInfixOf "cargo fmt")
        -- 装的人不该被要求装两个工具链：发版流水线出预编译包，install.sh 按同一套平台标签去取。
        -- 流水线里装配与打归档是直接写的（两个平台各一段）；本机要打同一个包用 scripts/ 下的
        -- package.sh 与它的 PowerShell 版，两边的布局必须一致。
        it "the release pipeline publishes one archive per platform, tagged the way install.sh looks it up" $ do
            release <- readUtf8 (".." </> ".github" </> "workflows" </> "release.yml")
            release `shouldSatisfy` T.isInfixOf "linux-x86_64"
            release `shouldSatisfy` T.isInfixOf "macos-arm64"
            release `shouldSatisfy` T.isInfixOf "macos-x86_64"
            release `shouldSatisfy` T.isInfixOf "windows-x86_64"
            release `shouldSatisfy` T.isInfixOf "cargo build --release"
            release `shouldSatisfy` T.isInfixOf "stack build --fast chusql-web:exe:chusql-web"
            release `shouldSatisfy` T.isInfixOf "stack build --fast chusql-cli:exe:csql"
            release `shouldSatisfy` T.isInfixOf "tar -czf"
            release `shouldSatisfy` T.isInfixOf "Compress-Archive"
            release `shouldSatisfy` T.isInfixOf "scripts/install.sh"
            release `shouldSatisfy` (not . T.isInfixOf "package.ps1")
            release `shouldSatisfy` T.isInfixOf "sha256"
            release `shouldSatisfy` T.isInfixOf "gh release upload"
            -- 四个平台各一个包，名字里没有组件那一层：一个包里 web 与 cli 都有；
            -- 矩阵里也不该再出现 web / cli 这一维（那样又会产出两个按组件命名的资产）
            release `shouldSatisfy` T.isInfixOf "name: ${{ matrix.platform }}"
            release `shouldSatisfy` T.isInfixOf "name=\"chusql-${{ matrix.platform }}\""
            release `shouldSatisfy` T.isInfixOf "name = \"chusql-${{ matrix.platform }}\""
            release `shouldSatisfy` (not . T.isInfixOf "matrix.component")
            release `shouldSatisfy` (not . T.isInfixOf "component: [web, cli]")
        it "no layer looks for a storage executable or a pipe name any more" $ do
            engine <- readUtf8 (".." </> "chusql-core" </> "engine" </> "test" </> "Spec.hs")
            bench <- readUtf8 (".." </> "benchmark" </> "src" </> "Main.hs")
            template <- readUtf8 (".." </> "resources" </> "settings.toml.linux")
            mapM_
                (\src -> src `shouldSatisfy` (not . T.isInfixOf "pipe_name"))
                [engine, bench, template]
            mapM_
                (\src -> src `shouldSatisfy` (not . T.isInfixOf "chusql-storage.exe"))
                [engine, bench]
            -- 起存储进程那一整个模块已经删掉，别再长回来
            moduleGone <-
                doesFileExist
                    (".." </> "chusql-server" </> "src" </> "ChuSQL" </> "Server" </> "StorageProcess.hs")
            moduleGone `shouldBe` False

    -- 文档只做结构约束：四篇在、README 能链到它们。正文怎么写、写多长都不校验，
    -- 免得以后精简文档反而过不了门禁。
    describe "Documentation (docs/)" $ do
        it "the README links the split documents" $ do
            readme <- readUtf8 (".." </> "README.md")
            mapM_
                (\target -> readme `shouldSatisfy` T.isInfixOf ("docs/" <> target <> ".md"))
                ["install", "config", "commands", "architecture"]
        it "the split documents exist and are not empty" $ do
            mapM_
                ( \name -> do
                    doc <- readUtf8 (".." </> "docs" </> name <> ".md")
                    doc `shouldSatisfy` (not . T.null)
                )
                ["install", "config", "commands", "architecture"]
