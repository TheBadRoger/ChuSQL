# README 徽章

所有徽章都是本地 SVG，采用统一的深色标签、彩色值区和圆角样式。

执行 `pwsh -File scripts/update-code-lines.ps1` 可重新生成全部徽章。代码行数包含代码、测试、注释和空行，排除 Git 忽略的构建工件；行数与提交编号是生成时的本地快照。

发布徽章链接 GitHub 版本列表。构建、测试和门禁徽章提供实时状态页面入口，本地图片显示 `view status` 或 `manual status`，不保存或宣称实时通过状态。门禁由所有者手动触发。
