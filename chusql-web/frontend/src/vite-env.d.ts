/// <reference types="vite/client" />

// Vite 环境类型声明：客户端环境变量与 CSS Modules 模块。

declare module '*.module.css' {
  const classes: Record<string, string>;
  export default classes;
}
