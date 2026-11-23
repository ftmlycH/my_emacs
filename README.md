# 只是一个 emacs 的配置而已。

原生 Emacs 的精简配置：只加语法高亮和按语言缩进，不引入整套框架。

| 文件 | 作用 |
| --- | --- |
| `early-init.el` | 启动早期设置：GC 阈值、TTY 透明背景的处理 |
| `init.el` | 主配置：编码、缩进、高亮、配色、括号彩虹 |
| `langs.el` | 各语言 major mode 的挂载与关键词补充高亮 |
| `chinese-help.el` | 中文帮助信息 |
| `install-packages.el` | 一次性安装依赖包 |

## 使用

把本仓库放到 `~/.emacs.d`（Windows 为 `%APPDATA%\\.emacs.d`）后启动 Emacs。
`elpa/`、`package-quickstart.el` 等生成内容不入库，重建方式：

```
emacs -nw --batch -l install-packages.el
```
