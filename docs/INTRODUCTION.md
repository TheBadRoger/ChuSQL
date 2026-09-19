# 实现要点

完整的实现原理相对复杂，故不多赘述，这里阐述一些关键的实现要点

## Haskell -- 处理引擎

### 词法分析

词法层与语法层同处 `chusql-engine/src/ChuSQL/Syntax/Parser.hs`，基于 **megaparsec** 实现。解析器类型为

```haskell
type Parser = Parsec Void String
```

即输入是 `String`，词法错误类型取 `Void`（不自定义错误类型），失败信息统一由 `errorBundlePretty` 渲染成人可读的报文。

#### 空白与词法单元骨架

| 组合子   | 定义                           | 作用                                 |
| -------- | ------------------------------ | ------------------------------------ |
| `sc`     | `Lxr.space space1 empty empty` | 跳过空白（含换行）；暂不解析注释     |
| `lexeme` | `Lxr.lexeme sc`                | 解析一个词法单元，并吃掉其后的空白   |
| `symbol` | `Lxr.symbol sc`                | 固定符号 `*` `,` `=` `>` `<` `(` `)` |

所有词法单元都从 `lexeme` 出口，游标返回时必定停在下一个 token 的起始位置，因此语法层不需要感知空白。

#### 已实现的 token

| token    | 组合子       | 词法规则                                   | 例                       |
| -------- | ------------ | ------------------------------------------ | ------------------------ |
| 关键字   | `keyword`    | 大小写不敏感；其后不能紧跟标识符字符       | `SELECT` / `select`      |
| 标识符   | `identifier` | 首字符为字母或 `_`，其后为字母、数字或 `_` | `users`、`user1`、`_tmp` |
| 整数     | `integer`    | 十进制，委托 `Lxr.decimal`                 | `18`                     |
| 字符串   | `stringLit`  | 单引号包裹，遵循标准 SQL 转义              | `'Alice'`                |
| 符号     | `symbol`     | 字面匹配                                   | `>`、`(`                 |
| 排序方向 | `sortDir`    | `ASC` / `DESC`，缺省 `Asc`                 | `DESC`                   |

目前已接入语法的关键字：`SELECT`、`FROM`、`WHERE`、`ORDER`、`BY`、`LIMIT`、`AND`、`OR`、`ASC`、`DESC`、`INSERT`、`INTO`、`VALUES`、`DELETE`、`UPDATE`、`SET`。

#### 三个关键实现点

**1. 关键字要防前缀**

`keyword` 匹配完关键字后追加 `notFollowedBy (satisfy isIdentChar)`，否则 `selection` 会被读成 `SELECT`、`fromage` 会被读成 `FROM`。

**2. 关键字必须可回溯（`try`）**

```haskell
keyword k = lexeme $ try $ do
    _ <- string' k
    notFollowedBy (satisfy isIdentChar)
```

`string'` 大小写不敏感，匹配 `ORDER` 时会先吃掉前两个字符 `OR`；紧接着的前缀守卫因为遇到 `D` 而失败，此时输入已被消耗，这个失败就成了**已消耗失败**，调用方的 `many`（`AND` / `OR` 运算符层）不会回溯而是直接报错：

```text
SELECT name FROM users WHERE age > 18 ORDER BY age DESC
                                              ^ unexpected 'D'
```

把整个 `keyword` 用 `try` 包住，失败即复位，`ORDER BY` 才能被后续子句正常识别。这同时消掉了一整类隐患：任何以 `AND` / `OR` 为前缀的子句关键字跟在表达式后面都不会再炸。

**3. 字符串字面量不能用 `manyTill`**

`manyTill p end` 每一步都先尝试结束符，`'It''s ok'` 会在 `''` 的第一个引号处提前收尾。正确做法是先读内容、再吃结尾引号：

```haskell
stringLit = lexeme $ do
    _ <- char '\''
    s <- many stringChar
    _ <- char '\''
    return s

stringChar = choice [ '\'' <$ try (string "''"), satisfy (/= '\'') ]
```

`stringChar` 的两个分支在失败时都不消耗输入，因此可以安全地放进 `many`。

#### 字符串字面量与标准 SQL 转义

| 写法      | 含义                                                    |
| --------- | ------------------------------------------------------- |
| `'it''s'` | `''` 折叠为一个单引号，值为 `it's`                      |
| `'a\b'`   | 反斜杠是普通字符，值为 `a\b`（**不**做 `\n`/`\t` 转义） |
| `''`      | 空字符串                                                |
| `''''`    | 只含一个单引号的字符串                                  |
| `'oops`   | 未闭合，解析失败                                        |
