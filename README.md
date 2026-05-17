# glushkov-algo

`glushkov-algo` — учебный Haskell-проект по курсу ТРЯП. Программа принимает регулярное выражение в небольшом формальном синтаксисе, строит по нему ДКА через алгоритм Глушкова и выводит LaTeX/TikZ-код диаграммы.

Это не реализация PCRE/POSIX regex. Синтаксис намеренно ограничен, чтобы были видны этапы: парсинг, линеаризация, атрибуты, позиционный автомат, детерминизация и рендер.

## Возможности

- разбор регулярных выражений через Parsec;
- раскрытие сахара `+` и `?`;
- линеаризация повторяющихся литералов;
- вычисление `nullable`, `first`, `last`, `follow`;
- построение позиционного автомата Глушкова;
- детерминизация достижимых подмножеств;
- проверка слов через DFA;
- генерация standalone LaTeX/TikZ-документа.

## Требования

- GHC;
- Cabal;
- `pdflatex` только для сборки PDF из `.tex`.

На macOS можно использовать BasicTeX. Если минимальной установки не хватает, обычно нужны такие пакеты:

```bash
tlmgr init-usertree
tlmgr --usermode install babel-russian cyrillic cm-super lh standalone pgf enumitem
```

## Сборка

```bash
cabal build
```

## Тесты

```bash
cabal test
```

## Быстрый пример

```bash
cabal run -v0 glushkov-algo -- --match "(a|b)*abb" "aabb"
cabal run -v0 glushkov-algo -- --match "(a|b)*abb" "aba"
```

Ожидаемый результат:

```text
accept
reject
```

## Генерация TikZ

```bash
cabal run -v0 glushkov-algo -- --tikz "(a|b)*abb" > /tmp/dfa.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory /tmp /tmp/dfa.tex
```

Если нужен только фрагмент `tikzpicture`:

```bash
cabal run -v0 glushkov-algo -- --tikz-snippet "(a|b)*abb"
```

## Отчет

```bash
pdflatex -interaction=nonstopmode -halt-on-error -output-directory report report/report.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory report report/report.tex
```

Файл отчета: `report/report.tex`. Собранный PDF: `report/report.pdf`.

## Диагностические режимы

- `--ast REGEX` — нормализованный AST;
- `--attrs REGEX` — позиции и атрибуты Глушкова;
- `--nfa REGEX` — позиционный автомат;
- `--dfa REGEX` — детерминированный автомат;
- `--match REGEX WORD` — `accept` или `reject`;
- `--tikz REGEX` — standalone `.tex`;
- `--tikz-snippet REGEX` — только `tikzpicture`.

## Важное про shell

Регулярное выражение лучше брать в обычные прямые кавычки `"..."`, иначе shell может перехватить `|`, `*`, `?` или скобки раньше программы.

После имени executable нужен разделитель `--`: всё, что стоит после него, передается программе, а не Cabal.

Экранированная звездочка как литерал:

```bash
cabal run -v0 glushkov-algo -- --match "\\*" "*"
```

## Структура проекта

```text
app/Main.hs
src/Regex/Syntax.hs
src/Regex/Parser.hs
src/Regex/Linearize.hs
src/Automata/Glushkov.hs
src/Automata/DFA.hs
src/Render/Tikz.hs
test/Main.hs
report/report.tex
examples/
```
