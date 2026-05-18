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

`--tikz` принимает ровно один аргумент `REGEX` и печатает standalone `.tex`:

```bash
cabal run -v0 glushkov-algo -- --tikz "(a|b)*abb" > /tmp/dfa.tex
pdflatex -interaction=nonstopmode -halt-on-error -output-directory /tmp /tmp/dfa.tex
```

Если нужен только фрагмент `tikzpicture`, используйте `--tikz-snippet`. Этот режим тоже принимает только `REGEX`, без слова для проверки:

```bash
cabal run -v0 glushkov-algo -- --tikz-snippet "(a|b)*abb"
```

## Скрипт рендера

Для быстрой сборки `.tex` и `.pdf` есть небольшой shell-скрипт:

```bash
scripts/render-regex.sh "(a|b)*abb"
```

По умолчанию он создаёт `generated/dfa.tex` и `generated/dfa.pdf` при наличии `pdflatex`. PNG создаётся как `generated/dfa.png`, только если найден один из конвертеров: `magick`, `pdftoppm`, `qlmanage` или `sips`. Если конвертера нет, скрипт не падает и просто оставляет готовый PDF.

## Генерация картинки в репозиторий

```bash
scripts/render-regex.sh "(a|b)*abb"
scripts/render-regex.sh "(a|b)*abb" generated/abb_dfa
```

Скрипт создаёт `.tex`, `.pdf` и, если возможно, PNG высокого разрешения. Для качественного PNG лучше иметь ImageMagick (`magick`) или `pdftoppm`; на macOS также подходит `qlmanage`. `sips` используется только как последний запасной вариант и может давать более низкое качество.

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
- `--tikz REGEX` — standalone `.tex`;
- `--tikz-snippet REGEX` — только `tikzpicture`;
- `--match REGEX WORD` — `accept` или `reject`.

Режимы `--ast`, `--attrs`, `--nfa`, `--dfa`, `--tikz` и `--tikz-snippet` принимают ровно один аргумент `REGEX`. Для проверки слова нужен именно `--match REGEX WORD`.

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
scripts/render-regex.sh
generated/README.md
report/report.tex
examples/
```
