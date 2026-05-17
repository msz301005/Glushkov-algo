# Examples

Небольшие входные выражения для ручной проверки генерации TikZ.

```bash
cabal run -v0 glushkov-algo -- --tikz "$(cat examples/abb.regex)" > /tmp/abb.tex
cabal run -v0 glushkov-algo -- --tikz "$(cat examples/a_star.regex)" > /tmp/a-star.tex
cabal run -v0 glushkov-algo -- --tikz "$(cat examples/literal_star.regex)" > /tmp/literal-star.tex
```

PDF можно собрать обычным `pdflatex`:

```bash
pdflatex -interaction=nonstopmode -halt-on-error -output-directory /tmp /tmp/abb.tex
```
