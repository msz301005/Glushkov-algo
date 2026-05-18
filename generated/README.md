# Generated Diagrams

Эта папка предназначена для сгенерированных диаграмм автоматов: `.tex`, `.pdf`
и, если доступен конвертер, `.png`.

Пример регенерации:

```bash
scripts/render-regex.sh "(a|b)*abb" generated/abb_dfa
```

Качество PNG зависит от доступного конвертера. Лучше всего подходят
ImageMagick (`magick`) или `pdftoppm`; macOS Quick Look (`qlmanage`) тоже дает
нормальное высокое разрешение. `sips` используется только как последний
запасной вариант.
