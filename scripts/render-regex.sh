#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat >&2 <<'USAGE'
Usage:
  scripts/render-regex.sh REGEX [OUTPUT_PREFIX]

Creates:
  generated/dfa.tex
  generated/dfa.pdf
  generated/dfa.png  if magick, pdftoppm, qlmanage, or sips is available

Examples:
  scripts/render-regex.sh "(a|b)*abb"
  scripts/render-regex.sh "(a|b)*abb" generated/abb_dfa
USAGE
}

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage
  exit 2
fi

regex=$1
output_prefix=${2:-generated/dfa}
output_dir=$(dirname "$output_prefix")
tex_file="${output_prefix}.tex"
pdf_file="${output_prefix}.pdf"
png_file="${output_prefix}.png"

if ! command -v pdflatex >/dev/null 2>&1; then
  echo "render-regex: pdflatex is required to build ${pdf_file}." >&2
  exit 1
fi

mkdir -p "$output_dir"

cabal run -v0 glushkov-algo -- --tikz "$regex" > "$tex_file"
echo "Wrote ${tex_file}"

pdflatex \
  -interaction=nonstopmode \
  -halt-on-error \
  -output-directory "$output_dir" \
  "$tex_file" >/dev/null
echo "Wrote ${pdf_file}"

if command -v magick >/dev/null 2>&1; then
  if magick -density 400 "${pdf_file}[0]" -quality 100 "$png_file"; then
    echo "Wrote ${png_file}"
  else
    echo "PDF created, PNG skipped: magick could not convert the file." >&2
  fi
elif command -v pdftoppm >/dev/null 2>&1; then
  if pdftoppm -png -r 400 -singlefile "$pdf_file" "$output_prefix"; then
    echo "Wrote ${png_file}"
  else
    echo "PDF created, PNG skipped: pdftoppm could not convert the file." >&2
  fi
elif command -v qlmanage >/dev/null 2>&1; then
  quicklook_png="${output_dir}/$(basename "$pdf_file").png"
  rm -f "$quicklook_png"
  if qlmanage -t -s 2400 -o "$output_dir" "$pdf_file" >/dev/null 2>&1 && [[ -f "$quicklook_png" ]]; then
    mv "$quicklook_png" "$png_file"
    echo "Wrote ${png_file}"
  else
    echo "PDF created, PNG skipped: qlmanage could not convert the file." >&2
  fi
elif command -v sips >/dev/null 2>&1; then
  if sips -s format png "$pdf_file" --out "$png_file" >/dev/null; then
    echo "Wrote ${png_file}"
  else
    echo "PDF created, PNG skipped: sips could not convert the file." >&2
  fi
else
  echo "PDF created, PNG skipped: no magick, pdftoppm, qlmanage, or sips found."
fi
