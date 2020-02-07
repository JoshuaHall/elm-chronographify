#!/bin/sh

set -e

js="elm.js"
minJs="elm.min.js"

elm make --optimize --output=$js $@

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=$minJs

echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $minJs | wc -c) bytes  ($minJs)"
echo "Gzipped size: $(cat $minJs | gzip -c | wc -c) bytes"

echo ""
echo "Optimizing CSS..."
css="css/styles.css"
minCss="css/styles.min.css"

postcss $css > $minCss

echo "Compiled size:$(cat $css | wc -c) bytes  ($css)"
echo "Minified size:$(cat $minCss | wc -c) bytes  ($minCss)"
echo "Gzipped size: $(cat $minCss | gzip -c | wc -c) bytes"