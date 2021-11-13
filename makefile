test-chibi:
	chibi-scheme -m '(arvyy mustache-test)' -e '(run-tests)'

test-gauche:
	gosh -I . -u 'arvyy.mustache-test' -e '(begin (run-tests) (exit))' -b

build-doc:
	pandoc -f markdown -t html5 -o readme.html readme.md

package:
	snow-chibi package --version=1.0.2 --authors="Arvydas Silanskas" \
		--maintainers="Arvydas Silanskas <nma.arvydas.silanskas@gmail.com>" --doc=readme.html \
		--description="Mustache templating 1.2.1 implementation" \
		--license="mit" \
		--test-library='(arvyy mustache-test)' \
		arvyy/mustache.sld arvyy/mustache/*.sld
