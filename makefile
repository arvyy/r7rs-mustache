test-chibi:
	cd test && \
	chibi-scheme -I .. mustache-test.scm

test-gauche:
	cd test && \
	gosh -I .. mustache-test.scm

build-doc:
	pandoc -f markdown -t html5 -o readme.html readme.md

package:
	snow-chibi package --version=1.0 --authors="Arvydas Silanskas" \
		--maintainers="Arvydas Silanskas <nma.arvydas.silanskas@gmail.com>" --doc=readme.html \
		--description="Mustache templating 1.2.1 implementation" \
		--license="mit" \
		--test=test/mustache-test.scm \
		arvyy/mustache.sld arvyy/mustache/*.sld
