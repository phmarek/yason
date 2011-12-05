#!/bin/sh

xmllint --noout doc.xml
[ -f clixdoc.xsl ] || wget -q https://raw.github.com/hanshuebner/clixdoc/master/clixdoc.xsl
xsltproc -o doc.html clixdoc.xsl doc.xml 