#!/bin/sh

# To make this script work on Ubuntu, run this command to install required packages:
# apt install xsltproc libxml2-utils

xmllint --noout doc.xml
[ -f clixdoc.xsl ] || wget -q https://raw.github.com/hanshuebner/clixdoc/master/clixdoc.xsl
xsltproc --stringparam current-release `perl -ne 'if (/^ *:version +"(.*)"/) { print "$1\n" }' yason.asd` -o index.html clixdoc.xsl doc.xml 
