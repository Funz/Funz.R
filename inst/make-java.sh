#!/bin/bash
# This will populate the direcotry inst/java with Funz java sources.
mkdir java
cd java
unzip -o ../Funz/lib/funz-core-*.jar
unzip -o ../Funz/lib/funz-calculator-*.jar
unzip -o ../Funz/lib/funz-client-*.jar
rm -rf META-INF
find . -name *.class -exec rm {} \;
