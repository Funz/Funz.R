#!/bin/bash
# This will populate the direcotry inst/java with Funz java sources.
unzip -o ../Funz/lib/funz-core-*.jar "*.java"
unzip -o ../Funz/lib/funz-calculator-*.jar "*.java"
unzip -o ../Funz/lib/funz-client-*.jar "*.java"
