## R CMD check results
There were no ERRORs or WARNINGs. 

There was 3 NOTEs:

* checking installed package size ... NOTE
  installed size is 26.7Mb
  sub-directories of 1Mb or more:
    Funz  26.5Mb
    
The purpose of this package is to provide a binding to Funz, included here as a third party dependency
  
* checking R code for possible problems ... NOTE
Funz_Design: no visible binding for '<<-' assignment to
...

All these global assignemnts are included in the inst/Funz/Funz.R file, provided as third-party (so taken, as-is).

* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: ‘rJava’
  All declared Imports should be used.
  
rJava is loaded inside the third-party Funz.R file only, so it is a real pre-requisite for Funz.

