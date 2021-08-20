---
title: "Funz from R"
author: "Y. Richet"
date: "30/07/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup}
# requirements
libs = c("jsonlite","scatterplot3d")
for (l in libs) {
  if (!(l %in% installed.packages()))
    install.packages(l)
}
```


## Install

Just use the standard 'install.packages("Funz")' command:
```{r setup}
# install Funz if needed
if (!("Funz" %in% installed.packages()))
    install.packages("Funz")

library(Funz)
install.Design("GradientDescent") # required for later example
```

## Usage: Funz from R console

Once installed, Funz R package allows to access almost all features of Funz through command line.

#### Starting calculations back-end

It is mandatory to launch calculation back-end which will be used to perform parametric calculations, later.

___Note: it is also possible to start this back-end on another computer/server/cluster, which will be usable by all computer which IP is declared in 'calculator.xml' file (by default, it just contains "127.0.0.1" local address).___

```{r}
# This will start 5 calculators, in background
calcs = startCalculators(5)
```

You can check all available calculators from your computer using:
```{r}
Grid()
```


### Parametric modelling

This main feature of Funz allows to evaluate a parametric model, built from parameterized files (like following 'branin.R' file including variables starting with a reserved character '?'):
```{r echo=F, comment=''}
cat(readLines(file.path(Funz:::FUNZ_HOME,"samples","branin.R")), sep = '\n')
```
___Note: usually, a parametric model is based on heavy simulation software, not callable easily like a function. In practice, this example with an R function may be easier to evaluate directly, of course.___

Once calculators (eg. started from back-end) are available, you can launch this parametric model for given variables (x1 and x2) values:
```{r results=F}
Run(model = "R",
    input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"),
    input.variables = list(x1=seq(0,1,by=0.1),x2=seq(0,1,by=0.1)),
    all.combinations = TRUE,
    output.expressions = "z")
```

... get and display results (using the '.Funz.Last.run' global variable, if 'Run()' was not assigned):
```{r}
r = .Funz.Last.run$results

head( data.frame(lapply(r,cbind)) )#[,c('state','calc','path','x1','x2','z')]
```

... or plot model response surface :
```{r}
# Response surface of previous Run
persp(z=matrix(unlist(r$z),nrow=sqrt(length(unlist(r$z))),byrow = F),xlab="x1",yla="x2",zlab="z")
```


### Applying algorithm on function

The other main feature of Funz consists in applying an algorithm/analysis on a function:

```{r results=F}
branin <- function(x) {
	x1 <- x[,1]*15-5   
	x2 <- x[,2]*15     
	(x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10
}

Design(fun = branin,
       design = "GradientDescent", options = list(max_iterations=15),
       input.variables = list(x1="[0,1]",x2="[0,1]"))
```

which solves the targeted issue (here an optimization):

```{r}
x = seq(0,1,,41)
contour(x,x,matrix(branin(expand.grid(x,x)),ncol=length(x)),nlevels = 31,xlab="x1",ylab="x2")

d = .Funz.Last.design$results
argmin = jsonlite::fromJSON(d$analysis.argmin)
points(argmin[1],argmin[2],col='red')
```


### Applying algorithm on parametric modelling

These two main features may also be coupled to apply an algorithm directly on the parametric model:

```{r results=F}
RunDesign(model = "R",
          input.files = file.path(Funz:::FUNZ_HOME,"samples","branin.R"),
          design = "GradientDescent", design.options = list(max_iterations=15),
          input.variables = list(x1="[0,1]",x2="[0,1]"),
          output.expressions = "z")
```

... and returns the algorithm analysis:

```{r}
x = seq(0,1,,41)
xx = expand.grid(x,x)
p3d = scatterplot3d::scatterplot3d(xx[,1],xx[,2],branin(xx), 
                                   xlab="x1",ylab="x2",zlab="z",color='gray',zlim = c(0,350))
# display assumed unknown response surface
contours = contourLines(x,x,matrix(branin(xx),ncol=length(x)),nlevels = 31)
for (l in contours) {
   lines(p3d$xyz.convert(l$x,l$y,rep(350,length(l$x))),col='gray')
}

rd = .Funz.Last.rundesign$results
# plot all evaluated points
p3d$points3d(unlist(rd$x1[[1]]), unlist(rd$x2[[1]]), unlist(rd$z[[1]]), 
             col='blue',pch=20)
# plot min/argmin searched
argmin = rd$analysis.argmin[[1]]
min = rd$analysis.min[[1]]
p3d$points3d(argmin[1],argmin[2],min,
             col='red',pch=20)
lines(p3d$xyz.convert(c(argmin[1],argmin[1]),c(argmin[2],argmin[2]),c(0,350)),col='red')
```

Once finished, it is recommended to shutdown calculators in back-end:

```{r}
# This will stop the 5 calculators started earlier
stopCalculators(calcs)
```

## Setup algorithms & models

After a fresh install of Funz, is is commonplace to add useful models or algorithms.
Such a plugin is a 'zip' file, which may be installed locally, or directly from GitHub Fuz repository.

### Install new model

Get already installed models:

```{r}
installed.Models()
```

Get available models from GitHub:

```{r}
available.Models()
```

Install a new model from GitHub:

```{r}
install.Model("Modelica")
```

... or from a local file:

```{r eval=F}
install_file.Model("plugin-Modelica.zip")
```

### Install new algorithm


Get already installed models:

```{r}
installed.Designs()
```

Get available models from GitHub:

```{r}
available.Designs()
```

Install a new model from GitHub:

```{r}
install.Design("Brent")
```

... or from a local file:

```{r eval=F}
install_file.Design("algorthm-Brent.zip")
```
