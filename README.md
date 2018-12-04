# jAMM: A Suite for Mediation Models

jamovi Advanced Mediation Models 
version 0.0.7

# Docs and help

Please visit the [jAMM docs page](https://mcfanda.github.io/jamm_docs/index.html)

# State of the art

* It makes the path model
* It shows interactions in the path models
* It makes checks for coherence of the model
* It makes suggestions toward more coherent models
* Automatically guesses which mediation model is more likely to be needed
* Custom models from UI
* It estimates the model coefficients
* Simple mediational effects (estimated at different levels of moderators)
 

# Not implemented

* Interactions among moderators of moderators (not really high priority)
* Rigorous checks for wired and complex models 

# Install

Currently, jAMM can only being installed via R (or Rstudio and equivelant software). First, you need to install R developing tools from CRAN and jamovi developing tools.

```r

install.packages("devtools",repos='http://cran.rstudio.com/')

install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

```

Then you download jAMM module from this repository and install it

```r
devtools::install_github("mcfanda/jamm")
pkg<-paste0(.libPaths()[[1]],"jamm")
jmvtools::install(pkg=pkg)

```


