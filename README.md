# jAMM: A Suite for Mediation Models

jamovi Advanced Mediation Models 
version 0.0.2

# State of the art

* It does the path model
* Shows interactions in the path models
* It makes checks for coherence of the model
* It makes suggestions toward more coherent models
* Automatically guess which mediation model is more likely to be needed
* Custom models from UI
* It estimates the model coefficients
 

# Not implemented

* Simple mediational effects (estimated at different levels of moderators)
* Interactions among moderators of moderators (not really high priority)
* Rigorous checks for weired and complex models 

# Install

Currently, jAMM can only being installed via R (Rstudio or equivelant software). First, you need to install R developing tools from CRAN and jamovi developing tools.

```r

install.packages("devtools",repos='http://cran.rstudio.com/')

install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))

```

Then you download jAMM module from this repository and install it

```r

devtools::install_github("mcfanda/jamm")
jmvtools::jmvtools::install()

```

If you are on linux and you are working with flatpak jamovi, you need to issue 

```r
devtools::install_github("mcfanda/jamm")
jmvtools::jmvtools::install(home="flatpak")

```


