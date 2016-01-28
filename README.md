[![Build Status](https://travis-ci.org/wolski/quantable.svg?branch=master)](https://travis-ci.org/wolski/quantable)
[![Project Stats](https://www.ohloh.net/p/quantable/widgets/project_thin_badge.gif)](https://www.ohloh.net/p/quantable)

## Streamline descriptive analysis of quantitative data.

Overwiev

- visualization and transformation of numeric dataframes and matrices


## How to install:
for CRAN version

```r
install.packages("quantable")
```

Please note that the CRAN version might be heavily outdated. 
This is because of, I find rather developer unfriendly, the [CRAN repository policies](http://CRAN.R-project.org/web/packages/policies.html) :

  Submitting updates should be done responsibly and with respect for the
  volunteers' time. Once a package is established (which may take
  several rounds), “no more than every 1–2 months” seems appropriate.


So for an up to date version please do: 

```r
install.packages("devtools")
library(devtools)
install_github("protViz/quantable")
```

### for developers

downlod git repo. Use roxygenize2 to document new functions. Than run these 2 commands to update namespace and Rd files:

```r
library(devtools)
document()
```

