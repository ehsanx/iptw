# iptw
### R package for calculating Inverse Probability of Treatment Weights
* This package calculates various measures of association and helps understand and visualize the link between causal models such as Marginal Structural Models and Standerdized measures.

## Installation
```R
library(devtools)
install_github("ehsanx/iptw")
```

## Loading the package
```R
require(iptw)
```

## Pulling the help file
```R
help(package="iptw")
```

## Using this package 
```R
r1 <- .9 # Risk of the treated subjects
r0 <- .5 # Risk of the untreated subjects
measures.calc(r1,r0)
# Calculates risk measures

form.table.object <- form.table(Y1A1L1=150, 
                                Y1A0L1=45, 
                                Y1A1L0=20, 
                                Y1A0L0=5, 
                                Y0A1L1=300, 
                                Y0A0L1=10, 
                                Y0A1L0=40, 
                                Y0A0L0=55)
form.table.object
# Forms the crude, stratified tables suitable 
# for using in other functions of this package.

ungrouped.data.object <- ungrouped.data(form.table.object)
# Creates ungrouped data suitable for performing regression on it
head(ungrouped.data.object)

associational(form.table.object) 
# Calculates measures of associations
standardization(form.table.object) 
# Calculates Standardized measures
iptw(form.table.object, type = "sw") 
# Standardized measures are calculated by this function 
# using inverse probability of treatment weights (IPTW).

iptw.regression(form.table.object, type = "sw") 
# Calculates IPTW estimates from regression
# requires grid and survey package

graph(form.table.object, type = "sw")
# Maps probabilities and frequesncies
```
### Author 
* Ehsan Karim :octocat: Feel free to [report](http://www.ehsankarim.com/) any errors / update suggestions. 

### Useful Reference
- [x] Hernán MA, Robins JM (2016). [Causal Inference](https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/). Boca Raton: Chapman & Hall/CRC, forthcoming.

### Related web-Apps
- [x] [Causal Inference Web-Apps](http://www.ehsankarim.com/software/webapps)
