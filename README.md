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
r1 <- .9
r0 <- .5
measures.calc(r1,r0)
form.table.object <- form.table(Y1A1L1=150, Y1A0L1=45, Y1A1L0=20, Y1A0L0=5, Y0A1L1=300, Y0A0L1=10, Y0A1L0=40, Y0A0L0=55)

form.table.object
ungrouped.data.object <- ungrouped.data(form.table.object)
head(ungrouped.data.object)
associational(form.table.object)
standardization(form.table.object)
iptw(form.table.object, type = "sw")
iptw.regression(form.table.object, type = "sw") # requires grid and survey package
graph(form.table.object, type = "sw")
```
### Author 
* Ehsan Karim :octocat: Feel free to [report](http://www.ehsankarim.com/) any errors / update suggestions. 

### Useful References
- [ ] Hernán MA, Robins JM (2016). [Causal Inference](https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/). Boca Raton: Chapman & Hall/CRC, forthcoming.

### Related web-Apps
- [x] [Causal Inference Web-Apps](http://www.ehsankarim.com/software/webapps)
