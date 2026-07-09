
<!-- README.md is generated from README.Rmd. -->

# Broad-scale biogeographic stability despite mass extinction in sharks across the K/Pg boundary

<!-- badges: start -->
<!-- badges: end -->

This GitHub repository contains the code used to analyse all
datasets presented in **Broad-scale biogeographic stability despite mass extinction in sharks across the K/Pg boundary**

by Mohamad Bazzi, Ashley Prow Fleischer, Mikael Siversson, Jood A. Al Aswad, Jun A. Ebersole, Jonathan L. Payne

Code written and maintained by Mohamad Bazzi
<br/>
Contact:
<mohamad.bazzi@nrm.se> and <mohammmed_bazzi@hotmail.com>

## Access .Rdata

Larger assets can be accessed from within a report using
[`piggyback`](https://github.com/ropensci/piggyback)!

``` r
# Install and load R package
require(piggyback)

# Create temporary directory and load .Rdata into R environment,
pb_download(file = "default.RData",dest = tempdir(),repo = "mohabazzi/kpg-homogenization",tag = "v.01")
load(file = file.path(tempdir(),"default.RData"))
```

[data](/data) contains all data file analysed in this study.
