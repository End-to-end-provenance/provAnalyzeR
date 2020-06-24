# provAnalyzeR
`provAnalyzeR` is an R package that performs dynamic analysis on R code. It employs provenance to catch and report coding anomalies present in a given R script or within a console session. 

# Installation
Devtools is needed for installation:
```{r}
install.packages("devtools")
```
Installation of all required packages (can be copied and pasted):
```{r}
devtools::install_github("End-to-end-provenance/rdtLite")
devtools::install_github("End-to-end-provenance/provAnalyzeR", ref = "base")
```
This package also imports:
* provGraphR
* plotly 
* lintr 
* rstudioapi

Once installed, load the package:
```{r}
library("provAnalyzeR")
```

# Usage
While writing a script, run the script by calling:
```{r}
prov.analyze.run("yourScriptNameHere.R")
```
If you already have provenance stored as prov.json, you can also 
use that file as an argument.
```{r}
prov.analyze.file("prov.json")
```

Both of these functions will run analysis on your code and report their findings to the console. A graph of line by line execution times will also be displayed.

If you are working with RStudio, `prov.analyze.run` can also be quickly executed by clicking `Run analysis` from the addins menu. This drop-down menu appears in the top bar of RStudio, just above file names. 
