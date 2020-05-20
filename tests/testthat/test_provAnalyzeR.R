library(testthat)
library(provAnalyzeR)

# test preexisting summary
test.script <- system.file("testscripts", "prov.R", package = "provAnalyzeR", mustWork=TRUE)
prov.analyze.run(test.cript)