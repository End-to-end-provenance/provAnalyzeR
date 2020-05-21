# assign pre-existing variables here, before provenance collection starts
a <- 1
b <- 2
cc <- 3

# start provenance collection using prov.init
library(rdtLite)
prov.init(prov.dir = "testdata")    # this tells it to put the provenance directory in the testdata folder

# source the script which contains your test case
prov.source("testscripts/source_preexisting.R")

# saves and ends provenance collection
prov.quit()