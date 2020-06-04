# assign pre-existing variables here, before provenance collection starts
a <- 1
b <- 2
cc <- 3

# start prov collection: use a, b, but not cc
library(rdtLite)
prov.init(prov.dir = "testdata", snapshot.size="10")   # this tells it to put the provenance directory in the testdata folder

# source the script which contains your test case
prov.source("testscripts/source_preexisting.R")

# saves and ends provenance collection
prov.quit()