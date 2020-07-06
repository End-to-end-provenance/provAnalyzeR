library(testthat)
library(provAnalyzeR)

context("analyze.invalid.names")

# === HELPER FUNCTIONS ======================================================= #

# helper function to create the list of expected tables
get.expected <- function()
{
  # container change
  `c` <- data.frame(value = as.character(5),
                  scriptNum = 1,
                  startLine = 2,
                  stringsAsFactors = FALSE)
  
  # dimension change
  # omit value column as it's too long
  `t` <- data.frame(value = as.character(13),
                  scriptNum = 1,
                  startLine = 6,
                  stringsAsFactors = FALSE)
  
  # type change
  `T` <- data.frame(value = "\"false\"",
                  scriptNum = 1,
                  startLine = 14,
                  stringsAsFactors = FALSE)
  
  # combine
  type.changes <- list(`c`, `t`, `T`)
  names(type.changes) <- c("c", "t", "T")
  
  return(type.changes)
}

# === THE TESTS ============================================================== #

# analyze.type.changes tests
json <- system.file("testdata", "invalidNames.json", package = "provAnalyzeR")

provAnalyzeR:::.clear()
expect_warning(prov.analyze.file(json))   # warning is due to deleted prov folder

expected <- get.expected()

# analyze.type.changes - no parameters
test_that("analyze.invalid.names - no parameters",
          {
            c1 <- analyze.invalid.names()
            
            expect_equivalent(c1, expected)
          })
