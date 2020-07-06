library(testthat)
library(provAnalyzeR)

context("analyze.function.reassignments")

# === HELPER FUNCTIONS ======================================================= #

# helper function to create the list of expected tables
get.expected <- function()
{
  # function change
  d <- data.frame(value = c("function(a, b) {...", "function (cc, d) {..."),
                  code = c('sum <- function(a, b) {\n  a + b\n}', 'sum <- function (cc, d) {\n  cc * d\n}'),
                  scriptNum = c(1,1),
                  startLine = c(3,9),
                  stringsAsFactors = FALSE)
  
  # reassginment, nothing changes
  e <- data.frame(value = c("function(a, b) {...", "function(a, b) {..."),
                  code = c('divide <- function(a, b) {\n  a / b\n}', 'divide <- function(a, b) {\n  a / b\n}'),
                  scriptNum = c(1,1),
                  startLine = c(18,24),
                  stringsAsFactors = FALSE)
  
  # multiple function changes
  f <- data.frame(value = c("function() {...", "function() {...", "function() {..."),
                  code = c('uselessFunction <- function() {\n  hello <- 5\n}', 'uselessFunction <- function() {\n  hello <- 5\n}', 'uselessFunction <- function() {\n  hello <- 5\n}'),
                  scriptNum = c(1,1,1),
                  startLine = c(32,36,40),
                  stringsAsFactors = FALSE)
  
  
  # combine
  type.changes <- list(d,e,f)
  names(type.changes) <- c("d", "e", "f")
  
  return(type.changes)
}

# === THE TESTS ============================================================== #

# no provenance
test_that("analyze.function.reassignments - no/empty provenance", 
          {
            # clean analyze environment of provAnalyzeR first to ensure inital state
            provAnalyzeR:::.clear()
            
            # initialisation not run
            expect_false(provAnalyzeR:::.analyze.env$has.graph)
            expect_error(analyze.function.reassignments())
            
            # empty provenance
            c0 <- system.file("testdata", "empty.json", package = "provAnalyzeR")
            expect_error(prov.analyze.file(c0))
            expect_false(provAnalyzeR:::.analyze.env$has.graph)
            expect_error(analyze.function.reassignments())
          })

# no data nodes
test_that("analyze.function.reassignments - no data nodes",
          {
            skip("analyze.function.reassignments - no data nodes")
            
            json <- system.file("testdata", "noDataNodes.json", package = "provAnalyzeR")
            
            provAnalyzeR:::.clear()
            expect_warning(prov.analyze.file(json))   # warning is due to deleted prov folder
            
            c2 <- utils::capture.output(c1 <- analyze.function.reassignments())
            
            expect_null(c1)
            expect_true(nchar(paste(c2, collapse='\n')) > 0)
          })

# no variables
test_that("analyze.function.reassignments - no variables",
          {
            json <- system.file("testdata", "noVars.json", package = "provAnalyzeR")
            
            provAnalyzeR:::.clear()
            expect_warning(prov.analyze.file(json))   # warning is due to deleted prov folder
            
            c2 <- utils::capture.output(c1 <- analyze.function.reassignments())
            
            expect_null(c1)
            expect_true(nchar(paste(c2, collapse='\n')) > 0)
          })


# analyze.function.reassignments tests
json <- system.file("testdata", "functionReassignments.json", package = "provAnalyzeR")

provAnalyzeR:::.clear()
expect_warning(prov.analyze.file(json))   # warning is due to deleted prov folder

expected <- get.expected()

# analyze.function.reassignments - no parameters
test_that("analyze.function.reassignments - no parameters",
          {
            c1 <- analyze.function.reassignments()
            
            expect_equivalent(c1, expected)
          })

# # analyze.function.reassignments - all valid variables queried (single)
# test_that("analyze.function.reassignments - all valid variables queried (single)",
#           {
#             # queries
#             q1 <- "d"
#             q2 <- "e"
#             q3 <- "f"
#             q4 <- "g"
#             q5 <- "h"
#             q6 <- "s"
#             
#             # test cases
#             c1 <- analyze.function.reassignments(var = q1)[[1]]
#             c2 <- analyze.function.reassignments(var = q2)[[1]][ ,-1]          # omit value column
#             c3 <- analyze.function.reassignments(var = q3)[[1]]
#             c4 <- analyze.function.reassignments(var = q4)[[1]]
#             c5 <- analyze.function.reassignments(var = q5)[[1]]
#             c6 <- analyze.function.reassignments(var = q6)[[1]][ , c(-1,-5)]   # omit value and code column
#             
#             # test
#             expect_equivalent(c1, expected$d)
#             expect_equivalent(c2, expected$e)
#             expect_equivalent(c3, expected$f)
#             expect_equivalent(c4, expected$g)
#             expect_equivalent(c5, expected$h)
#             expect_equivalent(c6, expected$s)
#           })
# 
# # analyze.function.reassignments - all valid variables queried (multiple)
# test_that("analyze.function.reassignments - all valid variables queried (multiple)",
#           {
#             # queries
#             q1 <- c("d", "f", "g")   # multiple variables
#             q2 <- c("f", "h", "d")   # multiple variables (in different order)
#             q3 <- c("d", "d", "f")   # repeated query
#             
#             # test cases
#             c1 <- analyze.function.reassignments(var = q1)
#             c2 <- analyze.function.reassignments(var = q2)
#             c3 <- analyze.function.reassignments(var = q3)
#             
#             # expected
#             e1 <- expected[q1]
#             e2 <- expected[q2]
#             e3 <- expected[c("d", "f")]
#             
#             # test
#             expect_equivalent(c1, e1)
#             expect_equivalent(c2, e2)
#             expect_equivalent(c3, e3)
#           })
# 
# # analyze.function.reassignments - all invalid variables queried
# test_that("analyze.function.reassignments - all invalid variables queried",
#           {
#             # queries
#             q1 <- "invalid"      # variable does not exist
#             q2 <- "Rplots.pdf"   # data node with name exists, but is not a variable
#             q3 <- "a"            # no type change (just single assignment)
#             q4 <- "cc"           # no type change (value does change)
#             
#             # test cases (both returned value and message output)
#             c2 <- utils::capture.output(c1 <- analyze.function.reassignments(var = q1))
#             c4 <- utils::capture.output(c3 <- analyze.function.reassignments(var = q2))
#             c6 <- utils::capture.output(c5 <- analyze.function.reassignments(var = q3))
#             c8 <- utils::capture.output(c7 <- analyze.function.reassignments(var = q4))
#             
#             # test returned values
#             expect_null(c1)
#             expect_null(c3)
#             expect_null(c5)
#             expect_null(c7)
#             
#             # test message output
#             c2 <- paste(c2, collapse = "\n")
#             c4 <- paste(c4, collapse = "\n")
#             c6 <- paste(c6, collapse = "\n")
#             c8 <- paste(c8, collapse = "\n")
#             
#             expect_true(nchar(c2) > 0)
#             expect_true(nchar(c4) > 0)
#             expect_true(nchar(c6) > 0)
#             expect_true(nchar(c8) > 0)
#           })
# 
# # analyze.function.reassignments - some valid, some invalid variables queried
# test_that("analyze.function.reassignments - some valid, some invalid vars",
#           {
#             # queries
#             q1 <- c("invalid", "h", "a", "g")
#             
#             # test cases
#             c1 <- analyze.function.reassignments(var = q1)
#             
#             # test expected return values
#             e1 <- expected[c("h", "g")]
#             expect_equivalent(c1, e1)
#           })
