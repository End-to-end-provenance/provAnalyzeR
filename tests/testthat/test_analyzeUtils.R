library(testthat)
library(provAnalyzeR)

context("Utility functions")

# .clear
# This is a function that is used only for testing purposes
test_that("Utility - .clear", 
          {
            json <- system.file("testdata", "exceptions.json", package = "provAnalyzeR")
            expect_warning(prov.analyze.file(json))   # warning due to deleted prov folder
            
            # ensure .analyze.env has been changed
            expect_false(is.null(provAnalyzeR:::.analyze.env$prov))
            expect_false(is.null(provAnalyzeR:::.analyze.env$graph))
            expect_true(provAnalyzeR:::.analyze.env$has.graph)
            expect_false(is.null(provAnalyzeR:::.analyze.env$proc.nodes))
            expect_false(is.null(provAnalyzeR:::.analyze.env$data.nodes))
            expect_false(is.null(provAnalyzeR:::.analyze.env$data.proc))
            expect_false(is.null(provAnalyzeR:::.analyze.env$proc.data))
            expect_false(is.null(provAnalyzeR:::.analyze.env$var.env))
            expect_false(is.null(provAnalyzeR:::.analyze.env$prov.dir))
            
            # clear .analyze.env
            provAnalyzeR:::.clear()
            
            # ensure .analyze.env has been returned to initial state
            expect_true(is.null(provAnalyzeR:::.analyze.env$prov))
            expect_true(is.null(provAnalyzeR:::.analyze.env$graph))
            expect_false(provAnalyzeR:::.analyze.env$has.graph)
            expect_true(is.null(provAnalyzeR:::.analyze.env$proc.nodes))
            expect_true(is.null(provAnalyzeR:::.analyze.env$data.nodes))
            expect_true(is.null(provAnalyzeR:::.analyze.env$data.proc))
            expect_true(is.null(provAnalyzeR:::.analyze.env$proc.data))
            expect_true(is.null(provAnalyzeR:::.analyze.env$var.env))
            expect_true(is.null(provAnalyzeR:::.analyze.env$prov.dir))
          })

# .extract.vars
test_that("Utility - .extract.vars",
          {
            # CASES
            df1 <- data.frame(name = character(),                     # no data nodes
                              type = character(),
                              stringsAsFactors = FALSE)
            
            df2 <- data.frame(name = "dev.off",                       # no variables
                              type = "Device",
                              stringsAsFactors = FALSE)
            
            df3 <- data.frame(name = c("x","y"),                      # all variables
                              type = c("Data", "Snapshot"),
                              stringsAsFactors = FALSE)
            
            df4 <- data.frame(name = c("x","dev.off","y"),            # some vars
                              type = c("Snapshot", "Device", "Data"),
                              stringsAsFactors = FALSE)
            
            c1 <- provAnalyzeR:::.extract.vars(df1)
            c2 <- provAnalyzeR:::.extract.vars(df2)
            c3 <- provAnalyzeR:::.extract.vars(df3)
            c4 <- provAnalyzeR:::.extract.vars(df4)
            
            # EXPECTED
            e1 <- df1
            e2 <- df1
            e3 <- df3
            e4 <- df4[-2, ]
            
            # TEST
            expect_equivalent(c1, e1)
            expect_equivalent(c2, e2)
            expect_equivalent(c3, e3)
            expect_equivalent(c4, e4)
          })

# .form.df
test_that("Utility - .form.df",
          {
            # 1 row
            l1 <- list(data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE))
            c1 <- provAnalyzeR:::.form.df(l1)
            
            e1 <- data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE)
            
            expect_equal(c1, e1)
            
            # 1 column
            l2 <- list(data.frame(col1 = 1L, stringsAsFactors = FALSE),
                       data.frame(col1 = 2L, stringsAsFactors = FALSE),
                       data.frame(col1 = c(3L,4L), stringsAsFactors = FALSE))
            c2 <- provAnalyzeR:::.form.df(l2)
            
            e2 <- data.frame(col1 = as.integer(c(1:4)), stringsAsFactors = FALSE)
            
            expect_equal(c2, e2)
            
            # columns have different types
            l3 <- list(data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE),
                       data.frame(col1 = 2L, col2 = "str.2", stringsAsFactors = FALSE),
                       data.frame(col1 = as.integer(NA), col2 = "str.3", stringsAsFactors = FALSE))
            c3 <- provAnalyzeR:::.form.df(l3)
            
            e3 <- data.frame(col1 = as.integer(c(1,2,NA)),
                             col2 = c("str.1", "str.2", "str.3"),
                             stringsAsFactors = FALSE)
            
            expect_equal(c3, e3)
            
            # columns have the same type
            l4 <- list(data.frame(col1 = 1L, col2 = 11L, stringsAsFactors = FALSE),
                       data.frame(col1 = 2L, col2 = 12L, stringsAsFactors = FALSE),
                       data.frame(col1 = 3L, col2 = 13L, stringsAsFactors = FALSE))
            c4 <- provAnalyzeR:::.form.df(l4)
            
            e4 <- data.frame(col1 = as.integer(c(1:3)),
                             col2 = as.integer(c(11:13)),
                             stringsAsFactors = FALSE)
            
            expect_equal(c4, e4)
            
            # some elements have multiple rows
            l5 <- list(data.frame(col1 = 1L, col2 = "str.1", stringsAsFactors = FALSE),
                       data.frame(col1 = c(2L,3L,4L), col2 = c("str.2", "str.3", "str.4"), stringsAsFactors = FALSE),
                       data.frame(col1 = 5L, col2 = "str.5", stringsAsFactors = FALSE))
            c5 <- provAnalyzeR:::.form.df(l5)
            
            e5 <- data.frame(col1 = as.integer(c(1:5)),
                             col2 = c("str.1", "str.2", "str.3", "str.4", "str.5"),
                             stringsAsFactors = FALSE)
            
            expect_equal(c5, e5)
          })

# .get.p.id
test_that("Utility - .get.p.id",
          {
            json <- system.file("testdata", "preexisting.json", package = "provAnalyzeR")
            
            provAnalyzeR:::.clear()
            expect_warning(prov.analyze.file(json))   # warning due to deleted prov folder
            
            # Cases
            c1 <- "d4"    # 1 node found (from output edge only)
            c2 <- "d14"   # 1 node found (from input edge only)
            c3 <- "d1"    # 1 node found (has both output and input edges)
            c4 <- "d2"    # multiple nodes found (multiple input edges)
            
            c1 <- provAnalyzeR:::.get.p.id(c1)
            c2 <- provAnalyzeR:::.get.p.id(c2)
            c3 <- provAnalyzeR:::.get.p.id(c3)
            c4 <- provAnalyzeR:::.get.p.id(c4)
            
            # Expected
            e1 <- "p5"  #p6
            e2 <- "p15"  #p16
            e3 <- "p3"
            e4 <- c("p4", "p6")
            
            # Test
            expect_equivalent(c1, e1)
            expect_equivalent(c2, e2)
            expect_equivalent(c3, e3)
            expect_equivalent(c4, e4)
          })

# .remove.na.rows
test_that("Utility - .remove.na.rows",
          {
            # cases
            df1 <- data.frame(col1 = integer(),                   # no rows
                              col2 = character(),
                              stringsAsFactors = FALSE)
            
            df2 <- data.frame(col1 = c(1:3),
                              col2 = c("item.1", NA, "item.3"),   # no NA rows
                              stringsAsFactors = FALSE)
            
            df3 <- data.frame(col1 = c(1,NA,3,4,NA),              # some NA rows
                              col2 = c(1,NA,3,4,NA),
                              stringsAsFactors = FALSE)
            
            df4 <- data.frame(col1 = as.integer(c(NA,NA)),        # all NA rows
                              col2 = as.character(c(NA,NA)),
                              stringsAsFactors = FALSE)
            
            c1 <- provAnalyzeR:::.remove.na.rows(df1)
            c2 <- provAnalyzeR:::.remove.na.rows(df2)
            c3 <- provAnalyzeR:::.remove.na.rows(df3)
            c4 <- provAnalyzeR:::.remove.na.rows(df4)
            
            # expected
            e1 <- df1
            e2 <- df2
            e3 <- df3[c(1,3,4), ]
            e4 <- df1
            
            # test
            expect_equivalent(c1, e1)
            expect_equivalent(c2, e2)
            expect_equivalent(c3, e3)
            expect_equivalent(c4, e4)
          })