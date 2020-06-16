# long line
a <- "this is a test of a very long line in R code to see if lintr notes that it is longer than a line should be. 0123456789876543210"

# incorrect and correct assignments
b = 5L
cc <- 5L

# TRUE abbreviated
if (T) {
  x <- 4L
}

# c used unnecessarily
d <- c("five")

# use sapply (slow function)
alist <- c("red", "blue", "green", "yellow")
sapply(alist, print) 
# FAILED!!

# use 1:nrow(...) type expression
df <- data.frame(c(4L, 3L, 2L), c(4L, 5L, 6L))
for (x in 1L:nrow(df)) {
  cat("hello\n")
}

# implicit integer
e <- 5
f <- 4 + 5L

#undesirable_operator_linter,  # report use of undesirable operators

#cyclocomp_linter,  # check for overly complicated expressions




