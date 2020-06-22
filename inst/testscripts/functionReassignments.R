# one function change
# parameters change
sum <- function(a, b) {
  a + b
}

sum_1 <- sum(5, 2)

sum <- function (cc, d) {
  cc * d
}

sum_2 <- sum(5, 2)


# one function change
# function contents don't change 
divide <- function(a, b) {
  a / b
}

divide_1 <- divide(5, 2)

divide <- function(a, b) {
  a / b
}

divide_2 <- divide(5, 2)


# function reassigned multiple times
uselessFunction <- function() {
  hello <- 5
}

uselessFunction <- function() {
  hello <- 5
}

uselessFunction <- function() {
  hello <- 5
}
