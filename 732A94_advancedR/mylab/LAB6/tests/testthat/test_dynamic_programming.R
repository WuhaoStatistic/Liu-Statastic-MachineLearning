suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
library(Rcpp)
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(bfk <- dynamic_programming(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
})


test_that("functions rejects errounous input.", {
  expect_error(dynamic_programming("hej", 3500))
  expect_error(dynamic_programming(x = knapsack_objects[1:8,], W = -3500))
  expect_error(dynamic_programming(x = knapsack_objects[1:8,], W = 3500,fast = "asdf"))
})

test_that("Function return correct results.", {
  bfk <- dynamic_programming(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  
  bfk <- dynamic_programming(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(bfk$value), 16770)
  expect_true(all(round(bfk$elements) %in% c(5, 8)))
  
  bfk <- dynamic_programming(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  bfk <- dynamic_programming(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(bfk$value), 15428)
  expect_true(all(round(bfk$elements) %in% c(3, 8)))
  
  bfk <- dynamic_programming(x = knapsack_objects[2:800,], W = 3500,TRUE)
  expect_equal(round(bfk$value), 195283)
  

  st <- system.time(bfk <- dynamic_programming(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)
})