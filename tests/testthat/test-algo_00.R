
library(testthat);

context("algo_00 R6 class");

test_that('algo_00: exhaustive output test', {

  #browser();

  verbosity <- 1;
  a1 <- algo_00$new();

  expect_equal(a1$exec("0", verbosity), "0");
  expect_equal(a1$exec("1", verbosity), "0");

});
