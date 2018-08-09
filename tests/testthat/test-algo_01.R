
library(testthat);

context("algo_01 R6 class");

test_that('algo_01: exhaustive output test', {

  #browser();

  a1 <- algo_01$new();

  expect_equal(a1$exec("0"), "0");
  expect_equal(a1$exec("1"), "1");

});
