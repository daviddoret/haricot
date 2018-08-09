
library(testthat);

context("algo_0110 R6 class");

test_that('algo_0110: exhaustive output test', {

  #browser();

  a1 <- algo_0110$new();

  expect_equal(a1$exec("00"), "0");
  expect_equal(a1$exec("10"), "1");
  expect_equal(a1$exec("01"), "1");
  expect_equal(a1$exec("11"), "0");

});
