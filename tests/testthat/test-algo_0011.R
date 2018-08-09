
library(testthat);

context("algo_0011 R6 class");

test_that('algo_0011: exhaustive output test', {

  #browser();

  a1 <- algo_0011$new();

  expect_equal(a1$exec("00"), "0");
  expect_equal(a1$exec("10"), "0");
  expect_equal(a1$exec("01"), "1");
  expect_equal(a1$exec("11"), "1");

});
