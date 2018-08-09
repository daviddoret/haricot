
library(testthat);

context("algo_0101 R6 class");

test_that('algo_0101: exhaustive output test', {

  #browser();

  a1 <- algo_0101$new();

  expect_equal(a1$exec("00"), "0");
  expect_equal(a1$exec("10"), "1");
  expect_equal(a1$exec("01"), "0");
  expect_equal(a1$exec("11"), "1");

});
