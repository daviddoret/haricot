
library(testthat);

context("algo_1001 R6 class");

test_that('algo_1001: exhaustive output test', {

  #browser();

  a <- algo_1001$new();

  expect_equal(a$exec("00"), "1");
  expect_equal(a$exec("10"), "0");
  expect_equal(a$exec("01"), "0");
  expect_equal(a$exec("11"), "1");

});
