
library(testthat);

context("algo_10 R6 class");

test_that('algo_10: exhaustive output test', {

  #browser();

  a <- algo_10$new();

  expect_equal(a$exec("0"), "1");
  expect_equal(a$exec("1"), "0");

});
