
library(testthat);

context("algo_not R6 class");

test_that('algo_not: test 01', {

  #browser();

  a <- algo_not$new();

  expect_equal(a$exec("0"), "1");
  expect_equal(a$exec("1"), "0");

});
