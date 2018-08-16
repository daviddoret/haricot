require(testthat);

context("algo_1 R6 class");

test_that('algo_1: exhaustive output test', {

  #browser();

  a <- algo_1$new();

  expect_equal(a$exec(""), "1");

});
