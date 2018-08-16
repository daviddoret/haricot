require(testthat);

context("algo_0 R6 class");

test_that('algo_0: exhaustive output test', {

  #browser();

  a <- algo_0$new();

  expect_equal(a$exec(""), "0");

});
