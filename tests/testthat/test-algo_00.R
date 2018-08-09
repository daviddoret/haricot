
library(testthat);

context("algo_00 R6 class");

test_that('algo_00: exhaustive output test', {

  #browser();

  a1 <- algo_00$new();

  expect_equal(a1$do_execute("0"), "0");
  expect_equal(a1$do_execute("1"), "0");

});
