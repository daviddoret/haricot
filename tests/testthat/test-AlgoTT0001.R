
library(testthat);

context("algo_0001 R6 class");

test_that('algo_0001: exhaustive output test', {

  #browser();

  a1 <- algo_0001$new();

  expect_equal(a1$do_execute("00"), "0");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "0");
  expect_equal(a1$do_execute("11"), "1");

});
