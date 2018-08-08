
library(testthat);

context("algo_1010 R6 class");

test_that('algo_1010: exhaustive output test', {

  #browser();

  a1 <- algo_1010$new();

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "0");

});
