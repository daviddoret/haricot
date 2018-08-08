
library(testthat);

context("algo_1000 R6 class");

test_that('algo_1000: exhaustive output test', {

  #browser();

  a1 <- algo_1000$new();

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "0");
  expect_equal(a1$do_execute("11"), "0");

});
