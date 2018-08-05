
library(testthat);

context("AlgoTT0010 R6 class");

test_that('AlgoTT0010: exhaustive output test', {

  #browser();

  a1 <- AlgoTT0010$new();

  expect_equal(a1$do_execute("00"), "0");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "0");

});
