
library(testthat);

context("AlgoTT0100 R6 class");

test_that('AlgoTT0100: exhaustive output test', {

  #browser();

  a1 <- AlgoTT0100$new();

  expect_equal(a1$do_execute("00"), "0");
  expect_equal(a1$do_execute("10"), "1");
  expect_equal(a1$do_execute("01"), "0");
  expect_equal(a1$do_execute("11"), "0");

});
