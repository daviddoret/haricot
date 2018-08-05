
library(testthat);

context("AlgoTT0111 R6 class");

test_that('AlgoTT0111: exhaustive output test', {

  #browser();

  a1 <- AlgoTT0111$new();

  expect_equal(a1$do_execute("00"), "0");
  expect_equal(a1$do_execute("10"), "1");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "1");

});
