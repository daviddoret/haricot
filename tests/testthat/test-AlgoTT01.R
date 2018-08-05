
library(testthat);

context("AlgoTT01 R6 class");

test_that('AlgoTT01: exhaustive output test', {

  #browser();

  a1 <- AlgoTT01$new();

  expect_equal(a1$do_execute("0"), "0");
  expect_equal(a1$do_execute("1"), "1");

});
