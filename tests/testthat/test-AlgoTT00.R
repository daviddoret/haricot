
library(testthat);

context("AlgoTT00 R6 class");

test_that('AlgoTT00: exhaustive output test', {

  #browser();

  a1 <- AlgoTT00$new();

  expect_equal(a1$do_execute("0"), "0");
  expect_equal(a1$do_execute("1"), "0");

});
