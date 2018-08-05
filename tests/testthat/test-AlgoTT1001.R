
library(testthat);

context("AlgoTT1001 R6 class");

test_that('AlgoTT1001: exhaustive output test', {

  #browser();

  a <- AlgoTT1001$new();

  expect_equal(a$do_execute("00"), "1");
  expect_equal(a$do_execute("10"), "0");
  expect_equal(a$do_execute("01"), "0");
  expect_equal(a$do_execute("11"), "1");

});
