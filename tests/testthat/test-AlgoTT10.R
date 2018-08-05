
library(testthat);

context("AlgoTT10 R6 class");

test_that('AlgoTT10: exhaustive output test', {

  #browser();

  a <- AlgoTT10$new();

  expect_equal(a$do_execute("0"), "1");
  expect_equal(a$do_execute("1"), "0");

});
