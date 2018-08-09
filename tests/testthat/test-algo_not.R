
library(testthat);

context("AlgoNOT R6 class");

test_that('AlgoNOT: test 01', {

  #browser();

  a <- AlgoNOT$new();

  expect_equal(a$exec("0"), "1");
  expect_equal(a$exec("1"), "0");

});
