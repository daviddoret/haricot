
library(testthat);

context("do_copy_logic_algo_composite_to_algo_composite");

test_that('AlgoNOT: test 01', {

  #browser();

  a <- AlgoNOT$new();

  expect_equal(a$do_execute("0"), "1");
  expect_equal(a$do_execute("1"), "0");

});
