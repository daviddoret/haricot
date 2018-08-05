
library(testthat);

context("AlgoNAND R6 class");

test_that('AlgoNAND: test 01', {

  #browser();

  a <- AlgoNAND$new();

  #print(a);
  #plot_nandtree(a);

  expect_equal(a$do_execute("00"), "1");
  expect_equal(a$do_execute("10"), "1");
  expect_equal(a$do_execute("01"), "1");
  expect_equal(a$do_execute("11"), "0");

  a$do_plot()

});
