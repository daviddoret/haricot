
library(testthat);

context("algo_nand R6 class");

test_that('algo_nand: test 01', {

  #browser();

  a <- algo_nand$new();

  #print(a);
  #plot_nandtree(a);

  expect_equal(a$do_execute("00"), "1");
  expect_equal(a$do_execute("10"), "1");
  expect_equal(a$do_execute("01"), "1");
  expect_equal(a$do_execute("11"), "0");

  a$plot()

});
