
library(testthat);

context("algo_1101 R6 class");

test_that('algo_1101: exhaustive output test', {

  #browser();

  a1 <- algo_1101$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "1");
  expect_equal(a1$do_execute("01"), "0");
  expect_equal(a1$do_execute("11"), "1");

});
