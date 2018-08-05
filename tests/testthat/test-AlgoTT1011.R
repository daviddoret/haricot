
library(testthat);

context("AlgoTT1011 R6 class");

test_that('AlgoTT1011: exhaustive output test', {

  #browser();

  a1 <- AlgoTT1011$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "1");

});
