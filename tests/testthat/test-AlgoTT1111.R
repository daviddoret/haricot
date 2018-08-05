
library(testthat);

context("AlgoTT1111 R6 class");

test_that('AlgoTT1111: exhaustive output test', {

  #browser();

  a1 <- AlgoTT1111$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "1");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "1");

});
