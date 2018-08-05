
library(testthat);

context("AlgoTT11 R6 class");

test_that('AlgoTT11: exhaustive output test', {

  #browser();

  a1 <- AlgoTT11$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("0"), "1");
  expect_equal(a1$do_execute("1"), "1");

});
