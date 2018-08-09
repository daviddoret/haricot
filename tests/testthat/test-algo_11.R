
library(testthat);

context("algo_11 R6 class");

test_that('algo_11: exhaustive output test', {

  #browser();

  a1 <- algo_11$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$exec("0"), "1");
  expect_equal(a1$exec("1"), "1");

});
