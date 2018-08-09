
library(testthat);

context("algo_1100 R6 class");

test_that('algo_1100: exhaustive output test', {

  #browser();

  a1 <- algo_1100$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$exec("00"), "1");
  expect_equal(a1$exec("10"), "1");
  expect_equal(a1$exec("01"), "0");
  expect_equal(a1$exec("11"), "0");

});
