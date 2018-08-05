
library(testthat);

test_that('AlgoTT01: exhaustive output test', {

  #browser();

  a1 <- AlgoTT01$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("0"), "0");
  expect_equal(a1$do_execute("1"), "1");

});
