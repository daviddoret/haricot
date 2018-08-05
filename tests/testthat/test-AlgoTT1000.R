
library(testthat);

test_that('AlgoTT1000: exhaustive output test', {

  #browser();

  a1 <- AlgoTT1000$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "0");
  expect_equal(a1$do_execute("11"), "0");

});
