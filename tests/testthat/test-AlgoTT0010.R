
library(testthat);

test_that('AlgoTT0010: exhaustive output test', {

  #browser();

  a1 <- AlgoTT0$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("00"), "0");
  expect_equal(a1$do_execute("10"), "0");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "0");

});
