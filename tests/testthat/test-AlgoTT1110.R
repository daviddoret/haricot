
library(testthat);

test_that('AlgoTT1110: exhaustive output test', {

  #browser();

  a1 <- AlgoTT1110$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a1$do_execute("00"), "1");
  expect_equal(a1$do_execute("10"), "1");
  expect_equal(a1$do_execute("01"), "1");
  expect_equal(a1$do_execute("11"), "0");

});
