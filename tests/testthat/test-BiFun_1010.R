
library(testthat);

test_that('BiFun_1010: exhaustive output test', {

  #browser();

  nt <- BiFun_1010$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_execute("00"), "1");
  expect_equal(nt$do_execute("10"), "0");
  expect_equal(nt$do_execute("01"), "1");
  expect_equal(nt$do_execute("11"), "0");

});
