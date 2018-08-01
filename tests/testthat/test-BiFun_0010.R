
library(testthat);

test_that('BiFun_0010: exhaustive output test', {

  #browser();

  nt <- BiFun_0010$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_execute("00"), "0");
  expect_equal(nt$do_execute("10"), "0");
  expect_equal(nt$do_execute("01"), "1");
  expect_equal(nt$do_execute("11"), "0");

});
