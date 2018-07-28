
library(testthat);

test_that('BiFun_1010: exhaustive output test', {

  #browser();

  nt <- BiFun_1010$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("00"), "1");
  expect_equal(nt$do_apply_algorithm("10"), "0");
  expect_equal(nt$do_apply_algorithm("01"), "1");
  expect_equal(nt$do_apply_algorithm("11"), "0");

});
