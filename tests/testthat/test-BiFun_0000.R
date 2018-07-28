
library(testthat);

test_that('BiFun_0000: exhaustive output test', {

  #browser();

  nt <- BiFun_0000$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("00"), "0");
  expect_equal(nt$do_apply_algorithm("10"), "0");
  expect_equal(nt$do_apply_algorithm("01"), "0");
  expect_equal(nt$do_apply_algorithm("11"), "0");

});
