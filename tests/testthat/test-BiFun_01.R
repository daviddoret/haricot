
library(testthat);

test_that('BiFun_01: exhaustive output test', {

  #browser();

  nt <- BiFun_01$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("0"), "0");
  expect_equal(nt$do_apply_algorithm("1"), "1");

});
