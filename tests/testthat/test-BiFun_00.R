
library(testthat);

test_that('BiFun_00: exhaustive output test', {

  #browser();

  nt <- BiFun_00$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_execute("0"), "0");
  expect_equal(nt$do_execute("1"), "0");

});
