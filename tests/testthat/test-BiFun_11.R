
library(testthat);

test_that('BiFun_11: exhaustive output test', {

  #browser();

  nt <- BiFun_11$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_execute("0"), "1");
  expect_equal(nt$do_execute("1"), "1");

});
