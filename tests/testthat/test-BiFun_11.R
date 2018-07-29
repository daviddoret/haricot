
library(testthat);

test_that('BiFun_11: exhaustive output test', {

  #browser();

  nt <- BiFun_11$new(node_id = "n1");

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("0"), "1");
  expect_equal(nt$do_apply_algorithm("1"), "1");

});
