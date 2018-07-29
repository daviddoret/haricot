
library(testthat);

test_that('BiFun_10: exhaustive output test', {

  #browser();

  nt <- BiFun_10$new(node_id = "n1");

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("0"), "1");
  expect_equal(nt$do_apply_algorithm("1"), "0");

});
