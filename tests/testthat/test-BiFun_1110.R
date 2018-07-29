
library(testthat);

test_that('BiFun_1110: exhaustive output test', {

  #browser();

  nt <- BiFun_1110$new(node_id = "n1");

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("00"), "1");
  expect_equal(nt$do_apply_algorithm("10"), "1");
  expect_equal(nt$do_apply_algorithm("01"), "1");
  expect_equal(nt$do_apply_algorithm("11"), "0");

});
