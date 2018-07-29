
library(testthat);

test_that('BiFun_1001: exhaustive output test', {

  #browser();

  nt <- BiFun_1001$new(node_id = "n1");

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(nt$do_apply_algorithm("00"), "1");
  expect_equal(nt$do_apply_algorithm("10"), "0");
  expect_equal(nt$do_apply_algorithm("01"), "0");
  expect_equal(nt$do_apply_algorithm("11"), "1");

});
