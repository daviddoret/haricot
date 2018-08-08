
library(testthat);

context("do_design_composite_switch");

test_that('do_design_composite_switch: test 01', {

  #browser();

  algo_0 <- algo_01$new();
  algo_1 <- algo_10$new();

  algo_switch <- do_design_composite_switch(algo_0, algo_1);

  expect_equal(algo_switch$do_execute("00"), "0");
  expect_equal(algo_switch$do_execute("10"), "1");
  expect_equal(algo_switch$do_execute("01"), "1");
  expect_equal(algo_switch$do_execute("11"), "0");

});
