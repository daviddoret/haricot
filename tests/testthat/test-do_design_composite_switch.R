
library(testthat);

context("design_algo_switch");

test_that('design_algo_switch: test 01', {

  #browser();

  algo_0 <- algo_01$new();
  algo_1 <- algo_10$new();

  algo_switch <- design_algo_switch(algo_0, algo_1);

  expect_equal(algo_switch$exec("00"), "0");
  expect_equal(algo_switch$exec("10"), "1");
  expect_equal(algo_switch$exec("01"), "1");
  expect_equal(algo_switch$exec("11"), "0");

});
