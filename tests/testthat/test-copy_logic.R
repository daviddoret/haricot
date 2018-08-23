
library(testthat);

context("copy_logic");

test_that('algo_not: test 01', {

  #browser();

  source <- algo_0011$new();
  target <- algo_composite$new(dim_i = 2, dim_o = 1);

  copy_logic(source, target);

  expect_equal(source$exec("00"),target$exec("00"));
  expect_equal(source$exec("10"),target$exec("10"));
  expect_equal(source$exec("01"),target$exec("01"));
  expect_equal(source$exec("11"),target$exec("11"));

});
