require(testthat);

context("convert_tt_constant_to_composite");

test_that('convert_tt_constant_to_composite: basic test', {

  #browser();

  a <- algo_tt$new(0,4);
  a$do_randomize_outputs();

  b <- convert_tt_constant_to_composite(a);

  expect_equal(a$exec(""), b$exec(""));

});
