require(testthat);

context("equal_bnum")

test_that('test 01', {

  #browser();

  expect_false(equal_bnum(bnum$new("1101"), bnum$new("110")));
  expect_true(equal_bnum(bnum$new("1101"), bnum$new("1101")));
  expect_false(equal_bnum(bnum$new("1101"), bnum$new("1100")));
  expect_false(equal_bnum(bnum$new("1101"), bnum$new("0010")));
  expect_true(equal_bnum(bnum$new("1101111001"), bnum$new("1101111001")));

});

