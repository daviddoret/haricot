require(testthat);

context("convert_int_to_logical_vector");

test_that('NULL input returns 0', {
  expect_equal(convert_int_to_logical_vector(NULL), c(FALSE));
});

test_that('missing input returns 0', {
  expect_equal(convert_int_to_logical_vector(), c(FALSE));
});

test_that('missing input with dim returns properly sized vector', {
  expect_equal(convert_int_to_logical_vector(dim = 3), c(FALSE, FALSE, FALSE));
});

test_that('17 == 10001', {
  expect_equal(convert_int_to_logical_vector(17), c(TRUE, FALSE, FALSE, FALSE, TRUE));
});

test_that('17 == 10001 with dim = 3 truncates the result', {
  expect_equal(convert_int_to_logical_vector(17, 3), c(TRUE, FALSE, FALSE));
});

test_that('17 == 10001 with dim = 9 pad with 4 zeroes', {
  expect_equal(convert_int_to_logical_vector(17, 9), c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE));
});
