require(testthat);

context("force_matrix");

test_that('force_matrix: basic tests', {

  m1 <- force_matrix(TRUE);
  expect_equal(nrow(m1), 1);
  expect_equal(ncol(m1), 1);

  m2 <- force_matrix(c(TRUE, FALSE), vector_direction = VECTOR_DIRECTION_HORIZONTAL);
  expect_equal(nrow(m2), 1);
  expect_equal(ncol(m2), 2);

  m3 <- force_matrix(c(TRUE, FALSE), vector_direction = VECTOR_DIRECTION_VERTICAL);
  expect_equal(nrow(m3), 2);
  expect_equal(ncol(m3), 1);

  m4 <- matrix(c(TRUE,FALSE,TRUE,FALSE), nrow=2, ncol=2);
  expect_equal(nrow(m4), 2);
  expect_equal(ncol(m4), 2);

});



