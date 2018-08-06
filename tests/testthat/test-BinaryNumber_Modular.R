

#install.packages("testthat");
library(testthat);

context("BinaryNumber R6 class")

test_that('MoBiNum initializes properly', {

  #browser();

  b1 <- MoBiNum$new(input = "1111");
  expect_equal(object = b1$get_prettystring(), expected = "1111");

  b1 <- MoBiNum$new(input = c(TRUE,FALSE,TRUE,FALSE));
  expect_equal(object = b1$get_prettystring(), expected = "1010");

});

test_that('MoBiNum increments properly', {

  #browser();

  b2 <- MoBiNum$new(input = "00");
  expect_equal(object = b2$get_prettystring(), expected = "00");
  b2$do_increment();
  expect_equal(object = b2$get_prettystring(), expected = "10");
  b2$do_increment();
  expect_equal(object = b2$get_prettystring(), expected = "01");
  b2$do_increment();
  expect_equal(object = b2$get_prettystring(), expected = "11");
  b2$do_increment();
  expect_equal(object = b2$get_prettystring(), expected = "00");
  b2$do_increment();

});

test_that("MoBiNum$get_equal_0()", {

  b <- MoBiNum$new(input = "11111");
  b$do_increment();
  expect_equal(b$get_equal_0(), TRUE);

  });

