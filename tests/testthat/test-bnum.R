

#install.packages("testthat");
library(testthat);

context("bnum R6 class")

test_that('bnum initializes properly', {

  #browser();

  b1 <- bnum$new(input = "1111");
  expect_equal(object = b1$get_prettystring(), expected = "1111");

  b1 <- bnum$new(input = c(TRUE,FALSE,TRUE,FALSE));
  expect_equal(object = b1$get_prettystring(), expected = "1010");

});

test_that('bnum increments properly', {

  #browser();

  b2 <- bnum$new(input = "00");
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

test_that("bnum$get_equal_0()", {

  b <- bnum$new(input = "11111");
  b$do_increment();
  expect_equal(b$get_equal_0(), TRUE);

  });

test_that("bnum$randomize()", {

  b <- bnum$new(input = "11111");
  b$do_increment();
  expect_equal(b$get_equal_0(), TRUE);

});
