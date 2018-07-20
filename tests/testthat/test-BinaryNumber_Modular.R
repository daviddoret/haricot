

#install.packages("testthat");
library(testthat);

test_that('BinaryNumber_Modular initializes properly', {

  b1 <- BinaryNumber_Modular$new(input = "1111");
  expect_equal(object = b1$get_prettystring(), expected = "1111");

  b1 <- BinaryNumber_Modular$new(input = c(TRUE,FALSE,TRUE,FALSE));
  expect_equal(object = b1$get_prettystring(), expected = "1010");

});

test_that('BinaryNumber_Modular increments properly', {

  b2 <- BinaryNumber_Modular$new(input = "00");
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
