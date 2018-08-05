

#install.packages("testthat");
library(testthat);

context("BinaryDomain R6 class")

test_that('BinaryDomain length is correct', {

  #browser();

  for(dimension in 1:6){
    d1 <- BinaryDomain$new(dimension = dimension);
    expect_equal(object = d1$get_length(), expected = 2 ^ dimension);
  }
});

