

#install.packages("testthat");
library(testthat);

context("bdom R6 class")

test_that('bdom length is correct', {

  #browser();

  for(dimension in 1:6){
    d1 <- bdom$new(dimension = dimension);
    expect_equal(object = d1$get_length(), expected = 2 ^ dimension);
  }
});

