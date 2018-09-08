

#install.packages("testthat");
library(testthat);

context("bset R6 class")

test_that('bset length is correct', {

  #browser();

  for(dimension in 1:6){
    d1 <- bset$new(dimension = dimension);
    expect_equal(object = d1$get_length(), expected = 2 ^ dimension);
  }
});

