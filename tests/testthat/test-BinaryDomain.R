

#install.packages("testthat");
library(testthat);

test_that('BinaryDomain length is correct', {

  for(dimension in 1:6){
    d1 <- BinaryDomain$new(dimension = dimension);
    #d1$do_print();
    expect_equal(object = d1$get_length(), expected = 2 ^ dimension);
  }
});

