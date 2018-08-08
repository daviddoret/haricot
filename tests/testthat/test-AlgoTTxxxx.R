
library(testthat);

context("algo_xxxx R6 class");

test_that('algo_xxxx: exhaustive output test', {

  #browser();

  n1 <- bnum$new("0000");

  repeat{

    a1 <- algo_xxxx$new(n1);

    expect_equal(a1$do_execute(c(FALSE,FALSE)), n1$get_bit(1), info = paste0(n1, ":00"));
    expect_equal(a1$do_execute(c(TRUE,FALSE)), n1$get_bit(2), info = paste0(n1, ":10"));
    expect_equal(a1$do_execute(c(FALSE,TRUE)), n1$get_bit(3), info = paste0(n1, ":01"));
    expect_equal(a1$do_execute(c(TRUE,TRUE)), n1$get_bit(4), info = paste0(n1, ":11"));

    n1$do_increment();
    if(n1$get_equal_0()){
      break;
    }
  }

});
