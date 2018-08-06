
library(testthat);

context("AlgoTTxxxx R6 class");

test_that('AlgoTTxxxx: exhaustive output test', {

  #browser();

  n1 <- MoBiNum$new("0000");

  repeat{

    a1 <- AlgoTTxxxx$new(n1);

    expect_equal(a1$do_execute("00"), n1$get_bit(1));
    expect_equal(a1$do_execute("10"), n1$get_bit(2));
    expect_equal(a1$do_execute("01"), n1$get_bit(3));
    expect_equal(a1$do_execute("11"), n1$get_bit(4));

    n1$do_increment();
    if(n1$get_equal_0()){
      break;
    }
  }

});
