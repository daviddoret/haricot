
library(testthat);

context("split_algo_tt");

test_that('split_algo_tt: test 01', {

  #browser();

  dim_i <- sample(x = 2:6, size = 1, replace = TRUE);
  dim_o <- sample(x = 2:6, size = 1, replace = TRUE);
  algo <- algo_tt$new(input_dimension = dim_i, output_dimension = dim_o);
  algo$do_randomize_outputs();


  algo_comp <- split_algo_tt(algo);

  #algo$plot();
  #algo_comp$plot();

  n <- bnum$new(dim = dim_i);
  repeat{

    expect_equal(algo$exec(n), algo_comp$exec(n));

    n$do_increment();
    if(n$get_equal_0()){
      break;
    }
  }

  });
