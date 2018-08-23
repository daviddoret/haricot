
library(testthat);

context("split");

test_that('split: test 01', {

  #browser();

  for(i in 1:6){

  # Pick a random input dimension
  dim_i <- sample(x = 1:4, size = 1, replace = TRUE);

  # Pick a random output dimension
  dim_o <- sample(x = 1:4, size = 1, replace = TRUE);

  # Create a truth table algorithm of desired dimensions
  truthtable_algo <- algo_tt$new(dim_i = dim_i, dim_o = dim_o);

  # Randomize the truth table outputs, we end up with a random deterministic algorithm
  truthtable_algo$do_randomize_outputs();

  # Split the random truth table algorithm and retrieve the resulting composite algorithm
  splitted_algo <- split(truthtable_algo);

  #algo$plot();
  #algo_comp$plot();

  n <- bnum$new(dim = dim_i);
  repeat{

    expect_equal(truthtable_algo$exec(n), splitted_algo$exec(n));

    n$do_increment();
    if(n$get_equal_0()){
      break;
    }
  }

    }

  });
