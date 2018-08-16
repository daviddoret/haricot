
library(testthat);

context("dissolve_tt");

test_that('dissolve_tt: test 01', {

  #browser();

  # Pick a random input dimension
  dim_i <- sample(x = 2:6, size = 1, replace = TRUE);

  # Pick a random output dimension
  dim_o <- sample(x = 2:6, size = 1, replace = TRUE);

  # Create a truth table algorithm of desired dimensions
  truthtable_algo <- algo_tt$new(input_dimension = dim_i, output_dimension = dim_o);

  # Randomize the truth table outputs, we end up with a random deterministic algorithm
  truthtable_algo $do_randomize_outputs();

  # Split the random truth table algorithm and retrieve the resulting composite algorithm
  dissolved_algo <- dissolve_tt(truthtable_algo);

  dissolved_algo$plot();

  #algo$plot();
  #algo_comp$plot();
  #n <- bnum$new(dim = dim_i);
  #repeat{
  #  expect_equal(truthtable_algo
  #             $exec(n), splitted_algo$exec(n));
  #  n$do_increment();
  #  if(n$get_equal_0()){
  #    break;
  #  }
  #}

  });
