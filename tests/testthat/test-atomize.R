
library(testthat);

context("atomize");

test_that('atomize: test 01', {

  for(dim_i in 0:3){
    for(dim_o in 1:3){

  # Create a truth table algorithm of desired dimensions
  truthtable_algo <- algo_tt$new(dim_i = dim_i, dim_o = dim_o);

  # Randomize the truth table outputs, we end up with a random deterministic algorithm
  truthtable_algo$do_randomize_outputs();

  # Split the random truth table algorithm and retrieve the resulting composite algorithm
  atomized_algo <- atomize(truthtable_algo);

  atomized_algo$plot(interactive = TRUE);

  n <- bnum$new(dim = dim_i);
  repeat{
    expect_equal(truthtable_algo$exec(n), atomized_algo$exec(n));
    n$do_increment();
    if(n$get_equal_0()){
      break;
    }
  };
    }
    }
  });
