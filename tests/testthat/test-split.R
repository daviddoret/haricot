require(testthat);
require(futile.logger);

context("split");

test_that('split: test 01', {

  #browser();
  #flog.threshold(DEBUG);

  for(i in 1:6){

  # Pick a random input dimension
  dim_i <- sample(x = 1:4, size = 1, replace = TRUE);
  flog.debug("dim_i: %s", dim_i);

  # Pick a random output dimension
  dim_o <- sample(x = 1:4, size = 1, replace = TRUE);
  flog.debug("dim_o: %s", dim_o);

  # Create a truth table algorithm of desired dimensions
  truthtable_algo <- algo_tt$new(dim_i = dim_i, dim_o = dim_o);

  # Randomize the truth table outputs, we end up with a random deterministic algorithm
  truthtable_algo$do_randomize_outputs();
  flog.debug("truthtable_algo: %s", truthtable_algo$get_prettystring());

  # Split the random truth table algorithm and retrieve the resulting composite algorithm
  splitted_algo <- split(truthtable_algo);

  #algo$plot();
  splitted_algo$plot();

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
