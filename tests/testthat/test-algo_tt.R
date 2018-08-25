require(futile.logger);
require(testthat);

context("algo_tt");

test_that('algo_tt: all zeroes tt return all zeroes.', {

  for(i in 1:6){

  # Pick a random input dimension
  dim_i <- sample(x = 0:4, size = 1, replace = TRUE);

  # Pick a random output dimension
  dim_o <- sample(x = 1:4, size = 1, replace = TRUE);

  # Create a truth table algorithm of desired dimensions
  tt <- algo_tt$new(dim_i = dim_i, dim_o = dim_o);

  #algo$plot();
  #algo_comp$plot();

  n <- bnum$new(dim = dim_i);
  repeat{

    expect_true(equal_bnum(tt$exec(n), bnum$new(dim = dim_o)));

    n$do_increment();
    if(n$get_equal_0()){
      break;
    }
  }

    }

  });

test_that('algo_tt: basic composition.', {

  #browser();

    dim_i <- 2;
    dim_o <- 1;

    # Create a truth table algorithm of desired dimensions
    tt <- algo_tt$new(dim_i = dim_i, dim_o = dim_o);

    compi <- algo_composite$new(2,1);
    compi$set_component(tt);
    compi$set_dag_edge(compi, "i1", tt, "i1");
    compi$set_dag_edge(compi, "i2", tt, "i2");
    compi$set_dag_edge(tt, "o1", compi, "o1");

    #algo$plot();
    #algo_comp$plot();

    n <- bnum$new(dim = dim_i);
    repeat{

      expect_true(equal_bnum(tt$exec(n), bnum$new(dim = dim_o)));

      n$do_increment();
      if(n$get_equal_0()){
        break;
      }
    }

});

test_that('algo_tt: composition with constant.', {

  #browser();

  dim_i <- 0;
  dim_o <- 4;

  # Create a truth table algorithm of desired dimensions
  tt <- algo_tt$new(dim_i = dim_i, dim_o = dim_o);

  compi <- algo_composite$new(dim_i, dim_o);
  compi$set_component(tt);
  compi$set_dag_edge(tt, "o1", compi, "o1");
  compi$set_dag_edge(tt, "o2", compi, "o2");
  compi$set_dag_edge(tt, "o3", compi, "o3");
  compi$set_dag_edge(tt, "o4", compi, "o4");

  #algo$plot();
  #algo_comp$plot();

  n <- bnum$new(dim = dim_i);
  repeat{

    expect_true(equal_bnum(tt$exec(n), bnum$new(dim = dim_o)));

    n$do_increment();
    if(n$get_equal_0()){
      break;
    }
  }
});
