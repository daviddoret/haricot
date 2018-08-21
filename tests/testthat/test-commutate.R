
library(testthat);

context("commutate");

test_that('commutate: test 01', {

  #browser();

  algo_0 <- algo_10$new();
  algo_1 <- algo_01$new();

  commutated <- commutate(algo_0, algo_1);

  expect_equal(commutated$exec("00"), "1");
  expect_equal(commutated$exec("10"), "0");
  expect_equal(commutated$exec("01"), "0");
  expect_equal(commutated$exec("11"), "1");

  commutated$plot();

  });

test_that('commutate: random sampling', {

  #browser();

  for(i in 1:6){

  # Pick random dimension
  d1 <- sample(x = 1:8, size = 1, replace = TRUE);

  # Populate 2 random truth tables
  a0 <- algo_tt$new(d1, 1)$do_randomize_outputs();
  a1 <- algo_tt$new(d1, 1)$do_randomize_outputs();

  # Pick a random input value with one extrabit
  random_input <- bnum$new(dim = d1)$randomize();

  # Pick a random switch decision
  random_decision <- sample(x = 0:1, size = 1, replace = TRUE);

  # Get the solution from the normal approach
  solution <- NA;
  if(random_decision == 0){
    solution <- a0$exec(random_input);
  } else {
    solution <- a1$exec(random_input);
  }

  # Setup the switch
  commutated <- commutate(a0, a1);

  raw <- as.logical(c(random_input$get_logical_vector(), random_decision));
  switch_input <- bnum$new(input = raw);

  switch_solution <- commutated$exec(switch_input);

  expect_true(equal_bnum(solution, switch_solution));

  #algo_switch$plot();

  }

});


