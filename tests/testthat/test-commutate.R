
library(testthat);

context("commutate");

test_that('commutate: basic test', {

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

test_that('commutate: commute random truth tables on small input dimensions', {

  #browser();

  for(dim_i in 0:3){

  # Populate 2 random truth tables
  a0 <- algo_tt$new(dim_i, 1)$do_randomize_outputs();
  a1 <- algo_tt$new(dim_i, 1)$do_randomize_outputs();

  # Setup the switch
  commutated <- commutate(a0, a1);

  # Iterate through all possible input values
  input <- bnum$new(dim = dim_i);
  repeat{

    # Compare the output of the original 0 algo,
    # with the output of the commutated algo.
    input_with_0 <- concat_bnum(input, bnum$new("0"));
    expect_true(equal_bnum(
      a0$exec(input),
      commutated$exec(input_with_0)));

    # Compare the output of the original 1 algo,
    # with the output of the commutated algo.
    input_with_1 <- concat_bnum(input, bnum$new("1"));
    expect_true(equal_bnum(
      a1$exec(input),
      commutated$exec(input_with_1)));

    input$do_increment();
    if(input$get_equal_0()){
      break;
      };
    };
  };
});


