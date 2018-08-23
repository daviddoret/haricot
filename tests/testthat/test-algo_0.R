require(testthat);

context("algo_0 R6 class");

test_that('algo_0: exhaustive output test', {

  #browser();

  a <- algo_0$new();

  expect_equal(a$exec(""), "0");

});


test_that('algo_0: test in composition', {

  #browser();

  composite <- algo_composite$new(0,1);

  a0 <- algo_0$new();

  composite$set_component(node = a0);
  composite$set_inner_edge(a0, "o1", composite, "o1");

  #composite$plot();

  expect_equal(composite$exec(""), "0");

});

