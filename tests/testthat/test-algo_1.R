require(testthat);

context("algo_1 R6 class");

test_that('algo_1: exhaustive output test', {

  #browser();

  a <- algo_1$new();

  expect_equal(a$exec(""), "1");

});

test_that('algo_1: test in composition', {

  #browser();

  composite <- algo_composite$new(0,1);

  a1 <- algo_1$new();

  composite$set_inner_node(node = a1);
  composite$set_inner_edge(a1, "o1", composite, "o1");

  #composite$plot();

  expect_equal(composite$exec(""), "1");

});
