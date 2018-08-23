#install.packages("testthat");
library(testthat);

context("swap");

test_that('swap: test 01', {

  #browser();

  dim_i <- 2;
  dim_o <- 1;

  composite <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o);

  pipe1 <- algo_01$new();
  composite$set_component(node = pipe1);
  composite$set_inner_edge(composite, "i1", pipe1, "i1");

  pipe2 <- algo_01$new();
  composite$set_component(node = pipe2);
  composite$set_inner_edge(composite, "i2", pipe2, "i1");

  old <- algo_0110$new(label = "old");
  composite$set_component(node = old);
  composite$set_inner_edge(pipe1, "o1", old, "i1");
  composite$set_inner_edge(pipe2, "o1", old, "i2");

  pipe3 <- algo_01$new();
  composite$set_component(node = pipe3);
  composite$set_inner_edge(old, "o1", pipe3, "i1");

  composite$set_inner_edge(pipe3, "o1", composite, "o1");

  composite$plot();

  expect_equal(composite$exec("00"), "0");
  expect_equal(composite$exec("10"), "1");
  expect_equal(composite$exec("01"), "1");
  expect_equal(composite$exec("11"), "0");

  new <- algo_1100$new(label = "new");

  swap(composite, old, new);

  composite$plot();

  expect_equal(composite$exec("00"), "1");
  expect_equal(composite$exec("10"), "1");
  expect_equal(composite$exec("01"), "0");
  expect_equal(composite$exec("11"), "0");

});
