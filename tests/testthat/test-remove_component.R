#install.packages("testthat");
library(testthat);

context("remove_component");

test_that('remove_component: test 01', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  composite <- algo_composite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  pipe1 <- algo_01$new();
  composite$set_inner_node(node = pipe1);
  composite$set_inner_edge(composite, "i1", pipe1, "i1");

  pipe2 <- algo_01$new();
  composite$set_inner_node(node = pipe2);
  composite$set_inner_edge(composite, "i2", pipe2, "i1");

  remove_me <- algo_0110$new(label = "remove_me");
  composite$set_inner_node(node = remove_me);
  composite$set_inner_edge(pipe1, "o1", remove_me, "i1");
  composite$set_inner_edge(pipe2, "o1", remove_me, "i2");

  pipe3 <- algo_01$new();
  composite$set_inner_node(node = pipe3);
  composite$set_inner_edge(remove_me, "o1", pipe3, "i1");

  composite$set_inner_edge(pipe3, "o1", composite, "o1");

  composite$plot();

  expect_equal(composite$exec("00"), "0");
  expect_equal(composite$exec("10"), "1");
  expect_equal(composite$exec("01"), "1");
  expect_equal(composite$exec("11"), "0");

  remove_component(composite, remove_me);

  composite$plot();

  add_me <- algo_0110$new(label = "add_me");
  composite$set_inner_node(node = add_me);
  composite$set_inner_edge(pipe1, "o1", add_me, "i1");
  composite$set_inner_edge(pipe2, "o1", add_me, "i2");

  # Re-pipe
  composite$set_inner_edge(add_me, "o1", pipe3, "i1");

  composite$plot();

  expect_equal(composite$exec("00"), "0");
  expect_equal(composite$exec("10"), "1");
  expect_equal(composite$exec("01"), "1");
  expect_equal(composite$exec("11"), "0");

});
