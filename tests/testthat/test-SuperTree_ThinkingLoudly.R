#install.packages("testthat");
library(testthat);

test_that('SuperTree initializes correctly', {

  #browser();

  node_id <- "st1";
  input_dimension <- 4;
  output_dimension <- 7;

  st <- SuperTree_ThinkingLoudly$new(
    node_id = node_id,
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(st$get_node_id(), node_id);
  expect_equal(st$get_input_dimension(), input_dimension);
  expect_equal(st$get_output_dimension(), output_dimension);

});

