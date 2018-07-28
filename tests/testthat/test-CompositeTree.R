#install.packages("testthat");
library(testthat);

test_that('CompositeTree initializes correctly', {

  #browser();

  node_id <- "st1";
  input_dimension <- 4;
  output_dimension <- 7;

  ct <- CompositeTree$new(
    node_id = node_id,
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(ct$get_node_id(), node_id);
  expect_equal(ct$get_input_dimension(), input_dimension);
  expect_equal(ct$get_output_dimension(), output_dimension);

});

