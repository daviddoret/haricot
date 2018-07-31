#install.packages("testthat");
library(testthat);

test_that('CompositeDiGraph initializes correctly', {

  #browser();

  input_dimension <- 4;
  output_dimension <- 2;

  ct <- CompositeDiGraph$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(ct$get_input_dimension(), input_dimension);
  expect_equal(ct$get_output_dimension(), output_dimension);

  i1_id <- ct$set_inner_node(inner_node = BiFun_NAND$new());
  i2_id <- ct$set_inner_node(inner_node = BiFun_OR$new());

  ct$set_inner_edge(
    source_node_id = "_self",
    source_bit_id = "i1",
    target_node_id = i1_id,
    target_bit_id = "i1");

  ct$set_inner_edge(
    source_node_id = "_self",
    source_bit_id = "i2",
    target_node_id = i1_id,
    target_bit_id = "i2");

  ct$set_inner_edge(
    source_node_id = "_self",
    source_bit_id = "i3",
    target_node_id = "NAND1",
    target_bit_id = "i1");

  ct$set_inner_edge(
    source_node_id = "_self",
    source_bit_id = "i4",
    target_node_id = "NAND1",
    target_bit_id = "i2");

  ct$set_inner_edge(
    source_node_id = "NAND1",
    source_bit_id = "o1",
    target_node_id = "_self",
    target_bit_id = "o1");

  ct$set_inner_edge(
    source_node_id = "OR1",
    source_bit_id = "o1",
    target_node_id = "_self",
    target_bit_id = "o2");

});

