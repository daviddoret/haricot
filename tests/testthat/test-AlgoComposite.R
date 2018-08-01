#install.packages("testthat");
library(testthat);

test_that('CompositeAlgo test 01', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(a$get_input_dimension(), input_dimension);
  expect_equal(a$get_output_dimension(), output_dimension);

  a$do_plot();

  nand1 <- AlgoNAND$new();
  nand1$get_node_id();
  nand1$do_plot();

  a$set_inner_node(node = nand1);

  a$set_inner_edge(
    source_node_id = a$get_node_id(),
    source_bit_id = "i1",
    target_node_id = nand1$get_node_id(),
    target_bit_id = "i1");

  a$set_inner_edge(
    source_node_id = a$get_node_id(),
    source_bit_id = "i2",
    target_node_id = nand1$get_node_id(),
    target_bit_id = "i2");

  a$set_inner_edge(
    source_node_id = nand1$get_node_id(),
    source_bit_id = "o1",
    target_node_id = a$get_node_id(),
    target_bit_id = "o1");

  a$do_plot();

  #expect_equal(a$get_inner_edge_count(), 3);
  #expect_equal(a$get_inner_node_count(), 1);

});
