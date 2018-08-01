#install.packages("testthat");
library(testthat);

test_that('CompositeAlgo test 01', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  ct <- CompositeAlgo$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(ct$get_input_dimension(), input_dimension);
  expect_equal(ct$get_output_dimension(), output_dimension);

  ct$do_plot();

  i1_id <- ct$set_inner_node(
    inner_node = BiFun_OR$new(),
    inner_node_id = "NAND");

  ct$do_plot();

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
    source_node_id = i1_id,
    source_bit_id = "o1",
    target_node_id = "_self",
    target_bit_id = "o2");

  expect_equal(ct$get_inner_edge_count(), 3);
  expect_equal(ct$get_inner_node_count(), 1);

});
