#install.packages("testthat");
library(testthat);

test_that('CompositeAlgo: faking NAND', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(a$get_input_dimension(), input_dimension);
  expect_equal(a$get_output_dimension(), output_dimension);

  nand1 <- AlgoNAND$new();

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

  expect_equal(a$do_execute("00"), "1");
  expect_equal(a$do_execute("10"), "1");
  expect_equal(a$do_execute("01"), "1");
  expect_equal(a$do_execute("11"), "0");

});


test_that('CompositeAlgo: faking NOT', {

  #browser();

  input_dimension <- 1;
  output_dimension <- 1;

  a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(a$get_input_dimension(), input_dimension);
  expect_equal(a$get_output_dimension(), output_dimension);

  nand1 <- AlgoNAND$new();

  a$set_inner_node(node = nand1);

  a$set_inner_edge(
    source_node_id = a$get_node_id(),
    source_bit_id = "i1",
    target_node_id = nand1$get_node_id(),
    target_bit_id = "i1");

  a$set_inner_edge(
    source_node_id = a$get_node_id(),
    source_bit_id = "i1",
    target_node_id = nand1$get_node_id(),
    target_bit_id = "i2");

  a$set_inner_edge(
    source_node_id = nand1$get_node_id(),
    source_bit_id = "o1",
    target_node_id = a$get_node_id(),
    target_bit_id = "o1");

  a$do_plot();

  expect_equal(a$do_execute("0"), "1");
  expect_equal(a$do_execute("1"), "0");

});


test_that('CompositeAlgo: basic composition', {

  #browser();

  input_dimension <- 4;
  output_dimension <- 3;

  a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(a$get_input_dimension(), input_dimension);
  expect_equal(a$get_output_dimension(), output_dimension);

  nand1 <- AlgoNAND$new();
  nand2 <- AlgoNAND$new();
  nand3 <- AlgoNAND$new();

  a$set_inner_node(node = nand1);
  a$set_inner_node(node = nand2);
  a$set_inner_node(node = nand3);

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
    source_node_id = a$get_node_id(),
    source_bit_id = "i3",
    target_node_id = nand2$get_node_id(),
    target_bit_id = "i1");

  a$set_inner_edge(
    source_node_id = a$get_node_id(),
    source_bit_id = "i4",
    target_node_id = nand2$get_node_id(),
    target_bit_id = "i2");

  a$set_inner_edge(
    source_node_id = nand1$get_node_id(),
    source_bit_id = "o1",
    target_node_id = nand3$get_node_id(),
    target_bit_id = "i1");

  a$set_inner_edge(
    source_node_id = nand2$get_node_id(),
    source_bit_id = "o1",
    target_node_id = nand3$get_node_id(),
    target_bit_id = "i2");

  a$set_inner_edge(
    source_node_id = nand1$get_node_id(),
    source_bit_id = "o1",
    target_node_id = a$get_node_id(),
    target_bit_id = "o1");

  a$set_inner_edge(
    source_node_id = nand2$get_node_id(),
    source_bit_id = "o1",
    target_node_id = a$get_node_id(),
    target_bit_id = "o2");

  a$set_inner_edge(
    source_node_id = nand3$get_node_id(),
    source_bit_id = "o1",
    target_node_id = a$get_node_id(),
    target_bit_id = "o3");

  a$do_plot();

});


