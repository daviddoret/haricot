#install.packages("testthat");
library(testthat);

context("AlgoComposite R6 class");

test_that('AlgoComposite: faking NAND', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  super_a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(super_a$get_input_dimension(), input_dimension);
  expect_equal(super_a$get_output_dimension(), output_dimension);

  nand1 <- AlgoNAND$new();

  super_a$set_inner_node(node = nand1);

  super_a$set_inner_edge(super_a, "i1", nand1, "i1");
  super_a$set_inner_edge(super_a, "i2", nand1, "i2");
  super_a$set_inner_edge(nand1, "o1", super_a, "o1");

  # a$do_plot();

  expect_equal(super_a$do_execute("00"), "1");
  expect_equal(super_a$do_execute("10"), "1");
  expect_equal(super_a$do_execute("01"), "1");
  expect_equal(super_a$do_execute("11"), "0");

});

test_that('CompositeAlgo: faking NOT', {

  #browser();

  input_dimension <- 1;
  output_dimension <- 1;

  super_a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(super_a$get_input_dimension(), input_dimension);
  expect_equal(super_a$get_output_dimension(), output_dimension);

  nand1 <- AlgoNAND$new();

  super_a$set_inner_node(node = nand1);

  super_a$set_inner_edge(super_a, "i1", nand1, "i1");
  super_a$set_inner_edge(super_a, "i1", nand1, "i2");
  super_a$set_inner_edge(nand1, "o1", super_a, "o1");

  # a$do_plot();

  expect_equal(super_a$do_execute("0"), "1");
  expect_equal(super_a$do_execute("1"), "0");

});


test_that('CompositeAlgo: basic composition', {

  #browser();

  input_dimension <- 4;
  output_dimension <- 3;

  super_a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  expect_equal(super_a$get_input_dimension(), input_dimension);
  expect_equal(super_a$get_output_dimension(), output_dimension);

  nand1 <- AlgoNAND$new();
  nand2 <- AlgoNAND$new();
  nand3 <- AlgoNAND$new();

  super_a$set_inner_node(node = nand1);
  super_a$set_inner_node(node = nand2);
  super_a$set_inner_node(node = nand3);

  super_a$set_inner_edge(super_a, "i1", nand1, "i1");
  super_a$set_inner_edge(super_a, "i2", nand1, "i2");
  super_a$set_inner_edge(super_a, "i3", nand2, "i1");
  super_a$set_inner_edge(super_a, "i4", nand2, "i2");
  super_a$set_inner_edge(nand1, "o1", nand3, "i1");
  super_a$set_inner_edge(nand2, "o1", nand3, "i2");
  super_a$set_inner_edge(nand1, "o1", super_a, "o1");
  super_a$set_inner_edge(nand2, "o1", super_a, "o2");
  super_a$set_inner_edge(nand3, "o1", super_a, "o3");

});

test_that('CompositeAlgo: super composite', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  super_a <- AlgoComposite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension);

  not1 <- AlgoNOT$new();
  not2 <- AlgoNOT$new();
  or1 <- AlgoOR$new();

  super_a$set_inner_node(node = not1);
  super_a$set_inner_edge(super_a, "i1", not1, "i1");

  super_a$set_inner_node(node = not2);
  super_a$set_inner_edge(super_a, "i2", not2, "i1");

  super_a$set_inner_node(node = or1);
  super_a$set_inner_edge(not1, "o1", or1, "i1");
  super_a$set_inner_edge(not2, "o1", or1, "i2");

  super_a$set_inner_edge(or1, "o1", super_a, "o1");

  super_a$do_plot();

  expect_equal(super_a$do_execute("00"), "1");
  expect_equal(super_a$do_execute("10"), "1");
  expect_equal(super_a$do_execute("01"), "1");
  expect_equal(super_a$do_execute("11"), "0");

});

