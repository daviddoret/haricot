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

  a$set_inner_edge(a, "i1", nand1, "i1");
  a$set_inner_edge(a, "i2", nand1, "i2");
  a$set_inner_edge(nand1, "o1", a, "o1");

  # a$do_plot();

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

  a$set_inner_edge(a, "i1", nand1, "i1");
  a$set_inner_edge(a, "i1", nand1, "i2");
  a$set_inner_edge(nand1, "o1", a, "o1");

  # a$do_plot();

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

  a$set_inner_edge(a, "i1", nand1, "i1");
  a$set_inner_edge(a, "i2", nand1, "i2");
  a$set_inner_edge(a, "i3", nand2, "i1");
  a$set_inner_edge(a, "i4", nand2, "i2");
  a$set_inner_edge(nand1, "o1", nand3, "i1");
  a$set_inner_edge(nand2, "o1", nand3, "i2");
  a$set_inner_edge(nand1, "o1", a, "o1");
  a$set_inner_edge(nand2, "o1", a, "o2");
  a$set_inner_edge(nand3, "o1", a, "o3");

  # a$do_plot();

});


