#install.packages("testthat");
library(testthat);

context("algo_composite R6 class");

test_that('algo_composite: faking NAND', {

  #browser();

  dim_i <- 2;
  dim_o <- 1;

  super_a <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o);

  expect_equal(super_a$get_dim_i(), dim_i);
  expect_equal(super_a$get_dim_o(), dim_o);

  nand1 <- algo_nand$new();

  super_a$set_component(node = nand1);

  super_a$set_dag_edge(super_a, "i1", nand1, "i1");
  super_a$set_dag_edge(super_a, "i2", nand1, "i2");
  super_a$set_dag_edge(nand1, "o1", super_a, "o1");

  # a$plot();

  expect_equal(super_a$exec("00"), "1");
  expect_equal(super_a$exec("10"), "1");
  expect_equal(super_a$exec("01"), "1");
  expect_equal(super_a$exec("11"), "0");

});

test_that('CompositeAlgo: faking NOT', {

  #browser();

  dim_i <- 1;
  dim_o <- 1;

  super_a <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o);

  expect_equal(super_a$get_dim_i(), dim_i);
  expect_equal(super_a$get_dim_o(), dim_o);

  nand1 <- algo_nand$new();

  super_a$set_component(node = nand1);

  super_a$set_dag_edge(super_a, "i1", nand1, "i1");
  super_a$set_dag_edge(super_a, "i1", nand1, "i2");
  super_a$set_dag_edge(nand1, "o1", super_a, "o1");

  # a$plot();

  expect_equal(super_a$exec("0"), "1");
  expect_equal(super_a$exec("1"), "0");

});


test_that('CompositeAlgo: basic composition', {

  #browser();

  dim_i <- 4;
  dim_o <- 3;

  super_a <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o);

  expect_equal(super_a$get_dim_i(), dim_i);
  expect_equal(super_a$get_dim_o(), dim_o);

  nand1 <- algo_nand$new();
  nand2 <- algo_nand$new();
  nand3 <- algo_nand$new();

  super_a$set_component(node = nand1);
  super_a$set_component(node = nand2);
  super_a$set_component(node = nand3);

  super_a$set_dag_edge(super_a, "i1", nand1, "i1");
  super_a$set_dag_edge(super_a, "i2", nand1, "i2");
  super_a$set_dag_edge(super_a, "i3", nand2, "i1");
  super_a$set_dag_edge(super_a, "i4", nand2, "i2");
  super_a$set_dag_edge(nand1, "o1", nand3, "i1");
  super_a$set_dag_edge(nand2, "o1", nand3, "i2");
  super_a$set_dag_edge(nand1, "o1", super_a, "o1");
  super_a$set_dag_edge(nand2, "o1", super_a, "o2");
  super_a$set_dag_edge(nand3, "o1", super_a, "o3");

});

test_that('CompositeAlgo: super composite', {

  #browser();

  dim_i <- 2;
  dim_o <- 1;

  super_a <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o);

  not1 <- algo_not$new();
  not2 <- algo_not$new();
  or1 <- algo_or$new();

  super_a$set_component(node = not1);
  super_a$set_dag_edge(super_a, "i1", not1, "i1");

  super_a$set_component(node = not2);
  super_a$set_dag_edge(super_a, "i2", not2, "i1");

  super_a$set_component(node = or1);
  super_a$set_dag_edge(not1, "o1", or1, "i1");
  super_a$set_dag_edge(not2, "o1", or1, "i2");

  super_a$set_dag_edge(or1, "o1", super_a, "o1");

  super_a$plot();

  expect_equal(super_a$exec("00"), "1");
  expect_equal(super_a$exec("10"), "1");
  expect_equal(super_a$exec("01"), "1");
  expect_equal(super_a$exec("11"), "0");

});

