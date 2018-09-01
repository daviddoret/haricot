#install.packages("testthat");
library(testthat);

context("algo_composite resizing");

test_that('algo_composite: resize input', {

  dim_i <- 2;
  dim_o <- 3;

  super_a <- algo_composite$new(dim_i, dim_o);

  expect_equal(super_a$get_dim_i(), dim_i);
  expect_equal(super_a$get_dim_o(), dim_o);

  nand1 <- algo_nand$new();
  super_a$set_component(node = nand1);
  super_a$set_dag_edge(super_a, "i1", nand1, "i1");
  super_a$set_dag_edge(super_a, "i2", nand1, "i2");
  super_a$set_dag_edge(nand1, "o1", super_a, "o1");
  super_a$set_dag_edge(nand1, "o1", super_a, "o2");
  super_a$set_dag_edge(nand1, "o1", super_a, "o3");

  expect_equal(super_a$exec("00"), "111");
  expect_equal(super_a$exec("10"), "111");
  expect_equal(super_a$exec("01"), "111");
  expect_equal(super_a$exec("11"), "000");

  dim_i <- 4;
  super_a$set_dim_i(dim_i);
  super_a$set_dag_edge(super_a, "i3", super_a, "o2");
  super_a$set_dag_edge(super_a, "i4", super_a, "o3");

  expect_equal(super_a$get_dim_i(), dim_i);
  expect_equal(super_a$get_dim_o(), dim_o);

  expect_equal(super_a$exec("0000"), "100");
  expect_equal(super_a$exec("1010"), "110");
  expect_equal(super_a$exec("0101"), "101");
  expect_equal(super_a$exec("1111"), "011");

});
