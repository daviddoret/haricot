#install.packages("testthat");
library(testthat);

context("algo_composite resizing");

test_that('algo_composite: resize input', {

  dim_i <- 2;
  dim_o <- 3;

  a1 <- algo_composite$new(dim_i, dim_o);

  expect_equal(a1$get_dim_i(), dim_i);
  expect_equal(a1$get_dim_o(), dim_o);

  nand1 <- algo_nand$new();
  a1$set_component(node = nand1);
  a1$set_dag_edge(a1, "i1", nand1, "i1");
  a1$set_dag_edge(a1, "i2", nand1, "i2");
  a1$set_dag_edge(nand1, "o1", a1, "o1");
  a1$set_dag_edge(nand1, "o1", a1, "o2");
  a1$set_dag_edge(nand1, "o1", a1, "o3");

  expect_equal(a1$exec("00"), "111");
  expect_equal(a1$exec("10"), "111");
  expect_equal(a1$exec("01"), "111");
  expect_equal(a1$exec("11"), "000");

  dim_i <- 4;
  a1$set_dim_i(dim_i);
  a1$set_dag_edge(a1, "i3", a1, "o2");
  a1$set_dag_edge(a1, "i4", a1, "o3");

  expect_equal(a1$get_dim_i(), dim_i);
  expect_equal(a1$get_dim_o(), dim_o);

  expect_equal(a1$exec("0000"), "100");
  expect_equal(a1$exec("1010"), "110");
  expect_equal(a1$exec("0101"), "101");
  expect_equal(a1$exec("1111"), "011");

  dim_o <- 5;
  a1$set_dim_o(dim_o);
  a1$set_dag_edge(a1, "i1", a1, "o4");
  a1$set_dag_edge(a1, "i3", a1, "o5");

  expect_equal(a1$get_dim_i(), dim_i);
  expect_equal(a1$get_dim_o(), dim_o);

  expect_equal(a1$exec("0000"), "10000");
  expect_equal(a1$exec("1010"), "11011");
  expect_equal(a1$exec("0101"), "10100");
  expect_equal(a1$exec("1111"), "01111");


});
