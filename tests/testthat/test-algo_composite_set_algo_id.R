library(testthat);

context("algo_composite set_algo_id()");

test_that('algo_composite: set_algo_id()', {

  dim_i <- 2;
  dim_o <- 3;
  id1 <- get_node_guid();
  a1 <- algo_composite$new(dim_i, dim_o, algo_id = id1);

  nand1 <- algo_nand$new();
  a1$set_component(node = nand1);
  a1$set_dag_edge(a1, "i1", nand1, "i1");
  a1$set_dag_edge(a1, "i2", nand1, "i2");
  a1$set_dag_edge(nand1, "o1", a1, "o1");
  a1$set_dag_edge(nand1, "o1", a1, "o2");
  a1$set_dag_edge(nand1, "o1", a1, "o3");

  expect_equal(a1$get_algo_id(), id1);

  expect_equal(a1$exec("00"), "111");
  expect_equal(a1$exec("10"), "111");
  expect_equal(a1$exec("01"), "111");
  expect_equal(a1$exec("11"), "000");

  id2 <- get_node_guid();
  a1$set_algo_id(id2);
  expect_equal(a1$get_algo_id(), id2);

  expect_equal(a1$exec("00"), "111");
  expect_equal(a1$exec("10"), "111");
  expect_equal(a1$exec("01"), "111");
  expect_equal(a1$exec("11"), "000");

});
