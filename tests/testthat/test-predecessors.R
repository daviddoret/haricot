library(testthat);

context("predecessors");

test_that('test 1', {

  dim_i <- 2;
  dim_o <- 1;
  a1 <- algo_composite$new(dim_i, dim_o);

  nand1 <- algo_nand$new();
  a1$set_component(nand1);
  a1$set_dag_edge(a1, "i1", nand1, "i1");
  a1$set_dag_edge(a1, "i2", nand1, "i2");

  nand2 <- algo_nand$new();
  a1$set_component(nand2);
  a1$set_dag_edge(a1, "i1", nand2, "i1");
  a1$set_dag_edge(nand1, "o1", nand2, "i2");

  nand3 <- algo_nand$new();
  a1$set_component(nand3);
  a1$set_dag_edge(nand2, "o1", nand3, "i1");
  a1$set_dag_edge(nand2, "o1", nand3, "i2");

  a1$set_dag_edge(nand3, "o1", a1, "o1");

  testthat::expect_equal(length(predecessors(a1, nand1)), 1);
  testthat::expect_equal(length(predecessors(a1, nand2)), 2);
  testthat::expect_equal(length(predecessors(a1, nand3)), 1);

});
