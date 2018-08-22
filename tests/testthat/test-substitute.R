#install.packages("testthat");
library(testthat);

context("substitute function");

test_that('substite: basic test', {

  #browser();

  input_dimension <- 2;
  output_dimension <- 1;

  compi <- algo_composite$new(
    input_dimension = input_dimension,
    output_dimension = output_dimension,
    label = "compi");

  orig <- algo_nand$new(label = "orig");

  compi$set_inner_node(node = orig);

  compi$set_inner_edge(compi, "i1", orig, "i1");
  compi$set_inner_edge(compi, "i2", orig, "i2");
  compi$set_inner_edge(orig, "o1", compi, "o1");

  compi$plot();

  expect_equal(compi$exec("00"), "1");
  expect_equal(compi$exec("10"), "1");
  expect_equal(compi$exec("01"), "1");
  expect_equal(compi$exec("11"), "0");

  subst <- algo_and$new(label = "subst");

  substitute(compi, orig, subst);

  compi$plot();

  expect_equal(compi$exec("00"), "0");
  expect_equal(compi$exec("10"), "0");
  expect_equal(compi$exec("01"), "0");
  expect_equal(compi$exec("11"), "1");

});
