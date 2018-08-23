#install.packages("testthat");
library(testthat);

context("substitute function");

test_that('substite: substitute a nand', {

  #browser();

  dim_i <- 2;
  dim_o <- 1;

  compi <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o,
    label = "compi");

  orig <- algo_nand$new(label = "orig");

  compi$set_component(node = orig);

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

test_that('substite: substitute a nand with multiple components', {

  #browser();

  dim_i <- 2;
  dim_o <- 2;

  compi <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o,
    label = "compi");

  orig <- algo_nand$new(label = "orig");
  compi$set_component(node = orig);
  compi$set_inner_edge(compi, "i1", orig, "i1");
  compi$set_inner_edge(compi, "i2", orig, "i2");
  compi$set_inner_edge(orig, "o1", compi, "o1");

  other <- algo_nand$new(label = "other");
  compi$set_component(node = other);
  compi$set_inner_edge(compi, "i1", other, "i1");
  compi$set_inner_edge(compi, "i2", other, "i2");
  compi$set_inner_edge(other, "o1", compi, "o2");

  compi$plot();

  expect_equal(compi$exec("00"), "11");
  expect_equal(compi$exec("10"), "11");
  expect_equal(compi$exec("01"), "11");
  expect_equal(compi$exec("11"), "00");

  subst <- algo_and$new(label = "subst");

  substitute(compi, orig, subst);

  compi$plot();

  expect_equal(compi$exec("00"), "01");
  expect_equal(compi$exec("10"), "01");
  expect_equal(compi$exec("01"), "01");
  expect_equal(compi$exec("11"), "10");

});

test_that('substite: substitute a truthtable with multiple components', {

  #browser();

  dim_i <- 2;
  dim_o <- 2;

  compi <- algo_composite$new(
    dim_i = dim_i,
    dim_o = dim_o,
    label = "compi");

  orig <- algo_tt$new(2, 1, label = "orig");
  orig$do_randomize_outputs();
  compi$set_component(node = orig);
  compi$set_inner_edge(compi, "i1", orig, "i1");
  compi$set_inner_edge(compi, "i2", orig, "i2");
  compi$set_inner_edge(orig, "o1", compi, "o1");

  other <- algo_tt$new(2, 1, label = "other");
  other$do_randomize_outputs();
  compi$set_component(node = other);
  compi$set_inner_edge(compi, "i1", other, "i1");
  compi$set_inner_edge(compi, "i2", other, "i2");
  compi$set_inner_edge(other, "o1", compi, "o2");

  compi$plot();

  subst <- algo_and$new(label = "subst");

  substitute(compi, orig, subst);

  compi$plot();

});
