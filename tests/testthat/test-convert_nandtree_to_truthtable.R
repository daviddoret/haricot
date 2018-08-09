

#install.packages("testthat");
library(testthat);
library(data.table);

test_that('convert_nandtree_to_truthtable: test 1', {

  # Very basic test, take a simplistic NandTree,
  # convert it to a TruthTable, and test all outputs.

  #browser();

  nt <- NandTree$new(input_dimension = 2, output_dimension = 1);
  nt$set_nand_subnode(subalgo_id = "n1", param1_id = "i1", param2_id = "i2");
  nt$set_output_subnode(subalgo_id = "o1", param1_id = "n1");

  expect_equal(nt$exec("00"), "1");
  expect_equal(nt$exec("10"), "1");
  expect_equal(nt$exec("01"), "1");
  expect_equal(nt$exec("11"), "0");

  tt <- convert_nandtree_to_truthtable(nt);

  expect_equal(tt$exec("00"), "1");
  expect_equal(tt$exec("10"), "1");
  expect_equal(tt$exec("01"), "1");
  expect_equal(tt$exec("11"), "0");

});

