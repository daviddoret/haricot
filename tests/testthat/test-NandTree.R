

#install.packages("testthat");
library(testthat);
library(data.table);

test_that('NandTree: stupid tree without Nand subnodes works properly', {

  #browser();

  nt <- NandTree$new(input_dimension = 2, output_dimension = 2);
  nt$set_output_subnode(subnode_id = "o1", param1_id = "i1");
  nt$set_output_subnode(subnode_id = "o2", param1_id = "i2");
  #print(nt);
  expect_equal(nt$do_execute("00"), "00", info = "00==00");
  expect_equal(nt$do_execute("10"), "10");
  expect_equal(nt$do_execute("01"), "01");
  expect_equal(nt$do_execute("11"), "11");

});

test_that('NandTree: stupid tree mimicking a single Nand works properly', {

  #browser();

  nt <- NandTree$new(input_dimension = 2, output_dimension = 1);
  nt$set_nand_subnode(subnode_id = "n1", param1_id = "i1", param2_id = "i2");
  nt$set_output_subnode(subnode_id = "o1", param1_id = "n1");
  #print(nt);
  expect_equal(nt$do_execute("00"), "1");
  expect_equal(nt$do_execute("10"), "1");
  expect_equal(nt$do_execute("01"), "1");
  expect_equal(nt$do_execute("11"), "0");

});


test_that('NandTree: Nand of single input subnode invert that subnode', {

  #browser();

  nt <- NandTree$new(input_dimension = 1, output_dimension = 1);
  n1 <- nt$set_nand_subnode(param1_id = "i1", param2_id = "i1");
  nt$set_output_subnode(subnode_id = "o1", param1_id = "n1");
  #print(nt);
  expect_equal(nt$do_execute("0"), "1");
  expect_equal(nt$do_execute("1"), "0");

  });

