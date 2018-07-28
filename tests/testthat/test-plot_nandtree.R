

#install.packages("testthat");
library(testthat);

test_that('NandTree: plot', {

  #browser();

  nt <- NandTree_FirstIdea$new(input_dimension = 4, output_dimension = 4);
  nt$set_nand_subnode(subnode_id = "n1", param1 = "i1", param2_id = "i2");
  nt$set_nand_subnode(subnode_id = "n2", param1 = "i3", param2_id = "i4");
  nt$set_nand_subnode(subnode_id = "n3", param1 = "n1", param2_id = "n2");
  nt$set_nand_subnode(subnode_id = "n4", param1 = "i1", param2_id = "n3");
  nt$set_nand_subnode(subnode_id = "n5", param1 = "i2", param2_id = "n4");
  nt$set_nand_subnode(subnode_id = "n6", param1 = "i3", param2_id = "n5");
  nt$set_output_subnode(subnode_id = "o1", param1_id = "n3");
  nt$set_output_subnode(subnode_id = "o2", param1_id = "n4");
  nt$set_output_subnode(subnode_id = "o3", param1_id = "n5");
  nt$set_output_subnode(subnode_id = "o4", param1_id = "n6");
  nt$do_plot();

  expect_equal(object = 1, expected = 1);
});

