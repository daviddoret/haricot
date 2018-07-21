

#install.packages("testthat");
library(testthat);

test_that('BinaryDomain length is correct', {

  #browser();

  nt <- NandTree_FirstIdea$new(input_dimension = 2, output_dimension = 2);
  n1 <- nt$set_nand_node(param1_id = "i1", param2_id = "i2");
  nt$set_output_node(node_id = "o1", param1_id = "n1");
  nt$set_output_node(node_id = "o2", param1_id = "i1");
  nt$logical_datatable;
  print(nt$logical_datatable);
  nt$do_apply_algorithm("01");

});

