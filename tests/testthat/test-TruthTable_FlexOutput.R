

#install.packages("testthat");
library(testthat);

test_that('TruthTable_FlexOutput basic reactions', {

  t1 <- TruthTable_FlexOutput$new(input_dimension = 2, output_dimension = 2, init_value = FALSE);
  t1$set_output(input = "01", output = "11");
  expect_equal(object = t1$do_apply_algorithm("01"), expected = "11");
  expect_equal(object = t1$do_apply_algorithm("10"), expected = "00");
});




