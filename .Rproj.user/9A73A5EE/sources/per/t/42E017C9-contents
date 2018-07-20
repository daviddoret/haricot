

#install.packages("testthat");
library(testthat);

test_that('TruthTable_FlexOutput basic reactions', {

  #browser();

  t1 <- TruthTable_FlexOutput$new(input_dimension = 2, output_dimension = 2);
  t1$set_output(input = "01", output = "11");
  expect_equal(object = t1$do_apply_algorithm("01"), expected = "11");
  expect_equal(object = t1$do_apply_algorithm("10"), expected = "00");
});

test_that("TruthTable_FlexOutput inversion yield correct results", {

  #browser();

  for(iteration in 1:32){

  input_dimension = sample(x = 1:8, size = 1);
  output_dimension = sample(x = 1:8, size = 1);

  t1 <- TruthTable_FlexOutput$new(input_dimension, output_dimension);
  t1$do_randomize_outputs();
  #print(t1);

  random_input_integer <- sample(x = 0:2 ^ input_dimension - 1, size = 1);
  random_input_logical_vector <- convert_int_to_logical_vector(random_input_integer, size = input_dimension);

  output <- t1$do_apply_algorithm(random_input_logical_vector);

  t2 <- t1$get_inverse();
  #print(t2);

  inverse_output <- t2$do_apply_algorithm(output);

  new_input <- inverse_output;
  new_output <- t1$do_apply_algorithm(new_input);

  expect_equal(output, new_output, info = c(t1, t2, random_input_logical_vector));

  }

})



