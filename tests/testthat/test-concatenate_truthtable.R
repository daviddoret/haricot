

#install.packages("testthat");
library(testthat);

test_that('concatenate_truthtable: check that the concatenation of random truthtables yields the expected output', {

  browser();

  for(i in 1:4){

    input_dimension = sample(x = c(1:6), size = 1, replace = TRUE);
    output_dimension_1 = sample(x = c(1:6), size = 1, replace = TRUE);
    output_dimension_2 = sample(x = c(1:6), size = 1, replace = TRUE);

    tt1 <- TruthTable_FlexOutput$new(input_dimension = input_dimension, output_dimension = output_dimension_1);
    tt1$do_randomize_outputs();
    print(tt1);

    tt2 <- TruthTable_FlexOutput$new(input_dimension = input_dimension, output_dimension = output_dimension_2);
    tt2$do_randomize_outputs();
    print(tt2);

    ttm <- concatenate_truthtable(tt1, tt2);
    print(ttm);

    bn <- BinaryNumber_Modular$new(input = rep(FALSE, input_dimension));

    repeat{
      i <- bn$convert_to_logical_vector();
      o1 <- tt1$do_apply_algorithm(i);
      o2 <- tt2$do_apply_algorithm(i);
      om <- ttm$do_apply_algorithm(i);
      cat(o1, " & ", o2, " = ", om, "\n");
      expect_equal(
        object = om,
        expected = c(o1, o2));
      bn$do_increment();
      if(bn$get_equal_0()){
        break;
      }
    }
  }
});

