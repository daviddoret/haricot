

#install.packages("testthat");
library(testthat);

test_that(
  'concatenate_truthtable: check that the concatenation of random truthtables yields the expected output',
  {

  #browser();

  for(i in 1:4){

    dim_i = sample(x = c(1:6), size = 1, replace = TRUE);
    dim_o_1 = sample(x = c(1:6), size = 1, replace = TRUE);
    dim_o_2 = sample(x = c(1:6), size = 1, replace = TRUE);

    tt1 <- algo_tt$new(dim_i = dim_i, dim_o = dim_o_1);
    tt1$do_randomize_outputs();
    # print(tt1);

    tt2 <- algo_tt$new(dim_i = dim_i, dim_o = dim_o_2);
    tt2$do_randomize_outputs();
    # print(tt2);

    ttm <- concatenate_truthtable(tt1, tt2);
    # print(ttm);

    bn <- bnum$new(input = rep(FALSE, dim_i));

    repeat{
      i <- bn$convert_to_logical_vector();
      o1 <- tt1$exec(i);
      o2 <- tt2$exec(i);
      om <- ttm$exec(i);
      # cat(o1, " & ", o2, " = ", om, "\n");
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

