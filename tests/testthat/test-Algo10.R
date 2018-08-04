
library(testthat);

test_that('Algo10: exhaustive output test', {

  #browser();

  a <- Algo10$new();

  #print(nt);
  #plot_nandtree(nt);

  expect_equal(a$do_execute("0"), "1");
  expect_equal(a$do_execute("1"), "0");

});
