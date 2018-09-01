require(testthat);
require(rjson);

context("json");

test_that('algo_nand', {

  a1 <- algo_nand$new(label = "test1");
  json <- to_json(a1);
  a2 <- from_json(json);

  # Test key properties.
  expect_equal(a1$get_algo_id(), a2$get_algo_id());
  expect_equal(a1$get_label(), a2$get_label());
  expect_equal(a1$get_dim_i(), a2$get_dim_i());
  expect_equal(a1$get_dim_o(), a2$get_dim_o());

  # The JSON re-exportation must be equal to the original.
  expect_equal(a1$to_json(), a2$to_json());

});

test_that('algo_0', {

  a1 <- algo_0$new(label = "test1");
  json <- to_json(a1);
  a2 <- from_json(json);

  # Test key properties.
  expect_equal(a1$get_algo_id(), a2$get_algo_id());
  expect_equal(a1$get_label(), a2$get_label());
  expect_equal(a1$get_dim_i(), a2$get_dim_i());
  expect_equal(a1$get_dim_o(), a2$get_dim_o());

  # The JSON re-exportation must be equal to the original.
  expect_equal(a1$to_json(), a2$to_json());

});

test_that('algo_1', {

  a1 <- algo_1$new(label = "test1");
  json <- to_json(a1);
  a2 <- from_json(json);

  # Test key properties.
  expect_equal(a1$get_algo_id(), a2$get_algo_id());
  expect_equal(a1$get_label(), a2$get_label());
  expect_equal(a1$get_dim_i(), a2$get_dim_i());
  expect_equal(a1$get_dim_o(), a2$get_dim_o());

  # The JSON re-exportation must be equal to the original.
  expect_equal(a1$to_json(), a2$to_json());

});

test_that('algo_tt', {

  a1 <- algo_tt$new(
    label = "test1",
    dim_i = sample(0:4, 1),
    dim_o = sample(1:8, 1));
  a1$do_randomize_outputs();
  json <- to_json(a1);
  a2 <- from_json(json);

  # Test key properties.
  expect_equal(a1$get_algo_id(), a2$get_algo_id());
  expect_equal(a1$get_label(), a2$get_label());
  expect_equal(a1$get_dim_i(), a2$get_dim_i());
  expect_equal(a1$get_dim_o(), a2$get_dim_o());

  # The JSON re-exportation must be equal to the original.
  expect_equal(a1$to_json(), a2$to_json());

});

test_that('algo_composite', {

  a1 <- algo_composite$new(label = "test1");

  # Invert the first input bit.
  nand1 <- a1$add_nand(a1, "i1", a1, "i1");

  # Inverse the second input bit.
  nand2 <- a1$add_nand(a1, "i2", a1, "i2");

  # Apply NAND to the two inverses.
  nand3 <- a1$add_nand(nand1, "o1", nand2, "o1");

  # Pipe the final output.
  a1$set_dag_edge(nand3, "o1", a1, "o1");

  json <- to_json(a1);
  a2 <- from_json(json);

  # Test key properties.
  expect_equal(a1$get_algo_id(), a2$get_algo_id());
  expect_equal(a1$get_label(), a2$get_label());
  expect_equal(a1$get_dim_i(), a2$get_dim_i());
  expect_equal(a1$get_dim_o(), a2$get_dim_o());

  # The JSON re-exportation must be equal to the original.
  expect_equal(a1$to_json(), a2$to_json());

});
