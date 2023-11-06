test_that("get_jags_model works", {
  expect_error(get_jags_model())
  expect_error(get_jags_model(base_filename = "amre"))
})
