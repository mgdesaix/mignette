test_that("get_jags_model works", {
  expect_error(get_jags_model())
  expect_error(get_jags_model(base_filename = "amre"))
})


test_that("get_vector_abunds works", {
  expect_equal(as.numeric(get_vector_abunds(populations = terra::vect(rbind(c(0,0), c(0,5), c(5,5), c(5,0)), "polygons", crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                 abunds = terra::rast(matrix(1,nrow=10,ncol=10), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                 pop_names = "cat")[1,2]), 25)
})
