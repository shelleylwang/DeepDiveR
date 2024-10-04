test_that("areas_matrix works", {

  # Expect error
  expect_error(areas_matrix())

  expect_error(areas_matrix(config = "files"))

})
