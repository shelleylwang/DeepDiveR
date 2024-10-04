test_that("edit_config works", {

  # Expect error
  expect_error(edit_config())

  expect_error(edit_config(config = "files"))

})
