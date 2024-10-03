test_that("create_config works", {
  # Load data as bad object example
  data(carnivora)

  # Expect equal
  #expect_equal(nrow(ages(dat = dat)), nrow(dat))

  # Expect error
  expect_error(create_config())

  expect_error(create_config(file_prefix = "files"))

  expect_error(create_config(file_prefix = "files",
                             data_file = carnivora))

  expect_error(create_config(file_prefix = "files",
                             data_file = "carnivora.txt"))

  expect_error(create_config(file_prefix = "files",
                             data_file = "carnivora.csv"))

  expect_error(create_config(file_prefix = "files",
                             data_file = "carnivora.csv",
                             bins = c("A", "B", "C")))

  expect_error(create_config(file_prefix = "files",
                             data_file = "carnivora.csv",
                             bins = c(66, 23, 2.6, 0),
                             n_areas = "test"))

  expect_error(create_config(file_prefix = "files",
                             data_file = "carnivora.csv",
                             bins = c(66, 23, 2.6, 0),
                             n_areas = 5,
                             autotune = "test"))
})
