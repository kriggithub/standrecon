test_that("standrecon() works on standrecon_example_data and returns a data frame", {

  data("standrecon_example_data")

  out <- standrecon(
    data = standrecon_example_data,
    meas_year = 2025,
    ref_year = c(1950, 1975),
    avg_inc_vec = c(PIEN = 0.5, ABBI = 0.3, PIPO = 0.4),
    plot_size = 400
  )


  # returns a data frame
  expect_s3_class(out, "data.frame")

  # data frame has expected columns
  expect_true(all(c(
    "type", "year", "percentile",
    "species", "basal_area", "stem_density"
  ) %in% names(out)))

  # data frame contains results
  expect_true(nrow(out) > 0)

  # data frame should have filled out species
  expect_false(any(is.na(out$species)))
})
