test_that("standrecon() works on standrecon_example_data and returns expected structure", {

  data("standrecon_example_data")

  out <- standrecon(
    data = standrecon_example_data,
    meas_year = 2025,
    ref_year = c(1950, 1975),
    avg_inc_vec = c(PIEN = 0.5, ABBI = 0.3, PIPO = 0.4),
    plot_size = 400
  )


  # returns a list
  expect_type(out, "list")
})
