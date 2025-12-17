test_that(".create_conclass() returns a numeric vector of same length", {
  status <- c(2, 2, 2, 3, 4, 6, 5)
  decay  <- c(1, 3, 5, NA, 7, 2, 4)

  out <- .create_conclass(status, decay)

  # Test output is a vector, same length as input
  expect_true(is.numeric(out))
  expect_length(out, length(status))

  # Test known cases are mapped correctly
  expect_equal(out[1], 3)  # status=2, decay=1, conclass 3
  expect_equal(out[2], 4)  # status=2, decay=3, conclass 4
  expect_equal(out[3], 5)  # status=2, decay=5, conclass 5
  expect_equal(out[4], 6)  # status=3, conclass 6
  expect_equal(out[5], 8)  # status=4, decay=7, conclass 8
  expect_equal(out[6], 7)  # status=6, conclass 7
  expect_equal(out[7], 7)  # status=5, conclass 7
})






test_that(".fule_decomp() correctly calculates years dead", {

  out <- .fule_decomp(0.2, 0.5)

  expected <- (log(0.5) - log(1))/log(1.2)

  expect_equal(out, expected)

})




test_that(".predict_live_tree_age() predicts missing ages for live trees", {

  # Dummy Dataset
  df <- data.frame(
    Species = c("A", "A", "A", "B", "B", "B"),
    Age     = c(10, 11, NA, 20, 30, NA),
    DBH     = c(5, 6, 10, 10, 12, 13),
    Status  = c(1, 1, 1, 1, 1, 1)  # all live
  )

  meas_year <- 2025

  # Run function on dummy data
  out <- .predict_live_tree_age(df, species_col = "Species", age_col = "Age", dbh_col = "DBH", status_col = "Status", meas_year = meas_year)

  # Expect predicted ages
  expect_equal(out$Age, c(10, 11, 15, 20, 30, 35))


})


test_that(".predict_live_tree_age() works only for live trees", {

  # Dummy Dataset
  df <- data.frame(
    Species = c("A", "A", "A", "B", "B", "B", "A", "B"),
    Age     = c(10, 11, NA, 20, 30, NA, NA, NA),
    DBH     = c(5, 6, 10, 10, 12, 13, 10, 13),
    Status  = c(1, 1, 1, 1, 1, 1, 3, 4)  # final 2 trees are not status 1
  )

  meas_year <- 2025

  # Run function on dummy data
  out <- .predict_live_tree_age(df, species_col = "Species", age_col = "Age", dbh_col = "DBH", status_col = "Status", meas_year = meas_year)

  # Expect predicted ages
  expect_equal(out$Age, c(10, 11, 15, 20, 30, 35, NA, NA))


})
