test_that("liveTreeAge() predicts missing ages for live trees", {

  # Dummy Dataset
  df <- data.frame(
    Species = c("A", "A", "A", "B", "B", "B"),
    Age     = c(10, 11, NA, 20, 30, NA),
    DBH     = c(5, 6, 10, 10, 12, 13),
    Status  = c(1, 1, 1, 1, 1, 1)  # all live
  )

  measYear <- 2025

  # Run function on dummy data
  out <- liveTreeAge(df, speciesCol = Species, ageCol = Age, dbhCol = DBH, statusCol = Status, measYear = measYear)

  # Expect predicted ages
  expect_equal(out$Age, c(10, 11, 15, 20, 30, 35))


})


test_that("liveTreeAge() works only for live trees", {

  # Dummy Dataset
  df <- data.frame(
    Species = c("A", "A", "A", "B", "B", "B", "A", "B"),
    Age     = c(10, 11, NA, 20, 30, NA, NA, NA),
    DBH     = c(5, 6, 10, 10, 12, 13, 10, 13),
    Status  = c(1, 1, 1, 1, 1, 1, 3, 4)  # final 2 trees are not status 1
  )

  measYear <- 2025

  # Run function on dummy data
  out <- liveTreeAge(df, speciesCol = Species, ageCol = Age, dbhCol = DBH, statusCol = Status, measYear = measYear)

  # Expect predicted ages
  expect_equal(out$Age, c(10, 11, 15, 20, 30, 35, NA, NA))


})
