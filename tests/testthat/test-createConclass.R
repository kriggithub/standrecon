test_that("createConclass() returns a numeric vector of same length", {
  status <- c(2, 2, 2, 3, 4, 6, 5)
  decay  <- c(1, 3, 5, NA, 7, 2, 4)

  out <- createConclass(status, decay)

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
