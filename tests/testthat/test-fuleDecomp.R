test_that("fuleDecomp() correctly calculates years dead", {

  out <- fuleDecomp(0.2, 0.5)

  expected <- (log(0.5) - log(1))/log(1.2)

  expect_equal(out, expected)

})
