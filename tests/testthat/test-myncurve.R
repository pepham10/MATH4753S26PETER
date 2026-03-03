test_that("myncurve returns correct mu", {
  output <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(output$mu, 10)
})

test_that("myncurve returns correct sigma", {
  output <- myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(output$sigma, 5)
})

test_that("myncurve returns correct probability", {
  output <- myncurve(mu = 10, sigma = 5, a = 6)
  expected_prob <- pnorm(6, mean = 10, sd = 5)
  expect_equal(output$prob, expected_prob)
})
