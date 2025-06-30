context('POT and block maxima utilities')

test_that('exceedances extracts values above threshold', {
  x <- c(-1, 0.5, 1.2, 2, 0.3)
  exc <- exceedances(x, 1)
  expect_equal(exc, c(1.2, 2))
})

test_that('block_maxima splits series correctly', {
  x <- 1:10
  bm <- block_maxima(x, 2)
  expect_equal(bm, c(2,4,6,8,10))
})
