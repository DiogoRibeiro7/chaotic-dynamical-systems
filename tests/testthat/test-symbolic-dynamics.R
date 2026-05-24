test_that("symbolize with one break splits at the break-point", {
  x <- c(0.1, 0.4, 0.5, 0.6, 0.9)
  s <- symbolize(x, breaks = 0.5)
  expect_type(s, "integer")
  expect_equal(s, c(0L, 0L, 1L, 1L, 1L))
})

test_that("symbolize with equiprobable partition gives balanced bin counts", {
  set.seed(1L)
  x <- runif(2000)
  s <- symbolize(x, n_symbols = 4L)
  expect_setequal(unique(s), 0L:3L)
  expect_true(max(abs(tabulate(s + 1L) / length(s) - 0.25)) < 0.05)
})

test_that("block_entropy of an IID Bernoulli-1/2 source approaches k * log(2)", {
  set.seed(2L)
  s <- sample(0L:1L, 4000L, replace = TRUE)
  expect_equal(block_entropy(s, 1L), log(2), tolerance = 0.05)
  expect_equal(block_entropy(s, 3L), 3 * log(2), tolerance = 0.10)
})

test_that("block_entropy is zero on a constant sequence", {
  expect_equal(block_entropy(rep(0L, 100), 1L), 0)
  expect_equal(block_entropy(rep(7L, 100), 2L), 0)
})

test_that("source_entropy of the logistic map at r = 4 approaches log(2)", {
  # Generating partition for the logistic map is at the critical point
  # x = 0.5. At r = 4 the symbol process is a Bernoulli-1/2 shift, so the
  # source entropy is log(2).
  x <- simulate_logistic_map(20000, r = 4, x0 = 0.2)
  s <- symbolize(x, breaks = 0.5)
  expect_equal(source_entropy(s), log(2), tolerance = 0.05)
})

test_that("source_entropy is zero on a constant sequence", {
  expect_equal(source_entropy(rep(0L, 200)), 0)
})

test_that("symbolic-dynamics functions reject invalid arguments", {
  expect_error(symbolize(c(0.1, NA)))
  expect_error(symbolize(c(0.1, 0.5), breaks = c(0.3, 0.1)))   # not sorted
  expect_error(symbolize(c(0.1, 0.5), n_symbols = 1))
  expect_error(block_entropy(numeric(0)))
  expect_error(block_entropy(c(0L, 1L), word_length = 0))
  expect_error(source_entropy(c(0L, 1L), max_word_length = 1))
  expect_error(source_entropy(c(0L, 1L), min_obs_per_word = 0.5))
})
