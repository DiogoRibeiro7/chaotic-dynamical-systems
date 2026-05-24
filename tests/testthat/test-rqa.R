test_that("rqa returns the documented named list", {
  set.seed(1L)
  x <- simulate_logistic_map(500, r = 3.8, x0 = 0.2)
  r <- rqa(x, embed = 3, delay = 1)

  expect_type(r, "list")
  expect_named(r, c("RR", "DET", "LAM", "L", "L_max", "TT", "V_max", "ENT"))
  expect_true(is.numeric(r$RR))
  expect_true(is.numeric(r$DET))
  expect_true(is.numeric(r$LAM))
})

test_that("RR and DET are in [0, 1]", {
  set.seed(2L)
  x <- simulate_henon_map(500, x0 = 0.1, y0 = 0.1)$x
  r <- rqa(x, embed = 3, delay = 1)
  expect_gte(r$RR, 0); expect_lte(r$RR, 1)
  expect_gte(r$DET, 0); expect_lte(r$DET, 1)
  expect_gte(r$LAM, 0); expect_lte(r$LAM, 1)
})

test_that("Deterministic chaotic input has higher DET than IID noise", {
  set.seed(3L)
  chaotic <- simulate_logistic_map(1000, r = 3.9, x0 = 0.2)
  iid     <- runif(1000)
  rc <- rqa(chaotic, embed = 3, delay = 1, eps = 0.1 * stats::sd(chaotic))
  rn <- rqa(iid,     embed = 3, delay = 1, eps = 0.1 * stats::sd(iid))
  # Deterministic dynamics produce longer diagonal segments, hence higher DET.
  expect_gt(rc$DET, rn$DET)
})

test_that("Theiler window excludes the main diagonal", {
  set.seed(4L)
  x <- simulate_logistic_map(200, r = 3.8, x0 = 0.2)
  # Without a Theiler band the entire main diagonal counts as one long
  # diagonal, which dominates L_max. With theiler = 1 it doesn't.
  r0 <- rqa(x, embed = 3, theiler = 0L)
  r1 <- rqa(x, embed = 3, theiler = 1L)
  expect_gt(r0$L_max, r1$L_max)
})

test_that("rqa is deterministic and rejects invalid arguments", {
  x <- simulate_logistic_map(200, r = 3.8, x0 = 0.2)
  a <- rqa(x, embed = 3, delay = 1)
  b <- rqa(x, embed = 3, delay = 1)
  expect_identical(a, b)

  expect_error(rqa(1:3))
  expect_error(rqa(x, embed = 0))
  expect_error(rqa(x, delay = 0))
  expect_error(rqa(x, l_min = 1))   # l_min must be >= 2
  expect_error(rqa(x, v_min = 1))
  expect_error(rqa(x, theiler = -1))
})
