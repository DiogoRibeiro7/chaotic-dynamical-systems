
test_that('run_demo executes and logs info', {
  logf <- tempfile()
  res <- with_logging(run_demo(n = 100, block_size = 20, output_report = FALSE),
                      logf, msg = 'run_demo')
  expect_true(is.list(res))
  expect_true('extremal_index' %in% names(res))
  lines <- readLines(logf)
  expect_true(any(grepl('INFO: run_demo', lines)))
})

test_that('extremal index workflow runs end-to-end', {
  logf <- tempfile()
  result <- with_logging({
    set.seed(1)
    x <- arima.sim(model = list(ar = 0.7), n = 1000)
    thr <- quantile(x, 0.95)
    runs <- extremal_index_runs(x, thr, run_length = 5)
    inter <- extremal_index_intervals(x, thr)
    list(runs = runs, intervals = inter)
  }, logf, msg = 'ei_workflow')
  expect_true(is.list(result))
  expect_true(all(c('runs', 'intervals') %in% names(result)))
  lines <- readLines(logf)
  expect_true(any(grepl('INFO: ei_workflow', lines)))
})

test_that('simulation results feed into recurrence analysis', {
  set.seed(123)
  x <- simulate_logistic_map(200, r = 3.8, x0 = 0.2)
  props <- recurrence_analysis(x)
  expect_true(is.list(props))
  expect_true(props$recurrence_rate > 0)
  expect_gte(props$determinism, 0)
})
