context('Integration tests')

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
