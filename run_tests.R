

library(testthat)

testthat::test_dir('./Tests/',reporter = ProgressReporter$new(show_praise = FALSE))
