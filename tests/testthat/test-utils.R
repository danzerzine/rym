test_that("Date validation works", {
  # Valid dates
  expect_equal(rym_validate_date("2024-01-01"), "2024-01-01")
  expect_equal(rym_validate_date("yesterday"), "yesterday")
  expect_equal(rym_validate_date("7daysAgo"), "7daysAgo")
  
  # Invalid dates
  expect_error(rym_validate_date("invalid-date"))
  expect_error(rym_validate_date(123))
  
  # NULL handling
  expect_null(rym_validate_date(NULL))
})

test_that("Counter validation works", {
  # Valid counters
  expect_equal(rym_validate_counter(12345), 12345L)
  expect_equal(rym_validate_counter("12345"), 12345L)
  expect_equal(rym_validate_counter("counter_12345"), 12345L)
  
  # Invalid counters
  expect_error(rym_validate_counter(NULL))
  expect_error(rym_validate_counter("no_numbers"))
  expect_error(rym_validate_counter(-123))
  expect_error(rym_validate_counter(0))
})

test_that("Credential management works", {
  # Test setting credentials
  expect_true(rym_set_credentials(user = "test_user"))
  expect_equal(getOption("rym.user"), "test_user")
  
  # Test getting credentials
  creds <- rym_get_credentials()
  expect_type(creds, "list")
  expect_true("user" %in% names(creds))
})

test_that("Dependency check works", {
  # Should return logical
  result <- rym_check_dependencies(verbose = FALSE)
  expect_type(result, "logical")
})

test_that("Data summarization works", {
  # Test with sample data
  test_data <- data.frame(
    date = c("2024-01-01", "2024-01-02", "2024-01-03"),
    visits = c(100, 150, 200),
    users = c(80, 120, 160),
    stringsAsFactors = FALSE
  )
  
  summary <- rym_summarize_data(test_data)
  expect_type(summary, "list")
  expect_equal(summary$total_rows, 3)
  expect_equal(summary$total_cols, 3)
  expect_true("visits" %in% names(summary$metric_summaries))
  
  # Test with empty data
  empty_summary <- rym_summarize_data(data.frame())
  expect_equal(empty_summary$message, "No data to summarize")
})
