test_that("Token creation works", {
  # Test token creation
  token <- create_rym_token(
    access_token = "test_token",
    refresh_token = "test_refresh",
    expires_in = 3600,
    username = "test_user"
  )
  
  expect_s3_class(token, "RymToken")
  expect_equal(token$access_token, "test_token")
  expect_equal(token$username, "test_user")
  expect_true("expire_at" %in% names(token))
})

test_that("Token validation works", {
  # Valid token
  valid_token <- list(
    access_token = "test",
    username = "user"
  )
  expect_true(validate_token_structure(valid_token))
  
  # Invalid token
  invalid_token <- list(access_token = "test")
  expect_false(validate_token_structure(invalid_token))
})

test_that("Token needs refresh detection works", {
  # Fresh token
  fresh_token <- list(expire_at = Sys.time() + 86400)  # 1 day
  expect_false(token_needs_refresh(fresh_token))
  
  # Expired token
  expired_token <- list(expire_at = Sys.time() - 3600)  # 1 hour ago
  expect_true(token_needs_refresh(expired_token))
  
  # Token expiring soon
  expiring_token <- list(expire_at = Sys.time() + 3600)  # 1 hour
  expect_true(token_needs_refresh(expiring_token))
})

test_that("URL code extraction works", {
  # Valid URL with code
  url <- "https://example.com/callback?code=abcd1234efgh5678&state=xyz"
  code <- extract_auth_code(url)
  expect_equal(code, "abcd1234efgh5678")
  
  # URL without code
  url_no_code <- "https://example.com/callback?error=access_denied"
  expect_null(extract_auth_code(url_no_code))
})

test_that("Print method for RymToken works", {
  token <- list(
    username = "test_user",
    expire_at = Sys.time() + 86400
  )
  class(token) <- "RymToken"
  
  # Should not error
  expect_output(print(token), "Yandex Metrica Token")
})
