#' Package Initialization and Utilities
#'
#' @description
#' Utility functions for package initialization, validation, and helper operations.

#' Set RYM Credentials
#'
#' @param client_id Character. OAuth2 client ID
#' @param client_secret Character. OAuth2 client secret
#' @param user Character. Default user login
#' @param token_path Character. Default token storage path
#'
#' @export
rym_set_credentials <- function(client_id = NULL,
                               client_secret = NULL,
                               user = NULL,
                               token_path = NULL) {
  
  if (!is.null(client_id)) {
    options(rym.client_id = client_id)
    cli::cli_alert_success("Client ID set")
  }
  
  if (!is.null(client_secret)) {
    options(rym.client_secret = client_secret)
    cli::cli_alert_success("Client secret set")
  }
  
  if (!is.null(user)) {
    options(rym.user = user)
    cli::cli_alert_success("Default user set to: {user}")
  }
  
  if (!is.null(token_path)) {
    if (!dir.exists(token_path)) {
      dir.create(token_path, recursive = TRUE)
    }
    options(rym.token_path = token_path)
    cli::cli_alert_success("Token path set to: {token_path}")
  }
  
  invisible(TRUE)
}

#' Get RYM Credentials
#'
#' @param mask_secrets Logical. Whether to mask sensitive information
#'
#' @return List with current credential settings
#' @export
rym_get_credentials <- function(mask_secrets = TRUE) {
  
  client_id <- getOption("rym.client_id")
  client_secret <- getOption("rym.client_secret")
  
  if (mask_secrets) {
    if (!is.null(client_id)) {
      client_id <- paste0(substr(client_id, 1, 8), "***")
    }
    if (!is.null(client_secret)) {
      client_secret <- paste0(substr(client_secret, 1, 8), "***")
    }
  }
  
  credentials <- list(
    client_id = client_id,
    client_secret = client_secret,
    user = getOption("rym.user"),
    token_path = getOption("rym.token_path"),
    show_deprecation_warnings = getOption("rym.show_deprecation_warnings", TRUE)
  )
  
  # Print formatted output
  cli::cli_h2("RYM Package Configuration")
  cli::cli_text("Client ID: {.val {credentials$client_id %||% 'Not set'}}")
  cli::cli_text("Client Secret: {.val {credentials$client_secret %||% 'Not set'}}")
  cli::cli_text("Default User: {.val {credentials$user %||% 'Not set'}}")
  cli::cli_text("Token Path: {.val {credentials$token_path %||% 'Not set'}}")
  cli::cli_text("Show Deprecation Warnings: {.val {credentials$show_deprecation_warnings}}")
  
  return(invisible(credentials))
}

#' Validate Date Input
#'
#' @param date Character or Date. Date to validate
#' @param param_name Character. Parameter name for error messages
#'
#' @return Character. Validated date string
#' @export
rym_validate_date <- function(date, param_name = "date") {
  
  if (is.null(date)) {
    return(NULL)
  }
  
  # Handle special date strings
  special_dates <- c(
    "today", "yesterday", "tomorrow",
    paste0(1:365, "daysAgo"),
    paste0(1:52, "weeksAgo"),
    paste0(1:12, "monthsAgo"),
    paste0(1:5, "yearsAgo")
  )
  
  if (is.character(date) && date %in% special_dates) {
    return(date)
  }
  
  # Try to parse as date
  tryCatch({
    if (is.character(date)) {
      parsed_date <- lubridate::ymd(date)
    } else if (inherits(date, "Date")) {
      parsed_date <- date
    } else {
      rlang::abort(
        paste("Invalid", param_name, "format. Use YYYY-MM-DD, Date object, or special strings like 'yesterday'"),
        class = "rym_validation_error"
      )
    }
    
    if (is.na(parsed_date)) {
      rlang::abort(
        paste("Could not parse", param_name, ":", date),
        class = "rym_validation_error"
      )
    }
    
    return(as.character(parsed_date))
    
  }, error = function(e) {
    rlang::abort(
      paste("Invalid", param_name, "format:", e$message),
      parent = e,
      class = "rym_validation_error"
    )
  })
}

#' Validate Counter ID
#'
#' @param counter_id Numeric or Character. Counter ID to validate
#'
#' @return Numeric. Validated counter ID
#' @export
rym_validate_counter <- function(counter_id) {
  
  if (is.null(counter_id)) {
    rlang::abort("Counter ID cannot be NULL", class = "rym_validation_error")
  }
  
  # Convert to numeric if possible
  if (is.character(counter_id)) {
    # Remove any non-numeric characters except digits
    clean_id <- stringr::str_extract(counter_id, "\\d+")
    
    if (is.na(clean_id)) {
      rlang::abort(
        "Counter ID must contain numeric value",
        class = "rym_validation_error"
      )
    }
    
    counter_id <- as.numeric(clean_id)
  }
  
  if (!is.numeric(counter_id) || is.na(counter_id) || counter_id <= 0) {
    rlang::abort(
      "Counter ID must be a positive number",
      class = "rym_validation_error"
    )
  }
  
  return(as.integer(counter_id))
}

#' Setup Parallel Processing
#'
#' @param strategy Character. Future strategy ("multisession", "sequential")
#' @param workers Numeric. Number of workers for parallel processing
#'
#' @export
rym_parallel_setup <- function(strategy = "multisession", workers = 2) {
  
  if (!requireNamespace("future", quietly = TRUE)) {
    rlang::abort("Package 'future' is required for parallel processing")
  }
  
  if (strategy == "multisession") {
    future::plan(future::multisession, workers = workers)
    cli::cli_alert_success("Parallel processing enabled with {workers} workers")
  } else if (strategy == "sequential") {
    future::plan(future::sequential)
    cli::cli_alert_info("Sequential processing enabled")
  } else {
    rlang::abort("Unknown strategy. Use 'multisession' or 'sequential'")
  }
  
  invisible(TRUE)
}

#' Setup Progress Reporting
#'
#' @param enable Logical. Enable progress reporting
#' @param type Character. Progress type ("cli", "none")
#'
#' @export
rym_progress_setup <- function(enable = TRUE, type = "cli") {
  
  if (enable) {
    if (type == "cli") {
      # CLI progress is handled automatically by cli package
      options(rym.progress = TRUE)
      cli::cli_alert_success("Progress reporting enabled")
    } else {
      rlang::abort("Unknown progress type. Use 'cli'")
    }
  } else {
    options(rym.progress = FALSE)
    cli::cli_alert_info("Progress reporting disabled")
  }
  
  invisible(TRUE)
}

#' Check Package Dependencies
#'
#' @param verbose Logical. Show detailed information
#'
#' @return Logical. TRUE if all dependencies are available
#' @export
rym_check_dependencies <- function(verbose = TRUE) {
  
  required_packages <- c(
    "httr2", "dplyr", "tidyr", "purrr", "stringr", "jsonlite",
    "lubridate", "readr", "cli", "rlang", "R6"
  )
  
  optional_packages <- c(
    "future", "future.apply", "progressr", "keyring", "base64enc"
  )
  
  missing_required <- character(0)
  missing_optional <- character(0)
  
  # Check required packages
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_required <- c(missing_required, pkg)
    }
  }
  
  # Check optional packages
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_optional <- c(missing_optional, pkg)
    }
  }
  
  if (verbose) {
    cli::cli_h2("Package Dependencies Check")
    
    if (length(missing_required) == 0) {
      cli::cli_alert_success("All required packages available")
    } else {
      cli::cli_alert_danger("Missing required packages: {paste(missing_required, collapse = ', ')}")
    }
    
    if (length(missing_optional) == 0) {
      cli::cli_alert_success("All optional packages available")
    } else {
      cli::cli_alert_warning("Missing optional packages: {paste(missing_optional, collapse = ', ')}")
    }
  }
  
  return(length(missing_required) == 0)
}

#' Format API Response for Display
#'
#' @param response List. API response object
#' @param max_rows Numeric. Maximum rows to display
#'
#' @return Formatted output
#' @keywords internal
format_api_response <- function(response, max_rows = 100) {
  
  if (is.data.frame(response)) {
    if (nrow(response) > max_rows) {
      cli::cli_alert_info("Showing first {max_rows} of {nrow(response)} rows")
      return(head(response, max_rows))
    } else {
      return(response)
    }
  } else if (is.list(response)) {
    # Format list response
    return(response)
  } else {
    return(response)
  }
}

#' Create Summary Statistics for Data
#'
#' @param data Data frame. Data to summarize
#' @param metrics Character. Metric columns to summarize
#'
#' @return List with summary statistics
#' @export
rym_summarize_data <- function(data, metrics = NULL) {
  
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(list(message = "No data to summarize"))
  }
  
  # Auto-detect metric columns if not specified
  if (is.null(metrics)) {
    numeric_cols <- sapply(data, is.numeric)
    metrics <- names(data)[numeric_cols]
  }
  
  summary_stats <- list(
    total_rows = nrow(data),
    total_cols = ncol(data),
    date_range = NULL,
    metric_summaries = list()
  )
  
  # Check for date columns
  date_cols <- sapply(data, function(x) inherits(x, c("Date", "POSIXct")) || 
                      (is.character(x) && any(stringr::str_detect(x, "\\d{4}-\\d{2}-\\d{2}"))))
  
  if (any(date_cols)) {
    date_col <- names(data)[date_cols][1]
    dates <- data[[date_col]]
    
    if (is.character(dates)) {
      dates <- lubridate::ymd(dates, quiet = TRUE)
    }
    
    if (any(!is.na(dates))) {
      summary_stats$date_range <- list(
        start = min(dates, na.rm = TRUE),
        end = max(dates, na.rm = TRUE)
      )
    }
  }
  
  # Calculate metric summaries
  for (metric in metrics) {
    if (metric %in% names(data) && is.numeric(data[[metric]])) {
      summary_stats$metric_summaries[[metric]] <- list(
        total = sum(data[[metric]], na.rm = TRUE),
        mean = mean(data[[metric]], na.rm = TRUE),
        median = median(data[[metric]], na.rm = TRUE),
        min = min(data[[metric]], na.rm = TRUE),
        max = max(data[[metric]], na.rm = TRUE)
      )
    }
  }
  
  return(summary_stats)
}

#' Print Method for RymClient
#'
#' @param x RymClient object
#' @param ... Additional arguments
#'
#' @export
print.RymClient <- function(x, ...) {
  x$print()
}

#' Summary Method for RymClient
#'
#' @param object RymClient object
#' @param ... Additional arguments
#'
#' @export
summary.RymClient <- function(object, ...) {
  cli::cli_h2("RYM Client Summary")
  cli::cli_text("User: {.strong {object$token$username}}")
  cli::cli_text("Base URL: {.url {object$base_url}}")
  cli::cli_text("Status: {.success Connected}")
  
  # Check API status
  status_ok <- object$check_status()
  if (status_ok) {
    cli::cli_text("API Status: {.success Available}")
  } else {
    cli::cli_text("API Status: {.error Unavailable}")
  }
  
  # Rate limit info
  rate_limit <- object$get_rate_limit_status()
  if (!is.null(rate_limit$remaining)) {
    cli::cli_text("Rate Limit: {.strong {rate_limit$remaining}} requests remaining")
  }
  
  invisible(object)
}
