#' Modern Yandex Metrica API Client
#'
#' @description
#' Modern R6-based client for Yandex Metrica API with comprehensive error handling,
#' automatic retries, and robust rate limiting.
#'
#' @export
RymClient <- R6::R6Class(
  "RymClient",
  
  public = list(
    #' @field token RymToken object for authentication
    token = NULL,
    
    #' @field base_url Base URL for API requests
    base_url = "https://api-metrika.yandex.ru",
    
    #' @field rate_limit_remaining Number of requests remaining
    rate_limit_remaining = NULL,
    
    #' @field rate_limit_reset Time when rate limit resets
    rate_limit_reset = NULL,
    
    #' Initialize RymClient
    #'
    #' @param token RymToken object or NULL (will prompt for auth)
    #' @param login Character. User login for authentication
    #' @param auto_refresh Logical. Automatically refresh tokens
    #'
    #' @return RymClient object
    initialize = function(token = NULL, login = NULL, auto_refresh = TRUE) {
      if (is.null(token)) {
        if (is.null(login)) {
          login <- getOption("rym.user")
        }
        self$token <- rym_auth_modern(login = login)
      } else {
        self$token <- token
      }
      
      private$auto_refresh <- auto_refresh
      cli::cli_alert_success("RymClient initialized for user: {self$token$username}")
    },
    
    #' Make HTTP Request
    #'
    #' @param method Character. HTTP method (GET, POST, DELETE)
    #' @param endpoint Character. API endpoint
    #' @param query List. Query parameters
    #' @param body List. Request body
    #' @param timeout Numeric. Request timeout in seconds
    #' @param max_retries Numeric. Maximum number of retries
    #'
    #' @return Response object
    request = function(method = "GET", endpoint, query = list(), body = NULL, 
                      timeout = 60, max_retries = 3) {
      
      # Ensure token is valid
      private$ensure_valid_token()
      
      # Build request
      req <- httr2::request(self$base_url) |>
        httr2::req_url_path_append(endpoint) |>
        httr2::req_auth_bearer_token(self$token$access_token) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = max_retries)
      
      # Add query parameters
      if (length(query) > 0) {
        req <- httr2::req_url_query(req, !!!query)
      }
      
      # Add body for POST/PUT requests
      if (!is.null(body)) {
        if (method %in% c("POST", "PUT", "PATCH")) {
          req <- httr2::req_body_form(req, !!!body)
        }
      }
      
      # Set method
      req$method <- method
      
      # Perform request with error handling
      tryCatch({
        resp <- httr2::req_perform(req)
        
        # Update rate limit info
        private$update_rate_limit(resp)
        
        # Check for API errors
        private$check_api_errors(resp)
        
        return(resp)
        
      }, error = function(e) {
        private$handle_request_error(e, endpoint)
      })
    },
    
    #' Get JSON Response
    #'
    #' @param ... Arguments passed to request()
    #'
    #' @return Parsed JSON response
    get_json = function(...) {
      resp <- self$request(...)
      return(httr2::resp_body_json(resp))
    },
    
    #' Get CSV Response
    #'
    #' @param ... Arguments passed to request()
    #'
    #' @return Data frame from CSV response
    get_csv = function(...) {
      resp <- self$request(...)
      csv_text <- httr2::resp_body_string(resp)
      return(readr::read_csv(csv_text, show_col_types = FALSE))
    },
    
    #' Check API Status
    #'
    #' @return Logical indicating if API is accessible
    check_status = function() {
      tryCatch({
        resp <- self$request("GET", "management/v1/counters", query = list(per_page = 1))
        return(httr2::resp_status(resp) == 200)
      }, error = function(e) {
        return(FALSE)
      })
    },
    
    #' Get Rate Limit Status
    #'
    #' @return List with rate limit information
    get_rate_limit_status = function() {
      list(
        remaining = self$rate_limit_remaining,
        reset_at = self$rate_limit_reset
      )
    },
    
    #' Print Method
    print = function() {
      cli::cli_text("Yandex Metrica API Client")
      cli::cli_text("User: {.strong {self$token$username}}")
      cli::cli_text("Base URL: {.url {self$base_url}}")
      
      if (!is.null(self$rate_limit_remaining)) {
        cli::cli_text("Rate Limit: {.strong {self$rate_limit_remaining}} requests remaining")
      }
      
      invisible(self)
    }
  ),
  
  private = list(
    auto_refresh = TRUE,
    
    #' Ensure Token is Valid
    ensure_valid_token = function() {
      if (private$auto_refresh && token_needs_refresh(self$token)) {
        cli::cli_alert_info("Token expired, refreshing...")
        
        new_token <- refresh_token(
          self$token,
          getOption("rym.client_id", "5a87e45d5562421bb29bb9abd17321b3"),
          getOption("rym.client_secret", "04e7f096ce21483fb1c9861f68c017d7")
        )
        
        if (!is.null(new_token)) {
          self$token <- new_token
          cli::cli_alert_success("Token refreshed successfully")
        } else {
          rlang::abort("Token refresh failed. Please re-authenticate.")
        }
      }
    },
    
    #' Update Rate Limit Information
    update_rate_limit = function(resp) {
      headers <- resp$headers
      
      if ("x-ratelimit-remaining" %in% names(headers)) {
        self$rate_limit_remaining <- as.numeric(headers[["x-ratelimit-remaining"]])
      }
      
      if ("x-ratelimit-reset" %in% names(headers)) {
        self$rate_limit_reset <- as.POSIXct(as.numeric(headers[["x-ratelimit-reset"]]), origin = "1970-01-01")
      }
    },
    
    #' Check for API Errors
    check_api_errors = function(resp) {
      status <- httr2::resp_status(resp)
      
      if (status >= 400) {
        body <- tryCatch({
          httr2::resp_body_json(resp)
        }, error = function(e) {
          list(message = httr2::resp_body_string(resp))
        })
        
        error_msg <- if (!is.null(body$message)) {
          body$message
        } else if (!is.null(body$error_description)) {
          body$error_description
        } else {
          paste("HTTP", status, "error")
        }
        
        rlang::abort(
          paste("API Error:", error_msg),
          class = "rym_api_error",
          status = status,
          body = body
        )
      }
    },
    
    #' Handle Request Errors
    handle_request_error = function(error, endpoint) {
      cli::cli_alert_danger("Request failed for endpoint: {endpoint}")
      cli::cli_alert_danger("Error: {error$message}")
      
      # Re-throw with additional context
      rlang::abort(
        paste("Request to", endpoint, "failed:", error$message),
        parent = error,
        class = "rym_request_error"
      )
    }
  )
)

#' Management API Client
#'
#' @description
#' Specialized client for Yandex Metrica Management API operations.
#'
#' @export
RymManagementAPI <- R6::R6Class(
  "RymManagementAPI",
  inherit = RymClient,
  
  public = list(
    #' Get Counters
    #'
    #' @param search_string Character. Search filter
    #' @param per_page Numeric. Items per page
    #' @param offset Numeric. Offset for pagination
    #'
    #' @return Data frame with counter information
    get_counters = function(search_string = NULL, per_page = 1000, offset = 1) {
      query <- list(
        per_page = per_page,
        offset = offset
      )
      
      if (!is.null(search_string)) {
        query$search_string <- search_string
      }
      
      response <- self$get_json("GET", "management/v1/counters", query = query)
      
      if (length(response$counters) == 0) {
        cli::cli_alert_warning("No counters found")
        return(data.frame())
      }
      
      # Convert to data frame
      counters_df <- purrr::map_dfr(response$counters, function(counter) {
        data.frame(
          id = counter$id %||% NA,
          status = counter$status %||% NA,
          owner_login = counter$owner_login %||% NA,
          name = counter$name %||% NA,
          code_status = counter$code_status %||% NA,
          site = counter$site %||% NA,
          permission = counter$permission %||% NA,
          type = counter$type %||% NA,
          gdpr_agreement_accepted = counter$gdpr_agreement_accepted %||% NA,
          stringsAsFactors = FALSE
        )
      })
      
      return(counters_df)
    },
    
    #' Get Goals
    #'
    #' @param counter_id Numeric. Counter ID
    #'
    #' @return Data frame with goals information
    get_goals = function(counter_id) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/goals")
      response <- self$get_json("GET", endpoint)
      
      if (length(response$goals) == 0) {
        cli::cli_alert_warning("No goals found for counter {counter_id}")
        return(data.frame())
      }
      
      # Convert to data frame
      goals_df <- purrr::map_dfr(response$goals, function(goal) {
        data.frame(
          id = goal$id %||% NA,
          name = goal$name %||% NA,
          type = goal$type %||% NA,
          is_retargeting = goal$is_retargeting %||% NA,
          stringsAsFactors = FALSE
        )
      })
      
      return(goals_df)
    },
    
    #' Get Segments
    #'
    #' @param counter_id Numeric. Counter ID
    #'
    #' @return Data frame with segments information
    get_segments = function(counter_id) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/apisegments")
      response <- self$get_json("GET", endpoint)
      
      if (length(response$segments) == 0) {
        cli::cli_alert_warning("No segments found for counter {counter_id}")
        return(data.frame())
      }
      
      # Convert to data frame
      segments_df <- purrr::map_dfr(response$segments, function(segment) {
        data.frame(
          segment_id = segment$segment_id %||% NA,
          name = segment$name %||% NA,
          expression = segment$expression %||% NA,
          stringsAsFactors = FALSE
        )
      })
      
      return(segments_df)
    },
    
    #' Add Goal
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param name Character. Goal name
    #' @param type Character. Goal type
    #' @param conditions List. Goal conditions
    #'
    #' @return Goal creation response
    add_goal = function(counter_id, name, type, conditions = list()) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/goals")
      
      body <- list(
        goal = list(
          name = name,
          type = type,
          conditions = conditions
        )
      )
      
      response <- self$get_json("POST", endpoint, body = body)
      cli::cli_alert_success("Goal '{name}' created successfully")
      
      return(response)
    }
  )
)

#' Reporting API Client
#'
#' @description
#' Specialized client for Yandex Metrica Reporting API operations.
#'
#' @export
RymReportingAPI <- R6::R6Class(
  "RymReportingAPI",
  inherit = RymClient,
  
  public = list(
    #' Get Data
    #'
    #' @param counters Character/Numeric. Counter IDs
    #' @param metrics Character. Metrics to retrieve
    #' @param dimensions Character. Dimensions to group by
    #' @param date_from Character/Date. Start date
    #' @param date_to Character/Date. End date
    #' @param filters Character. Filters to apply
    #' @param sort Character. Sort order
    #' @param limit Numeric. Maximum rows to retrieve
    #' @param accuracy Character. Data accuracy level
    #' @param include_undefined Logical. Include undefined values
    #' @param lang Character. Response language
    #'
    #' @return Data frame with reporting data
    get_data = function(counters,
                       metrics = "ym:s:visits,ym:s:pageviews,ym:s:users",
                       dimensions = NULL,
                       date_from = "8daysAgo",
                       date_to = "yesterday",
                       filters = NULL,
                       sort = NULL,
                       limit = 100000,
                       accuracy = "full",
                       include_undefined = TRUE,
                       lang = "en") {
      
      # Validate and prepare parameters
      counters <- private$prepare_counters(counters)
      metrics <- private$prepare_metrics(metrics)
      dimensions <- private$prepare_dimensions(dimensions)
      
      # Build query
      query <- list(
        ids = counters,
        metrics = metrics,
        date1 = date_from,
        date2 = date_to,
        accuracy = accuracy,
        include_undefined = include_undefined,
        lang = lang,
        limit = format(limit, scientific = FALSE)
      )
      
      # Add optional parameters
      if (!is.null(dimensions)) query$dimensions <- dimensions
      if (!is.null(filters)) query$filters <- filters
      if (!is.null(sort)) query$sort <- sort
      
      # Get data using CSV endpoint for better performance
      data <- self$get_csv("GET", "stat/v1/data.csv", query = query)
      
      # Remove totals row if present
      if (nrow(data) > 1 && !is.null(dimensions)) {
        data <- data[-1, ]
      }
      
      return(data)
    },
    
    #' Get Data with Pagination
    #'
    #' @param ... Arguments passed to get_data()
    #' @param progress Logical. Show progress bar
    #'
    #' @return Complete data frame with all paginated results
    get_data_paginated = function(..., progress = TRUE) {
      args <- list(...)
      limit <- args$limit %||% 100000
      
      # First request to get total count
      args$limit <- 1
      first_response <- do.call(self$request, c(list(method = "GET", endpoint = "stat/v1/data"), args))
      total_rows <- httr2::resp_body_json(first_response)$total_rows
      
      if (total_rows == 0) {
        cli::cli_alert_warning("No data found for the specified parameters")
        return(data.frame())
      }
      
      # Calculate pagination
      pages <- ceiling(total_rows / limit)
      
      if (progress) {
        cli::cli_alert_info("Retrieving {total_rows} rows in {pages} page(s)")
        pb <- cli::cli_progress_bar(total = pages, format = "Downloading {cli::pb_bar} {cli::pb_percent}")
      }
      
      # Collect all pages
      all_data <- list()
      
      for (page in seq_len(pages)) {
        args$limit <- limit
        args$offset <- (page - 1) * limit + 1
        
        page_data <- do.call(self$get_data, args)
        all_data[[page]] <- page_data
        
        if (progress) {
          cli::cli_progress_update(pb)
        }
      }
      
      if (progress) {
        cli::cli_progress_done(pb)
      }
      
      # Combine all pages
      result <- dplyr::bind_rows(all_data)
      cli::cli_alert_success("Retrieved {nrow(result)} rows successfully")
      
      return(result)
    }
  ),
  
  private = list(
    prepare_counters = function(counters) {
      if (is.null(counters)) {
        # Get all available counters
        mgmt <- RymManagementAPI$new(token = self$token)
        counters <- mgmt$get_counters()$id
      }
      
      paste(counters, collapse = ",")
    },
    
    prepare_metrics = function(metrics) {
      if (is.character(metrics) && length(metrics) > 1) {
        metrics <- paste(metrics, collapse = ",")
      }
      stringr::str_replace_all(metrics, "[\\s\\n\\t]", "")
    },
    
    prepare_dimensions = function(dimensions) {
      if (is.null(dimensions)) return(NULL)
      
      if (is.character(dimensions) && length(dimensions) > 1) {
        dimensions <- paste(dimensions, collapse = ",")
      }
      stringr::str_replace_all(dimensions, "[\\s\\n\\t]", "")
    }
  )
)

#' Null-default operator
#' @param x Object to test
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
