#' Logs API Client
#'
#' @description
#' Modern client for Yandex Metrica Logs API with async processing and progress tracking.
#'
#' @export
RymLogsAPI <- R6::R6Class(
  "RymLogsAPI",
  inherit = RymClient,
  
  public = list(
    #' Get Raw Logs
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param date_from Character/Date. Start date
    #' @param date_to Character/Date. End date
    #' @param fields Character. Fields to retrieve
    #' @param source Character. Data source (visits, hits)
    #' @param progress Logical. Show progress indicators
    #' @param timeout Numeric. Maximum wait time in seconds
    #'
    #' @return Data frame with log data
    get_logs = function(counter_id,
                       date_from = Sys.Date() - 10,
                       date_to = Sys.Date() - 1,
                       fields = "ym:s:date,ym:s:counterID,ym:s:dateTime,ym:s:isNewUser,ym:s:startURL,ym:s:visitDuration",
                       source = "visits",
                       progress = TRUE,
                       timeout = 3600) {
      
      start_time <- Sys.time()
      
      # Clean fields parameter
      fields <- stringr::str_replace_all(fields, "[\\s\\n\\t]", "")
      
      # Submit log request
      request_info <- private$submit_log_request(counter_id, date_from, date_to, fields, source)
      
      if (is.null(request_info)) {
        rlang::abort("Failed to submit log request")
      }
      
      request_id <- request_info$request_id
      
      if (progress) {
        cli::cli_alert_info("Log request submitted (ID: {request_id})")
        cli::cli_alert_info("Processing request...")
      }
      
      # Wait for processing with timeout
      result_data <- private$wait_for_processing(counter_id, request_id, progress, timeout)
      
      # Clean up request on server
      private$cleanup_request(counter_id, request_id)
      
      if (progress) {
        total_time <- round(as.numeric(Sys.time() - start_time, units = "secs"), 1)
        cli::cli_alert_success("Log data retrieved successfully in {total_time}s")
        cli::cli_alert_info("Retrieved {nrow(result_data)} rows")
      }
      
      return(result_data)
    },
    
    #' Get Available Fields
    #'
    #' @param source Character. Data source (visits, hits)
    #'
    #' @return Character vector of available fields
    get_available_fields = function(source = "visits") {
      # This would typically come from API documentation or metadata endpoint
      # For now, return common fields
      if (source == "visits") {
        return(c(
          "ym:s:date", "ym:s:dateTime", "ym:s:counterID", "ym:s:clientID",
          "ym:s:visitID", "ym:s:watchID", "ym:s:isNewUser", "ym:s:startURL",
          "ym:s:endURL", "ym:s:pageViews", "ym:s:visitDuration", "ym:s:bounce",
          "ym:s:ipAddress", "ym:s:userAgent", "ym:s:referer", "ym:s:lastTrafficSource"
        ))
      } else if (source == "hits") {
        return(c(
          "ym:pv:date", "ym:pv:dateTime", "ym:pv:counterID", "ym:pv:clientID",
          "ym:pv:visitID", "ym:pv:watchID", "ym:pv:URL", "ym:pv:title",
          "ym:pv:referer", "ym:pv:UTMCampaign", "ym:pv:UTMSource", "ym:pv:UTMMedium"
        ))
      } else {
        rlang::abort("Unknown source type. Use 'visits' or 'hits'")
      }
    }
  ),
  
  private = list(
    #' Submit Log Request
    submit_log_request = function(counter_id, date_from, date_to, fields, source) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/logrequests")
      
      query <- list(
        date1 = as.character(date_from),
        date2 = as.character(date_to),
        fields = fields,
        source = source
      )
      
      tryCatch({
        response <- self$get_json("POST", endpoint, query = query)
        
        if (!is.null(response$errors)) {
          rlang::abort(paste("Log request failed:", response$errors[[1]]$message))
        }
        
        return(response$log_request)
      }, error = function(e) {
        cli::cli_alert_danger("Failed to submit log request: {e$message}")
        return(NULL)
      })
    },
    
    #' Wait for Log Processing
    wait_for_processing = function(counter_id, request_id, progress, timeout) {
      start_time <- Sys.time()
      check_interval <- 5  # seconds
      
      if (progress) {
        pb <- cli::cli_progress_bar(
          format = "Processing {cli::pb_spin} {cli::pb_elapsed}",
          total = NA
        )
      }
      
      repeat {
        # Check if timeout exceeded
        if (as.numeric(Sys.time() - start_time, units = "secs") > timeout) {
          if (progress) cli::cli_progress_done(pb)
          rlang::abort("Log processing timeout exceeded")
        }
        
        # Check request status
        endpoint <- paste0("management/v1/counter/", counter_id, "/logrequest/", request_id)
        status_response <- self$get_json("GET", endpoint)
        status <- status_response$log_request$status
        
        if (progress) {
          cli::cli_progress_update(pb)
        }
        
        if (status == "processed") {
          if (progress) {
            cli::cli_progress_done(pb)
            cli::cli_alert_success("Log processing completed")
          }
          
          # Download all parts
          return(private$download_log_parts(counter_id, request_id, status_response$log_request$parts))
          
        } else if (status == "processing_failed") {
          if (progress) cli::cli_progress_done(pb)
          rlang::abort("Log processing failed on server")
          
        } else if (status %in% c("canceled", "cleaned_by_user", "cleaned_automatically_as_too_old")) {
          if (progress) cli::cli_progress_done(pb)
          rlang::abort(paste("Log request", status))
        }
        
        # Wait before next check
        Sys.sleep(check_interval)
      }
    },
    
    #' Download Log Parts
    download_log_parts = function(counter_id, request_id, parts) {
      all_data <- list()
      
      if (length(parts) > 1) {
        cli::cli_alert_info("Downloading {length(parts)} data parts...")
        pb <- cli::cli_progress_bar(total = length(parts))
      }
      
      for (i in seq_along(parts)) {
        part_num <- i - 1  # API uses 0-based indexing
        
        endpoint <- paste0(
          "management/v1/counter/", counter_id,
          "/logrequest/", request_id,
          "/part/", part_num, "/download"
        )
        
        tryCatch({
          response <- self$request("GET", endpoint)
          csv_text <- httr2::resp_body_string(response)
          
          # Parse CSV data
          part_data <- readr::read_delim(
            csv_text,
            delim = "\t",
            col_types = readr::cols(.default = "c"),
            show_col_types = FALSE
          )
          
          all_data[[i]] <- part_data
          
          if (length(parts) > 1) {
            cli::cli_progress_update(pb)
          }
          
        }, error = function(e) {
          cli::cli_alert_warning("Failed to download part {i}: {e$message}")
        })
      }
      
      if (length(parts) > 1) {
        cli::cli_progress_done(pb)
      }
      
      # Combine all parts
      if (length(all_data) > 0) {
        return(dplyr::bind_rows(all_data))
      } else {
        return(data.frame())
      }
    },
    
    #' Clean Up Request on Server
    cleanup_request = function(counter_id, request_id) {
      endpoint <- paste0(
        "management/v1/counter/", counter_id,
        "/logrequest/", request_id, "/clean"
      )
      
      tryCatch({
        self$request("POST", endpoint)
        cli::cli_alert_info("Server cleanup completed")
      }, error = function(e) {
        cli::cli_alert_warning("Server cleanup failed: {e$message}")
      })
    }
  )
)

#' Google Analytics Compatible API Client
#'
#' @description
#' Client that provides Google Analytics Core Reporting API v3 compatible interface.
#'
#' @export
RymGAAPI <- R6::R6Class(
  "RymGAAPI",
  inherit = RymClient,
  
  public = list(
    #' Get GA-Compatible Data
    #'
    #' @param counter Character. Counter ID with 'ga:' prefix
    #' @param dimensions Character. GA-style dimensions
    #' @param metrics Character. GA-style metrics
    #' @param start_date Character. Start date (YYYY-MM-DD)
    #' @param end_date Character. End date (YYYY-MM-DD)
    #' @param filters Character. GA-style filters
    #' @param sort Character. Sort order
    #' @param start_index Numeric. Start index for pagination
    #' @param max_results Numeric. Maximum results
    #'
    #' @return Data frame with GA-compatible column names
    get_ga_data = function(counter,
                          dimensions = NULL,
                          metrics = "ga:sessions,ga:users",
                          start_date = "7daysAgo",
                          end_date = "yesterday",
                          filters = NULL,
                          sort = NULL,
                          start_index = 1,
                          max_results = 1000) {
      
      # Remove ga: prefix from counter
      counter_clean <- stringr::str_replace(counter, "^ga:", "")
      
      # Build query
      query <- list(
        ids = counter_clean,
        dimensions = dimensions,
        metrics = metrics,
        `start-date` = start_date,
        `end-date` = end_date,
        `start-index` = start_index,
        `max-results` = max_results
      )
      
      # Add optional parameters
      if (!is.null(filters)) query$filters <- filters
      if (!is.null(sort)) query$sort <- sort
      
      # Make request to GA-compatible endpoint
      endpoint <- "stat/v1/data/ga"
      response <- self$get_json("GET", endpoint, query = query)
      
      # Convert response to data frame with GA-style structure
      return(private$format_ga_response(response))
    },
    
    #' Get GA Metadata
    #'
    #' @return List with available dimensions and metrics
    get_ga_metadata = function() {
      # This would typically come from a metadata endpoint
      # For now, return static mapping
      list(
        dimensions = private$get_ga_dimension_mapping(),
        metrics = private$get_ga_metric_mapping()
      )
    }
  ),
  
  private = list(
    #' Format GA Response
    format_ga_response = function(response) {
      if (is.null(response$data) || length(response$data) == 0) {
        return(data.frame())
      }
      
      # Extract column headers
      headers <- purrr::map_chr(response$columnHeaders, ~ .x$name)
      
      # Convert rows to data frame
      if (length(response$rows) > 0) {
        data_matrix <- do.call(rbind, response$rows)
        colnames(data_matrix) <- headers
        
        # Convert to data frame and handle data types
        result <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
        
        # Convert metric columns to numeric
        metric_cols <- stringr::str_detect(headers, "^ga:")
        if (any(metric_cols)) {
          result[metric_cols] <- lapply(result[metric_cols], as.numeric)
        }
        
        return(result)
      } else {
        return(data.frame())
      }
    },
    
    #' GA Dimension Mapping
    get_ga_dimension_mapping = function() {
      list(
        "ga:date" = "ym:s:date",
        "ga:source" = "ym:s:lastTrafficSource",
        "ga:medium" = "ym:s:trafficSource",
        "ga:campaign" = "ym:s:UTMCampaign",
        "ga:country" = "ym:s:regionCountry",
        "ga:city" = "ym:s:regionCity",
        "ga:browser" = "ym:s:browser",
        "ga:operatingSystem" = "ym:s:operatingSystem",
        "ga:deviceCategory" = "ym:s:deviceCategory"
      )
    },
    
    #' GA Metric Mapping
    get_ga_metric_mapping = function() {
      list(
        "ga:sessions" = "ym:s:visits",
        "ga:users" = "ym:s:users",
        "ga:pageviews" = "ym:s:pageviews",
        "ga:bounces" = "ym:s:bounces",
        "ga:sessionDuration" = "ym:s:avgVisitDurationSeconds"
      )
    }
  )
)

#' Get Logs (Legacy Function)
#'
#' @param counter Numeric. Counter ID
#' @param date.from Character/Date. Start date
#' @param date.to Character/Date. End date
#' @param fields Character. Fields to retrieve
#' @param source Character. Data source
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with log data
#' @export
rym_get_logs <- function(counter = NULL,
                        date.from = Sys.Date() - 10,
                        date.to = Sys.Date() - 1,
                        fields = "ym:s:date,ym:s:counterID,ym:s:dateTime,ym:s:isNewUser,ym:s:startURL,ym:s:visitDuration",
                        source = "visits",
                        login = getOption("rym.user"),
                        token.path = getOption("rym.token_path")) {
  
  # Show deprecation info
  if (getOption("rym.show_deprecation_warnings", TRUE)) {
    rlang::inform(
      "Consider using RymLogsAPI$new()$get_logs() for modern API access",
      .frequency = "once",
      .frequency_id = "rym_get_logs_modern"
    )
  }
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = FALSE
  )
  
  # Create logs client
  logs <- RymLogsAPI$new(token = token)
  
  return(logs$get_logs(
    counter_id = counter,
    date_from = date.from,
    date_to = date.to,
    fields = fields,
    source = source
  ))
}

#' Get GA Data (Legacy Function)
#'
#' @param counter Character. Counter ID with ga: prefix
#' @param dimensions Character. Dimensions
#' @param metrics Character. Metrics
#' @param start.date Character. Start date
#' @param end.date Character. End date
#' @param filters Character. Filters
#' @param sort Character. Sort order
#' @param start.index Numeric. Start index
#' @param max.results Numeric. Max results
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with GA-compatible data
#' @export
rym_get_ga <- function(counter,
                      dimensions = NULL,
                      metrics = "ga:sessions,ga:users",
                      start.date = "7daysAgo",
                      end.date = "yesterday",
                      filters = NULL,
                      sort = NULL,
                      start.index = 1,
                      max.results = 1000,
                      login = getOption("rym.user"),
                      token.path = getOption("rym.token_path")) {
  
  # Show deprecation info
  if (getOption("rym.show_deprecation_warnings", TRUE)) {
    rlang::inform(
      "Consider using RymGAAPI$new()$get_ga_data() for modern API access",
      .frequency = "once",
      .frequency_id = "rym_get_ga_modern"
    )
  }
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = FALSE
  )
  
  # Create GA client
  ga <- RymGAAPI$new(token = token)
  
  return(ga$get_ga_data(
    counter = counter,
    dimensions = dimensions,
    metrics = metrics,
    start_date = start.date,
    end_date = end.date,
    filters = filters,
    sort = sort,
    start_index = start.index,
    max_results = max.results
  ))
}
