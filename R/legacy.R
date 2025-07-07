#' Legacy Functions for Backward Compatibility
#'
#' @description
#' These functions maintain backward compatibility with existing scripts
#' while internally using the modern API clients.

#' Get Counters (Legacy)
#'
#' @param login Character. Yandex login
#' @param token.path Character. Path to token files
#' @param search.string Character. Search filter
#'
#' @return Data frame with counter information
#' @export
rym_get_counters <- function(login = getOption("rym.user"),
                            token.path = getOption("rym.token_path"),
                            search.string = NULL) {
  
  # Show deprecation info
  if (getOption("rym.show_deprecation_warnings", TRUE)) {
    rlang::inform(
      "Consider using RymManagementAPI$new()$get_counters() for modern API access",
      .frequency = "once",
      .frequency_id = "rym_get_counters_modern"
    )
  }
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create management client and get counters
  mgmt <- RymManagementAPI$new(token = token)
  return(mgmt$get_counters(search_string = search.string))
}

#' Get Data (Legacy)
#'
#' @param direct.client.logins Character. Direct client logins
#' @param counters Character/Numeric. Counter IDs
#' @param metrics Character. Metrics to retrieve
#' @param dimensions Character. Dimensions
#' @param filters Character. Filters
#' @param sort Character. Sort order
#' @param date.from Character. Start date
#' @param date.to Character. End date
#' @param accuracy Character. Data accuracy
#' @param include.undefined Logical. Include undefined values
#' @param lang Character. Language
#' @param timezone Character. Timezone
#' @param pretty Logical. Pretty format
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with reporting data
#' @export
rym_get_data <- function(direct.client.logins = NULL,
                        counters = NULL,
                        metrics = "ym:s:visits,ym:s:pageviews,ym:s:users",
                        dimensions = NULL,
                        filters = NULL,
                        sort = NULL,
                        date.from = "8daysAgo",
                        date.to = "yesterday",
                        accuracy = "full",
                        include.undefined = TRUE,
                        lang = "ru",
                        timezone = NULL,
                        pretty = FALSE,
                        login = getOption("rym.user"),
                        token.path = getOption("rym.token_path")) {
  
  # Show deprecation info
  if (getOption("rym.show_deprecation_warnings", TRUE)) {
    rlang::inform(
      "Consider using RymReportingAPI$new()$get_data() for modern API access",
      .frequency = "once",
      .frequency_id = "rym_get_data_modern"
    )
  }
  
  # Use modern auth with interactive mode
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create reporting client
  reporting <- RymReportingAPI$new(token = token)
  
  # Use modern get_data method
  tryCatch({
    result <- reporting$get_data(
      counters = counters,
      metrics = metrics,
      dimensions = dimensions,
      date_from = date.from,
      date_to = date.to,
      filters = filters,
      sort = sort,
      accuracy = accuracy,
      include_undefined = include.undefined,
      lang = lang
    )
    
    return(result)
    
  }, error = function(e) {
    # Provide helpful error message
    rlang::abort(
      paste("Data retrieval failed:", e$message),
      parent = e,
      class = "rym_legacy_error"
    )
  })
}

#' Get Goals (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with goals
#' @export
rym_get_goals <- function(counter,
                         login = getOption("rym.user"),
                         token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create management client
  mgmt <- RymManagementAPI$new(token = token)
  return(mgmt$get_goals(counter))
}

#' Get Segments (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with segments
#' @export
rym_get_segments <- function(counter,
                            login = getOption("rym.user"),
                            token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create management client
  mgmt <- RymManagementAPI$new(token = token)
  return(mgmt$get_segments(counter))
}

#' Get Filters (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with filters
#' @export
rym_get_filters <- function(counter,
                           login = getOption("rym.user"),
                           token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client and make request
  client <- RymClient$new(token = token)
  
  endpoint <- paste0("management/v1/counter/", counter, "/filters")
  response <- client$get_json("GET", endpoint)
  
  if (length(response$filters) == 0) {
    cli::cli_alert_warning("No filters found for counter {counter}")
    return(data.frame())
  }
  
  # Convert to data frame
  filters_df <- purrr::map_dfr(response$filters, function(filter) {
    data.frame(
      id = filter$id %||% NA,
      attr = filter$attr %||% NA,
      type = filter$type %||% NA,
      value = filter$value %||% NA,
      action = filter$action %||% NA,
      status = filter$status %||% NA,
      stringsAsFactors = FALSE
    )
  })
  
  return(filters_df)
}

#' Add Goal (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param name Character. Goal name
#' @param type Character. Goal type
#' @param conditions List. Goal conditions
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Goal creation response
#' @export
rym_add_goal <- function(counter, name, type, conditions = list(),
                        login = getOption("rym.user"),
                        token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create management client
  mgmt <- RymManagementAPI$new(token = token)
  return(mgmt$add_goal(counter, name, type, conditions))
}

#' Add Segment (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param name Character. Segment name
#' @param expression Character. Segment expression
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Segment creation response
#' @export
rym_add_segment <- function(counter, name, expression,
                           login = getOption("rym.user"),
                           token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client and make request
  client <- RymClient$new(token = token)
  
  endpoint <- paste0("management/v1/counter/", counter, "/apisegments")
  
  body <- list(
    segment = list(
      name = name,
      expression = expression
    )
  )
  
  response <- client$get_json("POST", endpoint, body = body)
  cli::cli_alert_success("Segment '{name}' created successfully")
  
  return(response)
}

#' Get User Grants (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with user permissions
#' @export
rym_users_grants <- function(counter,
                            login = getOption("rym.user"),
                            token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client and make request
  client <- RymClient$new(token = token)
  
  endpoint <- paste0("management/v1/counter/", counter, "/grants")
  response <- client$get_json("GET", endpoint)
  
  if (length(response$grants) == 0) {
    cli::cli_alert_warning("No grants found for counter {counter}")
    return(data.frame())
  }
  
  # Convert to data frame
  grants_df <- purrr::map_dfr(response$grants, function(grant) {
    data.frame(
      user_login = grant$user_login %||% NA,
      perm = grant$perm %||% NA,
      created_at = grant$created_at %||% NA,
      stringsAsFactors = FALSE
    )
  })
  
  return(grants_df)
}

#' Get My Logins (Legacy)
#'
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Character vector of available logins
#' @export
rym_get_my_logins <- function(login = getOption("rym.user"),
                             token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client and make request
  client <- RymClient$new(token = token)
  
  response <- client$get_json("GET", "management/v1/clients")
  
  if (length(response$clients) == 0) {
    cli::cli_alert_warning("No client logins found")
    return(character(0))
  }
  
  # Extract logins
  logins <- purrr::map_chr(response$clients, function(client) {
    client$chief_login %||% NA
  })
  
  # Remove NAs and return unique logins
  return(unique(logins[!is.na(logins)]))
}

#' Get Direct Clients (Legacy)
#'
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with direct clients
#' @export
rym_get_direct_clients <- function(login = getOption("rym.user"),
                                  token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client and make request
  client <- RymClient$new(token = token)
  
  response <- client$get_json("GET", "management/v1/clients")
  
  if (length(response$clients) == 0) {
    cli::cli_alert_warning("No direct clients found")
    return(data.frame())
  }
  
  # Convert to data frame
  clients_df <- purrr::map_dfr(response$clients, function(client) {
    data.frame(
      login = client$chief_login %||% NA,
      name = client$name %||% NA,
      stringsAsFactors = FALSE
    )
  })
  
  return(clients_df)
}

# Additional legacy functions for upload operations...
# These would implement the upload functionality using modern clients

#' Upload Expense (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param data Data frame. Expense data to upload
#' @param comment Character. Upload comment
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Upload response
#' @export
rym_upload_expense <- function(counter,
                              data,
                              comment = paste0("Upload by rym at ", Sys.time()),
                              login = getOption("rym.user"),
                              token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Create temporary file
  tf <- tempfile(fileext = ".csv")
  on.exit(file.remove(tf))
  
  # Save data to CSV
  readr::write_csv(data, tf)
  
  # URL encode comment
  comment_encoded <- utils::URLencode(comment)
  
  # Upload endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/expense/upload")
  
  # Build query parameters
  query <- list(
    provider = "rym",
    comment = comment_encoded
  )
  
  # Perform upload
  tryCatch({
    response <- client$request("POST", endpoint, query = query) |>
      httr2::req_body_multipart(file = tf) |>
      httr2::req_perform()
    
    result <- httr2::resp_body_json(response)
    
    # Check for errors
    if (!is.null(result$errors)) {
      rlang::abort(paste(result$errors$error_type, ":", result$errors$message))
    }
    
    # Display upload information
    cli::cli_alert_success("Expense upload completed")
    cli::cli_text("Upload ID: {result$uploading$id}")
    cli::cli_text("Source Quantity: {result$uploading$source_quantity}")
    cli::cli_text("Provider: {result$uploading$provider}")
    cli::cli_text("Type: {result$uploading$type}")
    cli::cli_text("Status: {result$uploading$status}")
    
    return(result)
    
  }, error = function(e) {
    rlang::abort(paste("Upload failed:", e$message))
  })
}

#' Delete Uploaded Expense (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param data Data frame. Expense data to delete
#' @param comment Character. Deletion comment
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Deletion response
#' @export
rym_delete_uploaded_expense <- function(counter,
                                       data,
                                       comment = paste0("Delete by rym at ", Sys.time()),
                                       login = getOption("rym.user"),
                                       token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Create temporary file
  tf <- tempfile(fileext = ".csv")
  on.exit(file.remove(tf))
  
  # Save data to CSV
  readr::write_csv(data, tf)
  
  # URL encode comment
  comment_encoded <- utils::URLencode(comment)
  
  # Delete endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/expense/delete")
  
  # Build query parameters
  query <- list(
    provider = "rym",
    comment = comment_encoded
  )
  
  # Perform deletion
  tryCatch({
    response <- client$request("POST", endpoint, query = query) |>
      httr2::req_body_multipart(file = tf) |>
      httr2::req_perform()
    
    result <- httr2::resp_body_json(response)
    
    # Check for errors
    if (!is.null(result$errors)) {
      rlang::abort(paste(result$errors$error_type, ":", result$errors$message))
    }
    
    # Display deletion information
    cli::cli_alert_success("Expense deletion completed")
    cli::cli_text("Upload ID: {result$uploading$id}")
    cli::cli_text("Source Quantity: {result$uploading$source_quantity}")
    cli::cli_text("Provider: {result$uploading$provider}")
    cli::cli_text("Type: {result$uploading$type}")
    cli::cli_text("Status: {result$uploading$status}")
    
    return(result)
    
  }, error = function(e) {
    rlang::abort(paste("Deletion failed:", e$message))
  })
}

#' Get Uploadings Expense (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with upload information
#' @export
rym_get_uploadings_expense <- function(counter,
                                      login = getOption("rym.user"),
                                      token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Get uploadings with pagination
  all_uploadings <- list()
  limit <- 10000
  offset <- 0
  
  repeat {
    endpoint <- paste0("management/v1/counter/", counter, "/expense/uploadings")
    
    query <- list(
      limit = limit,
      offset = offset
    )
    
    response <- client$get_json("GET", endpoint, query = query)
    
    # Check if we have uploadings
    if (is.null(response$uploadings) || length(response$uploadings) == 0) {
      break
    }
    
    # Add to results
    all_uploadings <- append(all_uploadings, response$uploadings)
    
    # Update offset for next page
    offset <- offset + limit
    
    # Break if we got less than limit (last page)
    if (length(response$uploadings) < limit) {
      break
    }
  }
  
  # Convert to data frame
  if (length(all_uploadings) > 0) {
    result <- purrr::map_dfr(all_uploadings, function(upload) {
      # Flatten nested structures
      upload_flat <- purrr::flatten(upload)
      
      # Convert to data frame row
      data.frame(
        id = upload_flat$id %||% NA,
        counter_id = upload_flat$counter_id %||% NA,
        status = upload_flat$status %||% NA,
        creation_time = upload_flat$creation_time %||% NA,
        source_quantity = upload_flat$source_quantity %||% NA,
        line_quantity = upload_flat$line_quantity %||% NA,
        provider = upload_flat$provider %||% NA,
        type = upload_flat$type %||% NA,
        comment = upload_flat$comment %||% NA,
        stringsAsFactors = FALSE
      )
    })
    
    return(result)
  } else {
    cli::cli_alert_warning("No expense uploads found for counter {counter}")
    return(data.frame())
  }
}

#' Upload Calls (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param data Data frame. Calls data to upload
#' @param client.id.type Character. Client ID type (CLIENT_ID or USER_ID)
#' @param new.goal.name Character. New goal name (optional)
#' @param comment Character. Upload comment
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Upload response
#' @export
rym_upload_calls <- function(counter,
                            data,
                            client.id.type = c("CLIENT_ID", "USER_ID"),
                            new.goal.name = NULL,
                            comment = paste0("Upload by rym at ", Sys.time()),
                            login = getOption("rym.user"),
                            token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Match client ID type argument
  client.id.type <- match.arg(client.id.type)
  
  # Create temporary file
  tf <- tempfile(fileext = ".csv")
  on.exit(file.remove(tf))
  
  # Save data to CSV
  readr::write_csv(data, tf)
  
  # URL encode comment
  comment_encoded <- utils::URLencode(comment)
  
  # Upload endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/upload_calls")
  
  # Build query parameters
  query <- list(
    client_id_type = client.id.type,
    comment = comment_encoded
  )
  
  # Add new goal name if provided
  if (!is.null(new.goal.name)) {
    query$new_goal_name <- utils::URLencode(new.goal.name)
  }
  
  # Perform upload
  tryCatch({
    response <- client$request("POST", endpoint, query = query) |>
      httr2::req_body_multipart(file = tf) |>
      httr2::req_perform()
    
    result <- httr2::resp_body_json(response)
    
    # Check for errors
    if (!is.null(result$errors)) {
      rlang::abort(paste(result$errors$error_type, ":", result$errors$message))
    }
    
    # Display upload information
    cli::cli_alert_success("Calls upload completed")
    cli::cli_text("Upload ID: {result$uploading$id}")
    cli::cli_text("Status: {result$uploading$status}")
    cli::cli_text("Source Quantity: {result$uploading$source_quantity}")
    cli::cli_text("Line Quantity: {result$uploading$line_quantity}")
    cli::cli_text("Client ID Type: {result$uploading$client_id_type}")
    cli::cli_text("Comment: {result$uploading$comment}")
    
    return(result)
    
  }, error = function(e) {
    rlang::abort(paste("Calls upload failed:", e$message))
  })
}

#' Enable Calls (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Boolean. Success status
#' @export
rym_enable_calls <- function(counter,
                            login = getOption("rym.user"),
                            token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Enable calls endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/calls_extended_threshold")
  
  tryCatch({
    response <- client$get_json("POST", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(response$message)
    }
    
    cli::cli_alert_success("Calls enabled for counter {counter}")
    return(response$success)
    
  }, error = function(e) {
    rlang::abort(paste("Failed to enable calls:", e$message))
  })
}

#' Disable Calls (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Boolean. Success status
#' @export
rym_disable_calls <- function(counter,
                             login = getOption("rym.user"),
                             token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Disable calls endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/calls_extended_threshold")
  
  tryCatch({
    response <- client$get_json("DELETE", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(response$message)
    }
    
    cli::cli_alert_success("Calls disabled for counter {counter}")
    return(response$success)
    
  }, error = function(e) {
    rlang::abort(paste("Failed to disable calls:", e$message))
  })
}

#' Allow Calls (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Character. Date information
#' @export
rym_allow_calls <- function(counter,
                           login = getOption("rym.user"),
                           token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Allow calls endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/calls_visit_join_threshold")
  
  tryCatch({
    response <- client$get_json("GET", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(response$message)
    }
    
    cli::cli_alert_success("Calls allowed for counter {counter}")
    cli::cli_text("Date: {response$date}")
    return(response$date)
    
  }, error = function(e) {
    rlang::abort(paste("Failed to allow calls:", e$message))
  })
}

#' Get Uploadings Calls (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with call upload information
#' @export
rym_get_uploadings_calls <- function(counter,
                                    login = getOption("rym.user"),
                                    token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Get calls uploadings endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/calls_uploadings")
  
  tryCatch({
    response <- client$get_json("GET", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(paste(response$errors$error_type, ":", response$errors$message))
    }
    
    # Convert to data frame
    if (!is.null(response$uploadings) && length(response$uploadings) > 0) {
      result <- purrr::map_dfr(response$uploadings, function(upload) {
        # Flatten nested structures
        upload_flat <- purrr::flatten(upload)
        
        # Convert to data frame row
        data.frame(
          id = upload_flat$id %||% NA,
          counter_id = upload_flat$counter_id %||% NA,
          status = upload_flat$status %||% NA,
          creation_time = upload_flat$creation_time %||% NA,
          source_quantity = upload_flat$source_quantity %||% NA,
          line_quantity = upload_flat$line_quantity %||% NA,
          client_id_type = upload_flat$client_id_type %||% NA,
          comment = upload_flat$comment %||% NA,
          stringsAsFactors = FALSE
        )
      })
      
      return(result)
    } else {
      cli::cli_alert_warning("No call uploads found for counter {counter}")
      return(data.frame())
    }
    
  }, error = function(e) {
    rlang::abort(paste("Failed to get calls uploadings:", e$message))
  })
}

#' Upload Offline Conversion (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param data Data frame. Offline conversion data to upload
#' @param client.id.type Character. Client ID type (CLIENT_ID or USER_ID)
#' @param comment Character. Upload comment
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Upload response
#' @export
rym_upload_offline_conversion <- function(counter,
                                         data,
                                         client.id.type = c("CLIENT_ID", "USER_ID"),
                                         comment = paste0("Upload by rym at ", Sys.time()),
                                         login = getOption("rym.user"),
                                         token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Match client ID type argument
  client.id.type <- match.arg(client.id.type)
  
  # Create temporary file
  tf <- tempfile(fileext = ".csv")
  on.exit(file.remove(tf))
  
  # Save data to CSV
  readr::write_csv(data, tf)
  
  # URL encode comment
  comment_encoded <- utils::URLencode(comment)
  
  # Upload endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/upload")
  
  # Build query parameters
  query <- list(
    client_id_type = client.id.type,
    comment = comment_encoded
  )
  
  # Perform upload
  tryCatch({
    response <- client$request("POST", endpoint, query = query) |>
      httr2::req_body_multipart(file = tf) |>
      httr2::req_perform()
    
    result <- httr2::resp_body_json(response)
    
    # Check for errors
    if (!is.null(result$errors)) {
      rlang::abort(paste(result$errors$error_type, ":", result$errors$message))
    }
    
    # Display upload information
    cli::cli_alert_success("Offline conversion upload completed")
    cli::cli_text("Upload ID: {result$uploading$id}")
    cli::cli_text("Source Quantity: {result$uploading$source_quantity}")
    cli::cli_text("Status: {result$uploading$status}")
    
    return(result)
    
  }, error = function(e) {
    rlang::abort(paste("Offline conversion upload failed:", e$message))
  })
}

#' Enable Offline Conversion (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Boolean. Success status
#' @export
rym_enable_offline_conversion <- function(counter,
                                         login = getOption("rym.user"),
                                         token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Enable offline conversion endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/extended_threshold")
  
  tryCatch({
    response <- client$get_json("POST", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(response$message)
    }
    
    cli::cli_alert_success("Offline conversions enabled for counter {counter}")
    return(response$success)
    
  }, error = function(e) {
    rlang::abort(paste("Failed to enable offline conversions:", e$message))
  })
}

#' Disable Offline Conversion (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Boolean. Success status
#' @export
rym_disable_offline_conversion <- function(counter,
                                          login = getOption("rym.user"),
                                          token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Disable offline conversion endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/extended_threshold")
  
  tryCatch({
    response <- client$get_json("DELETE", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(response$message)
    }
    
    cli::cli_alert_success("Offline conversions disabled for counter {counter}")
    return(response$success)
    
  }, error = function(e) {
    rlang::abort(paste("Failed to disable offline conversions:", e$message))
  })
}

#' Allow Offline Conversion (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Character. Date information
#' @export
rym_allow_offline_conversion <- function(counter,
                                        login = getOption("rym.user"),
                                        token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Allow offline conversion endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/visit_join_threshold")
  
  tryCatch({
    response <- client$get_json("GET", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(response$message)
    }
    
    cli::cli_alert_success("Offline conversions allowed for counter {counter}")
    cli::cli_text("Date: {response$date}")
    return(response$date)
    
  }, error = function(e) {
    rlang::abort(paste("Failed to allow offline conversions:", e$message))
  })
}

#' Get Uploadings Offline Conversions (Legacy)
#'
#' @param counter Numeric. Counter ID
#' @param login Character. User login
#' @param token.path Character. Token path
#'
#' @return Data frame with offline conversion upload information
#' @export
rym_get_uploadings_offline_conversions <- function(counter,
                                                  login = getOption("rym.user"),
                                                  token.path = getOption("rym.token_path")) {
  
  # Use modern auth
  token <- rym_auth_modern(
    login = login,
    token_path = token.path,
    use_keyring = FALSE,
    interactive = TRUE
  )
  
  # Create client
  client <- RymClient$new(token = token)
  
  # Get offline conversions uploadings endpoint
  endpoint <- paste0("management/v1/counter/", counter, "/offline_conversions/uploadings")
  
  tryCatch({
    response <- client$get_json("GET", endpoint)
    
    # Check for errors
    if (!is.null(response$errors)) {
      rlang::abort(paste(response$errors$error_type, ":", response$errors$message))
    }
    
    # Convert to data frame
    if (!is.null(response$uploadings) && length(response$uploadings) > 0) {
      result <- purrr::map_dfr(response$uploadings, function(upload) {
        # Flatten nested structures
        upload_flat <- purrr::flatten(upload)
        
        # Convert to data frame row
        data.frame(
          id = upload_flat$id %||% NA,
          counter_id = upload_flat$counter_id %||% NA,
          status = upload_flat$status %||% NA,
          creation_time = upload_flat$creation_time %||% NA,
          source_quantity = upload_flat$source_quantity %||% NA,
          line_quantity = upload_flat$line_quantity %||% NA,
          client_id_type = upload_flat$client_id_type %||% NA,
          comment = upload_flat$comment %||% NA,
          stringsAsFactors = FALSE
        )
      })
      
      return(result)
    } else {
      cli::cli_alert_warning("No offline conversion uploads found for counter {counter}")
      return(data.frame())
    }
    
  }, error = function(e) {
    rlang::abort(paste("Failed to get offline conversion uploadings:", e$message))
  })
}
