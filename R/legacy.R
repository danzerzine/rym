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

#' Upload Expense (Legacy Stub)
#'
#' @param ... Arguments for expense upload
#'
#' @return Upload response
#' @export
rym_upload_expense <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Delete Uploaded Expense (Legacy Stub)
#'
#' @param ... Arguments for expense deletion
#'
#' @return Deletion response
#' @export
rym_delete_uploaded_expense <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Get Uploadings Expense (Legacy Stub)
#'
#' @param ... Arguments for getting expense uploads
#'
#' @return Upload list
#' @export
rym_get_uploadings_expense <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Upload Calls (Legacy Stub)
#'
#' @param ... Arguments for calls upload
#'
#' @return Upload response
#' @export
rym_upload_calls <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Enable Calls (Legacy Stub)
#'
#' @param ... Arguments for enabling calls
#'
#' @return Response
#' @export
rym_enable_calls <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Disable Calls (Legacy Stub)
#'
#' @param ... Arguments for disabling calls
#'
#' @return Response
#' @export
rym_disable_calls <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Allow Calls (Legacy Stub)
#'
#' @param ... Arguments for allowing calls
#'
#' @return Response
#' @export
rym_allow_calls <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Get Uploadings Calls (Legacy Stub)
#'
#' @param ... Arguments for getting call uploads
#'
#' @return Upload list
#' @export
rym_get_uploadings_calls <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Upload Offline Conversion (Legacy Stub)
#'
#' @param ... Arguments for offline conversion upload
#'
#' @return Upload response
#' @export
rym_upload_offline_conversion <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Enable Offline Conversion (Legacy Stub)
#'
#' @param ... Arguments for enabling offline conversions
#'
#' @return Response
#' @export
rym_enable_offline_conversion <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Disable Offline Conversion (Legacy Stub)
#'
#' @param ... Arguments for disabling offline conversions
#'
#' @return Response
#' @export
rym_disable_offline_conversion <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Allow Offline Conversion (Legacy Stub)
#'
#' @param ... Arguments for allowing offline conversions
#'
#' @return Response
#' @export
rym_allow_offline_conversion <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}

#' Get Uploadings Offline Conversions (Legacy Stub)
#'
#' @param ... Arguments for getting offline conversion uploads
#'
#' @return Upload list
#' @export
rym_get_uploadings_offline_conversions <- function(...) {
  rlang::abort(
    "Upload functionality not yet implemented in modern version. Please use legacy package for now.",
    class = "rym_not_implemented"
  )
}
