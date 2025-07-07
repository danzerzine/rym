#' Upload API Client
#'
#' @description
#' Modern client for Yandex Metrica Upload APIs (expense, calls, offline conversions).
#'
#' @export
RymUploadAPI <- R6::R6Class(
  "RymUploadAPI",
  inherit = RymClient,
  
  public = list(
    #' Upload Expense Data
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param data Data frame. Expense data to upload
    #' @param comment Character. Upload comment
    #' @param provider Character. Data provider name
    #'
    #' @return Upload response
    upload_expense = function(counter_id, data, comment = NULL, provider = "rym") {
      private$upload_file(
        counter_id = counter_id,
        data = data,
        comment = comment,
        provider = provider,
        endpoint_path = "expense/upload"
      )
    },
    
    #' Delete Expense Data
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param data Data frame. Expense data to delete
    #' @param comment Character. Deletion comment
    #' @param provider Character. Data provider name
    #'
    #' @return Deletion response
    delete_expense = function(counter_id, data, comment = NULL, provider = "rym") {
      private$upload_file(
        counter_id = counter_id,
        data = data,
        comment = comment,
        provider = provider,
        endpoint_path = "expense/delete"
      )
    },
    
    #' Get Expense Uploads
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param limit Numeric. Results per page
    #' @param offset Numeric. Offset for pagination
    #'
    #' @return Data frame with upload information
    get_expense_uploads = function(counter_id, limit = 1000, offset = 0) {
      private$get_uploads(counter_id, "expense/uploadings", limit, offset)
    },
    
    #' Upload Calls Data
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param data Data frame. Calls data to upload
    #' @param client_id_type Character. CLIENT_ID or USER_ID
    #' @param new_goal_name Character. Optional new goal name
    #' @param comment Character. Upload comment
    #'
    #' @return Upload response
    upload_calls = function(counter_id, data, client_id_type = "CLIENT_ID", 
                           new_goal_name = NULL, comment = NULL) {
      # Build query parameters
      query <- list(client_id_type = client_id_type)
      if (!is.null(new_goal_name)) query$new_goal_name <- new_goal_name
      if (!is.null(comment)) query$comment <- comment
      
      private$upload_file(
        counter_id = counter_id,
        data = data,
        endpoint_path = "offline_conversions/upload_calls",
        additional_query = query
      )
    },
    
    #' Get Calls Uploads
    #'
    #' @param counter_id Numeric. Counter ID
    #'
    #' @return Data frame with upload information
    get_calls_uploads = function(counter_id) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/offline_conversions/calls_uploadings")
      response <- self$get_json("GET", endpoint)
      private$format_uploadings_response(response)
    },
    
    #' Upload Offline Conversions
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param data Data frame. Offline conversion data
    #' @param client_id_type Character. CLIENT_ID or USER_ID
    #' @param comment Character. Upload comment
    #'
    #' @return Upload response
    upload_offline_conversions = function(counter_id, data, client_id_type = "CLIENT_ID", comment = NULL) {
      query <- list(client_id_type = client_id_type)
      if (!is.null(comment)) query$comment <- comment
      
      private$upload_file(
        counter_id = counter_id,
        data = data,
        endpoint_path = "offline_conversions/upload",
        additional_query = query
      )
    },
    
    #' Get Offline Conversion Uploads
    #'
    #' @param counter_id Numeric. Counter ID
    #'
    #' @return Data frame with upload information
    get_offline_conversion_uploads = function(counter_id) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/offline_conversions/uploadings")
      response <- self$get_json("GET", endpoint)
      private$format_uploadings_response(response)
    },
    
    #' Enable/Disable Calls
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param enable Logical. Enable (TRUE) or disable (FALSE)
    #'
    #' @return Success status
    manage_calls = function(counter_id, enable = TRUE) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/offline_conversions/calls_extended_threshold")
      method <- if (enable) "POST" else "DELETE"
      response <- self$get_json(method, endpoint)
      
      cli::cli_alert_success("Calls {if(enable) 'enabled' else 'disabled'} for counter {counter_id}")
      return(response$success %||% TRUE)
    },
    
    #' Enable/Disable Offline Conversions
    #'
    #' @param counter_id Numeric. Counter ID
    #' @param enable Logical. Enable (TRUE) or disable (FALSE)
    #'
    #' @return Success status
    manage_offline_conversions = function(counter_id, enable = TRUE) {
      endpoint <- paste0("management/v1/counter/", counter_id, "/offline_conversions/extended_threshold")
      method <- if (enable) "POST" else "DELETE"
      response <- self$get_json(method, endpoint)
      
      cli::cli_alert_success("Offline conversions {if(enable) 'enabled' else 'disabled'} for counter {counter_id}")
      return(response$success %||% TRUE)
    }
  ),
  
  private = list(
    #' Generic File Upload Method
    upload_file = function(counter_id, data, comment = NULL, provider = "rym", 
                          endpoint_path, additional_query = list()) {
      # Create temporary file
      tf <- tempfile(fileext = ".csv")
      on.exit(file.remove(tf))
      
      # Save data to CSV
      readr::write_csv(data, tf)
      
      # Build endpoint
      endpoint <- paste0("management/v1/counter/", counter_id, "/", endpoint_path)
      
      # Build query parameters
      query <- list(provider = provider)
      if (!is.null(comment)) query$comment <- utils::URLencode(comment)
      query <- c(query, additional_query)
      
      # Perform upload
      tryCatch({
        response <- self$request("POST", endpoint, query = query) |>
          httr2::req_body_multipart(file = tf) |>
          httr2::req_perform()
        
        result <- httr2::resp_body_json(response)
        
        # Check for errors
        if (!is.null(result$errors)) {
          rlang::abort(paste(result$errors$error_type, ":", result$errors$message))
        }
        
        # Display success information
        upload_info <- result$uploading
        cli::cli_alert_success("Upload completed successfully")
        cli::cli_text("Upload ID: {upload_info$id}")
        cli::cli_text("Status: {upload_info$status}")
        cli::cli_text("Source Quantity: {upload_info$source_quantity}")
        
        return(result)
        
      }, error = function(e) {
        rlang::abort(paste("Upload failed:", e$message))
      })
    },
    
    #' Get Uploads with Pagination
    get_uploads = function(counter_id, endpoint_path, limit = 1000, offset = 0) {
      all_uploads <- list()
      
      repeat {
        endpoint <- paste0("management/v1/counter/", counter_id, "/", endpoint_path)
        query <- list(limit = limit, offset = offset)
        
        response <- self$get_json("GET", endpoint, query = query)
        
        if (is.null(response$uploadings) || length(response$uploadings) == 0) {
          break
        }
        
        all_uploads <- append(all_uploads, response$uploadings)
        
        # Check if we got less than limit (last page)
        if (length(response$uploadings) < limit) {
          break
        }
        
        offset <- offset + limit
      }
      
      return(private$format_uploadings_response(list(uploadings = all_uploads)))
    },
    
    #' Format Uploadings Response
    format_uploadings_response = function(response) {
      if (is.null(response$uploadings) || length(response$uploadings) == 0) {
        return(data.frame())
      }
      
      purrr::map_dfr(response$uploadings, function(upload) {
        upload_flat <- purrr::flatten(upload)
        
        data.frame(
          id = upload_flat$id %||% NA,
          counter_id = upload_flat$counter_id %||% NA,
          status = upload_flat$status %||% NA,
          creation_time = upload_flat$creation_time %||% NA,
          source_quantity = upload_flat$source_quantity %||% NA,
          line_quantity = upload_flat$line_quantity %||% NA,
          provider = upload_flat$provider %||% NA,
          type = upload_flat$type %||% NA,
          client_id_type = upload_flat$client_id_type %||% NA,
          comment = upload_flat$comment %||% NA,
          stringsAsFactors = FALSE
        )
      })
    }
  )
)
