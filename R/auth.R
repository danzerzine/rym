#' Modern Yandex Metrica Authentication
#'
#' @description
#' Provides modern, secure authentication for Yandex Metrica API with 
#' automatic token refresh, secure credential storage, and backward compatibility.
#'
#' @param login Character. Yandex login/username
#' @param client_id Character. OAuth2 client ID (default uses package credentials)
#' @param client_secret Character. OAuth2 client secret (default uses package credentials)
#' @param new_user Logical. Force new authentication flow
#' @param token_path Character. Path to store authentication tokens
#' @param use_keyring Logical. Use system keyring for secure storage
#' @param interactive Logical. Allow interactive authentication prompts
#'
#' @return RymToken object with authentication details
#' @export
#'
#' @examples
#' \dontrun{
#' # Modern authentication with keyring
#' token <- rym_auth_modern("your_login@yandex.com")
#' 
#' # Legacy authentication (backward compatible)
#' token <- rym_auth("your_login")
#' }
rym_auth_modern <- function(login = getOption("rym.user"),
                           client_id = getOption("rym.client_id"),
                           client_secret = getOption("rym.client_secret"),
                           new_user = FALSE,
                           token_path = getOption("rym.token_path"),
                           use_keyring = TRUE,
                           interactive = TRUE) {
  
  # Set defaults
  if (is.null(client_id)) {
    client_id <- "5a87e45d5562421bb29bb9abd17321b3"  # Legacy hardcoded
  }
  
  if (is.null(client_secret)) {
    client_secret <- "04e7f096ce21483fb1c9861f68c017d7"  # Legacy hardcoded
  }
  
  if (is.null(token_path)) {
    token_path <- getwd()
  }
  
  if (is.null(login)) {
    if (interactive) {
      login <- readline("Enter your Yandex login: ")
    } else {
      rlang::abort("Login must be provided")
    }
  }
  
  # Try to load existing token
  existing_token <- NULL
  
  if (!new_user) {
    existing_token <- try_load_token(login, token_path, use_keyring)
    
    if (!is.null(existing_token)) {
      # Check if token needs refresh
      if (token_needs_refresh(existing_token)) {
        message("Refreshing expired token...")
        refreshed_token <- refresh_token(existing_token, client_id, client_secret)
        
        if (!is.null(refreshed_token)) {
          save_token(refreshed_token, login, token_path, use_keyring)
          cli::cli_alert_success("Token refreshed successfully")
          return(refreshed_token)
        } else {
          cli::cli_alert_warning("Token refresh failed, starting new authentication")
        }
      } else {
        expire_days <- as.numeric(existing_token$expire_at - Sys.time(), units = "days")
        cli::cli_alert_success("Using existing token (expires in {round(expire_days)} days)")
        return(existing_token)
      }
    }
  }
  
  # Start new authentication flow
  cli::cli_alert_info("Starting authentication flow for {login}")
  new_token <- start_auth_flow(login, client_id, client_secret, interactive)
  
  if (!is.null(new_token)) {
    save_token(new_token, login, token_path, use_keyring)
    cli::cli_alert_success("Authentication successful!")
    return(new_token)
  } else {
    rlang::abort("Authentication failed")
  }
}

#' Legacy Authentication Function (Backward Compatibility)
#'
#' @param login Character. Yandex login
#' @param new.user Logical. Force new user authentication
#' @param token.path Character. Path to store tokens
#'
#' @return RymToken object
#' @export
rym_auth <- function(login = getOption("rym.user"), 
                     new.user = FALSE, 
                     token.path = getOption("rym.token_path")) {
  
  # Show deprecation warning
  rlang::warn(
    "rym_auth() is deprecated. Please use rym_auth_modern() for better security and features.",
    .frequency = "once",
    .frequency_id = "rym_auth_deprecation"
  )
  
  # Call modern auth with legacy parameters
  rym_auth_modern(
    login = login,
    new_user = new.user,
    token_path = token.path,
    use_keyring = FALSE,  # Legacy mode doesn't use keyring
    interactive = TRUE
  )
}

#' Try to Load Existing Token
#'
#' @param login Character. User login
#' @param token_path Character. Path to token files
#' @param use_keyring Logical. Use system keyring
#'
#' @return RymToken object or NULL
#' @keywords internal
try_load_token <- function(login, token_path, use_keyring) {
  
  if (use_keyring && has_keyring_support()) {
    return(load_token_from_keyring(login))
  } else {
    return(load_token_from_file(login, token_path))
  }
}

#' Load Token from File (Legacy Method)
#'
#' @param login Character. User login
#' @param token_path Character. Path to token files
#'
#' @return RymToken object or NULL
#' @keywords internal
load_token_from_file <- function(login, token_path) {
  token_file <- file.path(token_path, paste0(login, ".rymAuth.RData"))
  
  if (file.exists(token_file)) {
    tryCatch({
      env <- new.env()
      load(token_file, envir = env)
      token <- env$token
      
      if (inherits(token, "RymToken") && validate_token_structure(token)) {
        return(token)
      }
    }, error = function(e) {
      cli::cli_alert_warning("Failed to load token from file: {e$message}")
    })
  }
  
  return(NULL)
}

#' Load Token from System Keyring
#'
#' @param login Character. User login
#'
#' @return RymToken object or NULL
#' @keywords internal
load_token_from_keyring <- function(login) {
  tryCatch({
    token_json <- keyring::key_get("rym_token", login)
    token_data <- jsonlite::fromJSON(token_json)
    
    # Reconstruct token object
    token <- create_rym_token(
      access_token = token_data$access_token,
      refresh_token = token_data$refresh_token,
      expires_in = token_data$expires_in,
      username = token_data$username,
      expire_at = as.POSIXct(token_data$expire_at)
    )
    
    return(token)
  }, error = function(e) {
    return(NULL)
  })
}

#' Save Token
#'
#' @param token RymToken object
#' @param login Character. User login
#' @param token_path Character. Path to save token
#' @param use_keyring Logical. Use system keyring
#'
#' @keywords internal
save_token <- function(token, login, token_path, use_keyring) {
  
  if (use_keyring && has_keyring_support()) {
    save_token_to_keyring(token, login)
  } else {
    save_token_to_file(token, login, token_path)
  }
}

#' Save Token to File (Legacy Method)
#'
#' @param token RymToken object
#' @param login Character. User login
#' @param token_path Character. Path to save token
#'
#' @keywords internal
save_token_to_file <- function(token, login, token_path) {
  if (!dir.exists(token_path)) {
    dir.create(token_path, recursive = TRUE)
  }
  
  token_file <- file.path(token_path, paste0(login, ".rymAuth.RData"))
  
  tryCatch({
    save(token, file = token_file)
    cli::cli_alert_info("Token saved to {token_file}")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to save token: {e$message}")
  })
}

#' Save Token to System Keyring
#'
#' @param token RymToken object
#' @param login Character. User login
#'
#' @keywords internal
save_token_to_keyring <- function(token, login) {
  tryCatch({
    token_data <- list(
      access_token = token$access_token,
      refresh_token = token$refresh_token,
      expires_in = token$expires_in,
      username = token$username,
      expire_at = as.character(token$expire_at)
    )
    
    token_json <- jsonlite::toJSON(token_data, auto_unbox = TRUE)
    keyring::key_set("rym_token", login, token_json)
    cli::cli_alert_info("Token saved to system keyring")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to save token to keyring: {e$message}")
  })
}

#' Start OAuth2 Authentication Flow
#'
#' @param login Character. User login
#' @param client_id Character. OAuth2 client ID
#' @param client_secret Character. OAuth2 client secret
#' @param interactive Logical. Allow interactive prompts
#'
#' @return RymToken object or NULL
#' @keywords internal
start_auth_flow <- function(login, client_id, client_secret, interactive) {
  
  # Build authorization URL
  auth_url <- paste0(
    "https://oauth.yandex.ru/authorize",
    "?response_type=code",
    "&client_id=", client_id,
    "&redirect_uri=https://selesnow.github.io/rym/getToken/get_code.html",
    "&force_confirm=", as.integer(FALSE),
    if (!is.null(login)) paste0("&login_hint=", URLencode(login)) else ""
  )
  
  if (interactive) {
    cli::cli_alert_info("Opening browser for authentication...")
    utils::browseURL(auth_url)
    
    # Get authorization code from user
    full_url <- readline(
      "After authorization, copy the full URL from your browser and paste it here: "
    )
    
    # Extract code from URL
    auth_code <- extract_auth_code(full_url)
    
    if (is.null(auth_code)) {
      cli::cli_alert_danger("Failed to extract authorization code from URL")
      return(NULL)
    }
    
    # Exchange code for token
    return(exchange_code_for_token(auth_code, client_id, client_secret, login))
    
  } else {
    cli::cli_alert_danger("Interactive authentication required but not allowed")
    return(NULL)
  }
}

#' Extract Authorization Code from URL
#'
#' @param url Character. Full redirect URL
#'
#' @return Character. Authorization code or NULL
#' @keywords internal
extract_auth_code <- function(url) {
  # Extract code parameter from URL
  code_match <- stringr::str_extract(url, "code=([^&]+)")
  
  if (is.na(code_match)) {
    return(NULL)
  }
  
  code <- stringr::str_replace(code_match, "code=", "")
  
  # Validate code format (should be 16 characters)
  if (nchar(gsub("[^0-9A-Za-z]", "", code)) != 16) {
    cli::cli_alert_warning("Invalid authorization code format")
    return(NULL)
  }
  
  return(code)
}

#' Exchange Authorization Code for Access Token
#'
#' @param code Character. Authorization code
#' @param client_id Character. OAuth2 client ID
#' @param client_secret Character. OAuth2 client secret
#' @param login Character. User login
#'
#' @return RymToken object or NULL
#' @keywords internal
exchange_code_for_token <- function(code, client_id, client_secret, login) {
  
  tryCatch({
    req <- httr2::request("https://oauth.yandex.ru/token") |>
      httr2::req_body_form(
        grant_type = "authorization_code",
        code = code,
        client_id = client_id,
        client_secret = client_secret
      ) |>
      httr2::req_timeout(30)
    
    resp <- httr2::req_perform(req)
    token_data <- httr2::resp_body_json(resp)
    
    # Check for errors
    if (!is.null(token_data$error)) {
      cli::cli_alert_danger("Token exchange failed: {token_data$error_description}")
      return(NULL)
    }
    
    # Create token object
    token <- create_rym_token(
      access_token = token_data$access_token,
      refresh_token = token_data$refresh_token,
      expires_in = token_data$expires_in,
      username = login
    )
    
    return(token)
    
  }, error = function(e) {
    cli::cli_alert_danger("Token exchange failed: {e$message}")
    return(NULL)
  })
}

#' Refresh Access Token
#'
#' @param token RymToken object
#' @param client_id Character. OAuth2 client ID
#' @param client_secret Character. OAuth2 client secret
#'
#' @return RymToken object or NULL
#' @keywords internal
refresh_token <- function(token, client_id, client_secret) {
  
  if (is.null(token$refresh_token)) {
    return(NULL)
  }
  
  tryCatch({
    req <- httr2::request("https://oauth.yandex.ru/token") |>
      httr2::req_body_form(
        grant_type = "refresh_token",
        refresh_token = token$refresh_token,
        client_id = client_id,
        client_secret = client_secret
      ) |>
      httr2::req_timeout(30)
    
    resp <- httr2::req_perform(req)
    token_data <- httr2::resp_body_json(resp)
    
    # Check for errors
    if (!is.null(token_data$error)) {
      cli::cli_alert_danger("Token refresh failed: {token_data$error_description}")
      return(NULL)
    }
    
    # Update token
    token$access_token <- token_data$access_token
    token$expires_in <- token_data$expires_in
    token$expire_at <- Sys.time() + as.numeric(token_data$expires_in)
    
    return(token)
    
  }, error = function(e) {
    cli::cli_alert_danger("Token refresh failed: {e$message}")
    return(NULL)
  })
}

#' Create RymToken Object
#'
#' @param access_token Character. Access token
#' @param refresh_token Character. Refresh token
#' @param expires_in Numeric. Token expiry in seconds
#' @param username Character. Username
#' @param expire_at POSIXct. Explicit expiry time (optional)
#'
#' @return RymToken object
#' @keywords internal
create_rym_token <- function(access_token, refresh_token, expires_in, username, expire_at = NULL) {
  
  if (is.null(expire_at)) {
    expire_at <- Sys.time() + as.numeric(expires_in)
  }
  
  token <- list(
    access_token = access_token,
    refresh_token = refresh_token,
    expires_in = expires_in,
    username = username,
    expire_at = expire_at
  )
  
  class(token) <- "RymToken"
  return(token)
}

#' Check if Token Needs Refresh
#'
#' @param token RymToken object
#' @param threshold_days Numeric. Days before expiry to refresh
#'
#' @return Logical
#' @keywords internal
token_needs_refresh <- function(token, threshold_days = 7) {
  if (is.null(token$expire_at)) {
    return(TRUE)
  }
  
  days_until_expiry <- as.numeric(token$expire_at - Sys.time(), units = "days")
  return(days_until_expiry < threshold_days)
}

#' Validate Token Structure
#'
#' @param token Object to validate
#'
#' @return Logical
#' @keywords internal
validate_token_structure <- function(token) {
  required_fields <- c("access_token", "username")
  return(all(required_fields %in% names(token)))
}

#' Check Keyring Support
#'
#' @return Logical
#' @keywords internal
has_keyring_support <- function() {
  tryCatch({
    keyring::default_backend()
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Print Method for RymToken
#'
#' @param x RymToken object
#' @param ... Additional arguments
#'
#' @export
print.RymToken <- function(x, ...) {
  expire_info <- if (!is.null(x$expire_at)) {
    days_left <- round(as.numeric(x$expire_at - Sys.time(), units = "days"))
    paste0(" (expires in ", days_left, " days)")
  } else {
    ""
  }
  
  cli::cli_text("Yandex Metrica Token")
  cli::cli_text("User: {.strong {x$username}}")
  cli::cli_text("Status: {.success Active}{expire_info}")
  
  invisible(x)
}
