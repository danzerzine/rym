#' Package Initialization Functions
#'
#' @description
#' Functions that run when the package is loaded or attached.

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(rymWelcomeMessage())
  
  packageStartupMessage("rym 2.0 configuration:")
  
  ## Check dependencies
  packageStartupMessage("...Checking dependencies: ", appendLF = FALSE)
  if (rym_check_dependencies(verbose = FALSE)) {
    packageStartupMessage("OK")
  } else {
    packageStartupMessage("Some dependencies missing")
  }
  
  ## token path
  packageStartupMessage("...Token path: ", appendLF = FALSE)
  if (Sys.getenv("RYM_TOKEN_PATH") != "") {
    packageStartupMessage("set via environment")
  } else if (!is.null(getOption("rym.token_path"))) {
    packageStartupMessage("set via options")
  } else {
    packageStartupMessage("using default")
  }
  
  ## username
  packageStartupMessage("...Default user: ", appendLF = FALSE)
  if (Sys.getenv("RYM_USER") != "") {
    packageStartupMessage("set via environment")
  } else if (!is.null(getOption("rym.user"))) {
    packageStartupMessage("set via options")
  } else {
    packageStartupMessage("none")
  }
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  
  op.rym <- list(
    rym.user = Sys.getenv("RYM_USER"),
    rym.token_path = Sys.getenv("RYM_TOKEN_PATH"),
    rym.client_id = Sys.getenv("RYM_CLIENT_ID"),
    rym.client_secret = Sys.getenv("RYM_CLIENT_SECRET"),
    rym.show_deprecation_warnings = TRUE,
    rym.progress = TRUE
  )
  
  # Convert empty strings to NULL
  op.rym <- lapply(op.rym, function(x) if (x == "") return(NULL) else return(x))
  
  # Only set options that aren't already set
  toset <- !(names(op.rym) %in% names(op))
  if (any(toset)) options(op.rym[toset])
  
  invisible()
}

#' Package Welcome Message
#'
#' @return Character string with welcome message
#' @keywords internal
rymWelcomeMessage <- function() {
  paste0(
    "\n",
    "---------------------\n",
    "Welcome to rym version ", utils::packageDescription("rym")$Version, " - Modern R Interface to Yandex.Metrica API.\n",
    "\n",
    "NEW in v2.0:\n",
    " * Modern R6-based API clients\n",
    " * Enhanced authentication with keyring support\n",
    " * Parallel processing and progress tracking\n",
    " * Comprehensive error handling\n",
    " * 100% backward compatibility\n",
    "\n",
    "Documentation:\n",
    " * Package website: https://selesnow.github.io/rym\n",
    " * Getting started: vignette('intro-to-rym', package = 'rym')\n",
    " * Modern API: ?RymClient, ?RymManagementAPI, ?RymReportingAPI\n",
    "\n",
    "Quick setup:\n",
    " * Modern auth: rym_auth_modern('your_login')\n",
    " * Legacy auth: rym_auth('your_login')\n",
    " * Check config: rym_get_credentials()\n",
    "\n",
    "TIP: Use modern API clients for best performance and features!\n",
    "\n",
    "Authors: Alexey Seleznev, Community Contributors\n",
    "Support: https://github.com/selesnow/rym/issues\n",
    "\n",
    "To suppress this message: suppressPackageStartupMessages(library(rym))\n",
    "---------------------\n"
  )
}
