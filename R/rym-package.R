#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' rym: Modern R Interface to Yandex Metrica API
#'
#' @description
#' The rym package provides a modern, robust interface to the Yandex Metrica API,
#' offering comprehensive access to analytics data with advanced features while
#' maintaining backward compatibility with existing scripts.
#'
#' @section Main Features:
#' \itemize{
#'   \item Modern R6-based API clients for object-oriented programming
#'   \item Enhanced OAuth2 authentication with automatic token refresh
#'   \item Secure credential storage using system keyring
#'   \item Parallel processing and progress tracking for large datasets
#'   \item Comprehensive error handling and validation
#'   \item Support for Management, Reporting, Logs, and GA-compatible APIs
#'   \item 100% backward compatibility with legacy functions
#' }
#'
#' @section Getting Started:
#' 
#' For modern API usage:
#' \preformatted{
#' # Authenticate
#' token <- rym_auth_modern("your_login@yandex.com")
#' 
#' # Create API clients
#' management <- RymManagementAPI$new(token = token)
#' reporting <- RymReportingAPI$new(token = token)
#' 
#' # Get data
#' counters <- management$get_counters()
#' data <- reporting$get_data(counters = counters$id[1])
#' }
#' 
#' For legacy compatibility:
#' \preformatted{
#' # Legacy authentication (still works)
#' rym_auth("your_login")
#' 
#' # Legacy functions
#' counters <- rym_get_counters()
#' data <- rym_get_data(counters = counters$id[1])
#' }
#'
#' @section API Clients:
#' \itemize{
#'   \item \code{\link{RymClient}} - Base client class
#'   \item \code{\link{RymManagementAPI}} - Counter management, goals, segments
#'   \item \code{\link{RymReportingAPI}} - Aggregated analytics data
#'   \item \code{\link{RymLogsAPI}} - Raw visit and hit data
#'   \item \code{\link{RymGAAPI}} - Google Analytics compatible interface
#' }
#'
#' @section Legacy Functions:
#' All original functions are maintained for backward compatibility:
#' \itemize{
#'   \item \code{\link{rym_auth}} - Authentication
#'   \item \code{\link{rym_get_counters}} - Get available counters
#'   \item \code{\link{rym_get_data}} - Get reporting data
#'   \item \code{\link{rym_get_logs}} - Get raw logs
#'   \item \code{\link{rym_get_goals}} - Get goals
#'   \item \code{\link{rym_get_segments}} - Get segments
#' }
#'
#' @section Configuration:
#' \itemize{
#'   \item \code{\link{rym_set_credentials}} - Set default credentials
#'   \item \code{\link{rym_get_credentials}} - View current configuration
#'   \item \code{\link{rym_check_dependencies}} - Check package dependencies
#' }
#'
#' @docType package
#' @name rym-package
#' @aliases rym
#' @author Alexey Seleznev \email{selesnow@gmail.com}
#' @references \url{https://yandex.ru/dev/metrika/doc/api2/concept/about-docpage}
NULL
