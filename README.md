# rym 2.0 - Modern R Interface to Yandex Metrica API

<a href='https://selesnow.github.io/rym/'><img src='https://raw.githubusercontent.com/selesnow/rym/master/inst/logo/rym.png' align="right" height="139" /></a>

[![CRAN status](https://www.r-pkg.org/badges/version/rym)](https://CRAN.R-project.org/package=rym)
[![R-CMD-check](https://github.com/selesnow/rym/workflows/R-CMD-check/badge.svg)](https://github.com/selesnow/rym/actions)
[![Codecov test coverage](https://codecov.io/gh/selesnow/rym/branch/master/graph/badge.svg)](https://codecov.io/gh/selesnow/rym?branch=master)

## Overview

`rym` is a modern, robust R interface for the Yandex Metrica API, providing comprehensive access to analytics data with advanced features while maintaining backward compatibility with existing scripts.

### Key Features

- **Modern Architecture**: Built with R6 classes, modern dependencies, and best practices
- **Secure Authentication**: Enhanced OAuth2 flow with automatic token refresh and keyring support
- **High Performance**: Parallel processing and optimized data retrieval
- **Multiple APIs**: Complete support for Management, Reporting, Logs, and GA-compatible APIs
- **Backward Compatible**: Existing scripts continue to work without modification
- **Rich Functionality**: Advanced error handling, progress tracking, and data validation

## Installation

### Latest Version (Recommended)

```r
# Install from GitHub for the latest features
devtools::install_github("selesnow/rym")

# Or using remotes
remotes::install_github("selesnow/rym")
```

### CRAN Version

```r
install.packages("rym")
```

## Quick Start

### Modern API (Recommended)

```r
library(rym)

# Modern authentication
token <- rym_auth_modern("your_login@yandex.com")

# Create API clients
management <- RymManagementAPI$new(token = token)
reporting <- RymReportingAPI$new(token = token)
logs <- RymLogsAPI$new(token = token)

# Get your counters
counters <- management$get_counters()
head(counters)

# Get reporting data
data <- reporting$get_data(
  counters = counters$id[1],
  metrics = c("ym:s:visits", "ym:s:pageviews", "ym:s:users"),
  dimensions = "ym:s:date",
  date_from = "30daysAgo",
  date_to = "yesterday"
)

# Get raw logs
log_data <- logs$get_logs(
  counter_id = counters$id[1],
  date_from = Sys.Date() - 7,
  date_to = Sys.Date() - 1,
  fields = "ym:s:date,ym:s:startURL,ym:s:visitDuration"
)
```

### Legacy API (Backward Compatible)

```r
library(rym)

# Legacy authentication (still works!)
rym_auth(login = "your_login")

# Get counters
my_counters <- rym_get_counters()

# Get reporting data
report_data <- rym_get_data(
  counters = my_counters$id[1],
  date.from = "2024-01-01",
  date.to = "yesterday",
  dimensions = "ym:s:date,ym:s:lastTrafficSource",
  metrics = "ym:s:visits,ym:s:pageviews,ym:s:users"
)

# Get goals
goals <- rym_get_goals(counter = my_counters$id[1])

# Get logs
logs <- rym_get_logs(
  counter = my_counters$id[1],
  date.from = Sys.Date() - 7,
  date.to = Sys.Date() - 1,
  fields = "ym:s:date,ym:s:startURL,ym:s:visitDuration"
)
```

## Advanced Usage

### Parallel Processing

```r
# Enable parallel processing for large datasets
rym_parallel_setup(strategy = "multisession", workers = 4)

# Retrieve data with automatic pagination
large_dataset <- reporting$get_data_paginated(
  counters = my_counters$id,
  date_from = "90daysAgo",
  date_to = "yesterday",
  progress = TRUE
)
```

### Secure Credential Management

```r
# Set up secure credential storage
rym_set_credentials(
  client_id = "your_client_id",
  client_secret = "your_client_secret", 
  user = "your_default_user",
  token_path = "~/.rym_tokens"
)

# Use keyring for maximum security
token <- rym_auth_modern(
  login = "your_login",
  use_keyring = TRUE
)
```

### Google Analytics Compatible API

```r
# Use GA-style queries
ga_client <- RymGAAPI$new(token = token)

ga_data <- ga_client$get_ga_data(
  counter = paste0("ga:", my_counters$id[1]),
  dimensions = "ga:date,ga:source",
  metrics = "ga:sessions,ga:users",
  start_date = "30daysAgo",
  end_date = "yesterday"
)
```

## Supported APIs

| API | Modern Client | Legacy Functions | Description |
|-----|---------------|------------------|-------------|
| **Management API** | `RymManagementAPI` | `rym_get_counters()`, `rym_get_goals()`, etc. | Counter management, goals, segments, filters |
| **Reporting API** | `RymReportingAPI` | `rym_get_data()` | Aggregated analytics data |
| **Logs API** | `RymLogsAPI` | `rym_get_logs()` | Raw, non-aggregated visit and hit data |
| **GA Compatible** | `RymGAAPI` | `rym_get_ga()` | Google Analytics Core API v3 compatible interface |

## Authentication

The package supports multiple authentication methods:

### Modern Authentication (Recommended)

- Automatic token refresh
- Secure keyring storage
- Enhanced error handling
- Better security practices

### Legacy Authentication

- File-based token storage
- Maintains backward compatibility
- Still functional but deprecated

## Error Handling

The modern API provides comprehensive error handling:

```r
tryCatch({
  data <- reporting$get_data(
    counters = "invalid_counter",
    metrics = "ym:s:visits"
  )
}, rym_api_error = function(e) {
  cat("API Error:", e$message, "\n")
}, rym_request_error = function(e) {
  cat("Request Error:", e$message, "\n")
})
```

## Data Validation and Utilities

```r
# Validate inputs
counter_id <- rym_validate_counter("12345")
date_from <- rym_validate_date("2024-01-01")

# Check package setup
rym_check_dependencies()
rym_get_credentials()

# Summarize data
summary_stats <- rym_summarize_data(report_data)
```

## Migration Guide

### From Legacy to Modern API

1. **Replace authentication:**
   ```r
   # Old
   rym_auth(login = "user")
   
   # New
   token <- rym_auth_modern(login = "user")
   ```

2. **Use client classes:**
   ```r
   # Old
   rym_get_counters()
   
   # New
   management <- RymManagementAPI$new(token = token)
   management$get_counters()
   ```

3. **Enjoy enhanced features:**
   - Better error messages
   - Progress tracking
   - Parallel processing
   - Automatic pagination

## Performance Tips

1. **Use parallel processing** for large datasets
2. **Enable progress tracking** for long-running operations
3. **Use CSV endpoints** for better performance (automatically handled)
4. **Implement pagination** for very large result sets
5. **Cache authentication tokens** securely

## Configuration

Set up package defaults for smoother workflow:

```r
# Configure package options
options(
  rym.user = "your_default_user",
  rym.token_path = "~/.rym_tokens",
  rym.show_deprecation_warnings = TRUE
)

# Or use the helper function
rym_set_credentials(
  user = "your_default_user",
  token_path = "~/.rym_tokens"
)
```

## Documentation

- **Package Website**: https://selesnow.github.io/rym/
- **Vignettes**: Use `vignette("intro-to-rym", package = "rym")`
- **API Reference**: https://yandex.ru/dev/metrika/doc/api2/concept/about-docpage

### Available Vignettes

- `vignette("intro-to-rym")` - Getting started guide
- `vignette("rym-modern-api")` - Modern API usage
- `vignette("rym-management-api")` - Management API details
- `vignette("rym-reporting-api")` - Reporting API guide
- `vignette("rym-logs-api")` - Logs API usage
- `vignette("rym-ga-api")` - Google Analytics compatibility

## Support and Contributing

- **Bug Reports**: https://github.com/selesnow/rym/issues
- **Questions**: https://t.me/R4marketing
- **Documentation**: https://selesnow.github.io/rym/

## Acknowledgments

- **Original Author**: Alexey Seleznev (Head of Analytics Dept. at Netpeak)
- **Contributors**: Community contributors and maintainers
- **Special Thanks**: Yandex Metrica team for comprehensive API documentation

## License

GPL-2

---

**Note**: This package maintains the legacy hardcoded credentials for backward compatibility. For production use, consider setting up your own Yandex OAuth application.
