# rym 2.0.0

## Overview
This release is a complete overhaul of the `rym` package, offering modern design and extensive functionality enhancements while maintaining backward compatibility with existing scripts.

### New Features
- **Modern R Package Structure:** Entire package rewritten using modern R best practices including R6 classes, enhanced modularity, and comprehensive documentation.
- **Enhanced Authentication:** Introduced `rym_auth_modern()` for secure authentication handling with automatic token refresh and optional usage of keyring for credential management.
- **API Client Classes:** Introduced R6-based clients such as `RymManagementAPI`, `RymReportingAPI`, `RymLogsAPI`, and `RymGAAPI` offering extensible interfaces and object-oriented design.
- **Comprehensive Error Handling:** Utilized `httr2` for advanced request and error management with detailed handling of API errors and rate-limiting.
- **Asynchronous and Parallel Processing:** Leveraged `future` and `progressr` for parallel computing and progress tracking to improve performance, especially for large data retrievals.
- **Google Analytics Compatibility:** The new `RymGAAPI` provides seamless integration with GA Core Reporting API v3 style queries.

### Improvements
- **Backward Compatibility:** All original function signatures are preserved. Deprecated functions are maintained but route through modern APIs, with warnings to transition to new methods.
- **Error Diagnostics:** Comprehensive alerts and messages guide users toward using the new functionality.
- **Data Handling:** Adapted to provide `data.frame` outputs with clear format adjustments for integration and analysis.
- **Validation:** Enhanced input validation ensures accurate request formation and execution.

### Deprecated
- Legacy function calls now point to the modern API internals.

### Known Issues
- Some uploading functions are yet to be ported to modern format; warnings are in place to inform users. Plans are to implement these in subsequent releases.

### Migration Guide
Developers can gradually transition to the new API by exploring the enhanced features of the modern clients and functions while retaining old calls throughout the process of update.

See the updated README and vignettes for detailed examples and migration steps.
