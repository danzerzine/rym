# rym 2.0.0

## Overview
This release is a complete overhaul of the `rym` package, offering modern design and extensive functionality enhancements while maintaining backward compatibility with existing scripts.

### New Features
- **Modern R Package Structure:** Entire package rewritten using modern R best practices including R6 classes, enhanced modularity, and comprehensive documentation.
- **Enhanced Authentication:** Introduced `rym_auth_modern()` for secure authentication handling with automatic token refresh and optional usage of keyring for credential management.
- **API Client Classes:** Introduced R6-based clients such as `RymManagementAPI`, `RymReportingAPI`, `RymLogsAPI`, `RymGAAPI`, and `RymUploadAPI` offering extensible interfaces and object-oriented design.
- **Complete Upload and Management Functionality:** All legacy upload and management functions now fully implemented:
  - **Expense Management:** `rym_upload_expense()`, `rym_delete_uploaded_expense()`, `rym_get_uploadings_expense()`
  - **Calls Management:** `rym_upload_calls()`, `rym_enable_calls()`, `rym_disable_calls()`, `rym_allow_calls()`, `rym_get_uploadings_calls()`
  - **Offline Conversions:** `rym_upload_offline_conversion()`, `rym_enable_offline_conversion()`, `rym_disable_offline_conversion()`, `rym_allow_offline_conversion()`, `rym_get_uploadings_offline_conversions()`
- **Modern Upload API Client:** New `RymUploadAPI` R6 class provides unified interface for all upload operations with improved error handling, automatic file cleanup, and progress indicators.
- **Comprehensive Error Handling:** Utilized `httr2` for advanced request and error management with detailed handling of API errors and rate-limiting.
- **Asynchronous and Parallel Processing:** Leveraged `future` and `progressr` for parallel computing and progress tracking to improve performance, especially for large data retrievals.
- **Google Analytics Compatibility:** The new `RymGAAPI` provides seamless integration with GA Core Reporting API v3 style queries.

### Improvements
- **100% Backward Compatibility:** All original function signatures are preserved and fully functional. All legacy upload and management functions now implemented with modern internals.
- **Enhanced Upload Mechanism:** Migrated from `httr` to `httr2` with multipart file uploads, automatic temporary file cleanup, and comprehensive error handling.
- **User Experience:** Added CLI progress indicators, user-friendly error messages, and deprecation warnings that guide users to modern alternatives.
- **Data Handling:** Consistent `data.frame` outputs with proper pagination handling for large result sets and safe NULL value handling.
- **Validation:** Enhanced input validation with proper parameter encoding and comprehensive API response checking.
- **Authentication Integration:** Seamless integration with modern authentication system supporting both interactive and non-interactive modes.

### Deprecated
- Legacy function calls now point to modern API internals with deprecation warnings encouraging migration to R6 classes for enhanced functionality.

### Migration Guide
Users have three migration phases:
1. **Continue using legacy functions** - All existing scripts work without changes
2. **Explore modern R6 classes** - `RymUploadAPI`, `RymManagementAPI`, etc. for enhanced features
3. **Adopt advanced functionality** - Async processing, better error handling, and progress tracking

#### Legacy to Modern Examples
```r
# Legacy (still works)
rym_upload_expense(counter = 12345, data = df)

# Modern equivalent
upload_client <- RymUploadAPI$new(token = token)
upload_client$upload_expense(counter_id = 12345, data = df)
```

See the updated README and vignettes for detailed examples and migration steps.
