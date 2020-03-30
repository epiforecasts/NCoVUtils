#' Reset Cache and Update all Local Data
#' @param refresh_data Logical defaults to `FALSE`. Should all data sources be refreshed once the cache has been
#' removed.
#' @return Null
#' @export
#' @importFrom memoise cache_filesystem
#' @examples
#'
#'## Code
#'reset_cache
reset_cache <- function() {

  unlink(".cache", recursive = TRUE)

  return(invisible(NULL))
}
