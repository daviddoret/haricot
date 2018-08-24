##### LEGACY. MIGRATE EVERYTHING TO FUTILE.LOGGER #####

#' log
#'
#' @description ...
#'
#' @examples ...
#'
#' @param error_level LOG_TRACE, LOG_INFO, LOG_WARN, LOG_ERROR, LOG_FATAL
#' @param ... TBD
#' @return Nothing
#' @export
log <- function(
  error_level = NULL,
  ...){

  if(is.null(error_level)){
    error_level <- LOG_INFO;
  };

  args <- list(...);
  verbosity <- args[["verbosity"]];
  if(is.null(verbosity)) { verbosity <- 0; };
  print_log <- args[["print_log"]];
  if(is.null(print_log)) { print_log <- FALSE; };

  if(verbosity > 0){
    log_text <- paste0(paste0(names(args),": ",args), "; ", collapse = "");
    if(print_log){
      print(log_text);
    }
  }

}
