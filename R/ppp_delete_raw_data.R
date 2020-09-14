#' Remove all raw PPP data files
#'
#' Deletes all files created by \code{ppp_collect()} and  \code{naics_collect()}
#'
#' @export
#'
ppp_delete_raw_data=function(){
  cat('This will remove all PPP-related raw data files from your system.\n')
  proceed = menu(c("Yes",
                   "Cancel"),
                 title = "Proceed?")
  if (proceed!=1) stop("Canceling raw data removal.")

  unlink(paste0(here::here("data-raw")),
         recursive = T)

  cat('Raw PPP data files deleted from your file system.\n')
}
