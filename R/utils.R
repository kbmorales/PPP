
make_dir=function(datapath){
  if (!dir.exists(datapath)) {
    cat("Creating path", paste0("./",datapath,"/\n"))
    suppressWarnings(
      dir.create(here::here(datapath), recursive = T)
    )
  }
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
