#' Collect PPP data from DataKind Google Drive
#'
#' @param version either 1 for most revent version or 2 for oldest data. If not provided will ask
#'
#' @export
#'
#' @examples
ppp_collect = function(version=NULL) {

  # Ask version of data wanted
  if (is.null(version)) {
    version = menu(c("Most recent (2020-08-08)",
                     "First release (2020-07-06?)"),
                   title = "Which version of the PPP data would you like to download?")
  }

  if (version != 1 & version != 2) stop("Must specify version 1 (most recent) or 2 (oldest)")

  file_suffix = ifelse(version==1,
                       "0808",
                       "by State"
  )

  cat(paste0("This will save data to ~/data/All Data ",
             file_suffix,
             "\n"))
  cat('It will download over 600 MB of files.\n')
  cat('Any existing files will be overwritten.\n')

  proceed = menu(c("Yes", "Cancel"),
                 title = "Proceed?")

  if (proceed!=1) stop("Canceling download.")

  datapath=paste('data/All Data',file_suffix)
  if (!dir.exists(datapath)) dir.create(datapath, recursive = T)

  # Pull list of PPP .csv filename
  files_ls <- drive_ls(
    paste("National Press Foundation - DataKind Volunteers/Data/All Data",
          file_suffix),
    recursive = TRUE
  )

  csv_ls <- files_ls[str_ends(files_ls$name, ".csv"),]

  # Download files
  for (i in 1:nrow(csv_ls)) {
    drive_download(csv_ls[i,],
                   path = file.path(datapath,
                                    csv_ls[i, "name"]),
                   overwrite = TRUE)
  }

}

#' Download NAICS code
#'
#' Downloads final NAICS file from DataKind Google Drive ready for joining
#' with PPP data
#'
#' @return
#' @export
#'
#' @examples
naics_collect = function(){

  cat("This will save data to ~/data/tidy_data\n")
  # cat('It will download over 600 MB of files.\n')
  cat('Any existing files will be overwritten.\n')

  proceed = menu(c("Yes", "Cancel"),
                 title = "Proceed?")

  if (proceed!=1) stop("Canceling download.")

  datapath = 'data/tidy_data'
  if (!dir.exists(datapath)) dir.create(datapath,recursive = T)

  # Search for shared folder -- takes a minute
  drive_download(drive_get("adbs_naics.csv"),
                 path = file.path("data",
                                  "tidy_data",
                                  "naics_clean.csv"),
                 overwrite = T)
}
