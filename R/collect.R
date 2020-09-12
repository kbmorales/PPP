#' Collect PPP data from DataKind Google Drive
#'
#' @param version Either 1 for most recent version or 2 for oldest data. If not provided will ask
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
  cat('It will download over 100 MB of files.\n')
  cat('Once unzipped, over 600 MB of disk space will be used.\n')
  cat('Any existing files will be overwritten.\n')

  proceed = menu(c("Yes", "Cancel"),
                 title = "Proceed?")

  if (proceed!=1) stop("Canceling download.")

  url_dl=ifelse(
    version==1,
    # Public URL for 0808 data:
    # https://sba.app.box.com/s/ahn2exwfebgqruk714v3hnf75qdap3du
    'https://sba.app.box.com/index.php?rm=box_download_shared_file&shared_name=ahn2exwfebgqruk714v3hnf75qdap3du&file_id=f_710681792224',
    # Public URL for original release:
    # https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp
    'https://sba.app.box.com/index.php?rm=box_download_shared_file&shared_name=tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp&file_id=f_687644633952'
  )

  temp=tempfile(fileext = ".zip")
  download.file(url_dl,
                temp,
                mode="wb")
  cat('Download complete.\n')
  cat('Unzipping...\n')
  make_dir("data")
  unzip(temp,
        exdir = here::here("data"),
        overwrite = T)
  unlink(temp)
  cat('Unzip complete.\n')

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
