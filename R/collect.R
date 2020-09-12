#' Collect PPP data from DataKind Google Drive
#'
#' @param version either 1 for most revent version or 2 for oldest data. If not provided will ask
#'
#' @export
#'
#' @examples
ppp_collect = function(version=NULL) {

  # Public URL for 0808 data:
  # https://sba.app.box.com/s/ahn2exwfebgqruk714v3hnf75qdap3du

  # Public URL for original release:
  # https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp

  # Unable to download .zip file successfully from POST request
  # first_url='https://sba.app.box.com/index.php?rm=box_download_shared_file&shared_name=tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp&file_id=f_687644633952'
  # # Trying GET request
  # test_url='https://public.boxcloud.com/d/1/b1!mEu14bCHramNVL1yHiOlB930qvhuEAzb_1JqO_JPbnBcT-NDuur1ftAMrUkMMh-VTXLsjTP1l4FDT5U22ANS57Y6VJcpP3Prf0fmIskvwfbJHD3ml7Ea5mam1OWsbbfF5JbodvmUrXQWiL28uAXAyFCMTX4IrHN-67g5Blob2Sb-Csc5DiSDeORYQ40aMbLJCe1DKwvnEjVbTTPB6e9gTAgzUTfk-ON-xjqtM6GKO9z42OzFQSJT4s0RYubToXd_lQ3jT8gsfLO5_yrzMHy55uxFR0dtGdmBjJe1ClLumlCHWQGBgXnZ5T7PBO0Pc23PZTCtbAHhbjniL2jZDlkAa47J5t0rrBGr_K73crKDaNdgCYMtljo8qGQxpxZOjkcgnZTgf4TlQpbHTd6wto4nOvz0PcdbjW1Tr6C7fKIoDZgdrPUsWW3ioCDWmay0N68NrNrXtTY27XpqY6Tf3RN0vqIwTSU27p_f63Y4u6geEZMi5dO6gmZaY7tywDRTsLbR0uiGZDxOY5YzEEItPLQu9x6WcYH9qTdrrvcsae0WbrzkyrwUdjOWiMkDMbJnFCVc88bLjSCD_As5anD7ilXIa7iqHefAiWU7BsxT0yLqMojqIpAOUBDWYwECN3ZNsw4hgLNI67Twf-_tqHolma9nWcC08b3dINnTRpdsruPhWl9oz7r45lFWFWKRJK-7TqMCRHJxSk9E2ucNnm_QmqrbEgStz5HPDs5yXHOgMtNCrfWDwMCCJuJ5YmMOAwBROPQa5XJjdCQrxxlnpyV8kBffnDon7ptnK8ihPvtSmkjvQ54ZpJTcAj7xYo-aOo_OhU1D6Nw0ynymVBdOtla_FJM2FSW1nJuWUIOr9fwS8klnBBLOrrIGOSU5tFJFZcBhx5Vt4KTKhQh-PuHGwq_x3rXlN-ojTwPz9WUvxvrkvJnMszl0bzJV98Rkql2eTIfhW5wgUqBFmSZ8asPtlNFg9v692RRwHqZ0V5AknmRNNgBF8QVK2S8MPXiZDtLvXVxFdO2Dh-L2TEcJ2SXbKf5q_y2AQn-7Kb-LMLm_QdjyH5FRTmcms30tmjbzaHTvRzDcigJg83FfOIxQzdwwlcUmxUKo7ExIT-pjV4oQ4KN0sBO8aEPSeR4H71x8_j7TFvCXEGrRGPLum2uIMkksgxofiNTmins6HuMvIjT4w3qgA89of2evymFDgstFhpBkSSK6ij71H5mqTZ-Jj1EcyHF9bvvHFUZbSwQ02PLDG9SBClWHASbUummXrA8Vy7dkwdjbS_TFMetZjEKkN5QU/download'
  #
  # temp=tempfile(fileext = ".zip")
  # downloader::download(first_url,
  #                      temp)
  # make_dir("data")
  # unzip(here::here(temp),
  #       exdir = here::here("data"))
  # unlink(temp)

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
