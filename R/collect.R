#' Collect PPP data from DataKind Google Drive
#'
#' @param version Either 1 for most recent version or 2 for oldest data. If not provided will ask
#'
#' @export
#'
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

  cat(paste0("This will save data to ~/data-raw/All Data ",
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
  make_dir("data-raw")
  unzip(temp,
        exdir = here::here("data-raw"),
        overwrite = T)
  unlink(temp)
  cat('Unzip complete.\n')
}

#' Downloads raw NAICS legends from US Census website
#'
#'
naics_collect=function(){
  # Download from
  # https://www.census.gov/eos/www/naics/downloadables/downloadables.html

  naics_url_2017='https://www.census.gov/eos/www/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
  naics_url_2012='https://www.census.gov/eos/www/naics/2012NAICS/2-digit_2012_Codes.xls'
  naics_url_2007='https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07.xls'
  naics_url_2002='https://www.census.gov/eos/www/naics/reference_files_tools/2002/naics_2_6_02.txt'

  naics_urls=c(naics_url_2017,
               naics_url_2012,
               naics_url_2007,
               naics_url_2002)

  # Helper dataframe
  naics_helper=data.frame(naics_urls,
                          year=c(2017,2012,2007,2002)) %>%
    dplyr::mutate(ext=stringr::str_extract(naics_urls,
                                           "\\.\\w+$"))


  cat('Downloading NAICS index files from US Census...\n')
  make_dir("data-raw/naics")
  for(i in seq_along(naics_helper$naics_urls)){
  download.file(naics_urls[i],
                here::here("data-raw/naics",
                           paste0("naics_",
                                  naics_helper$year[i],
                                  naics_helper$ext[i]
                                  )
                           ),
                mode="wb"
                )
  }
  cat('NAICS index files downloaded.\n')
}
