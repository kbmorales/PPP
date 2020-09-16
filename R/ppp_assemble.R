#' Assemble a tidied PPP data set for analysis
#'
#' This function will download, clean and return a large dataframe in memory of
#' PPP data.
#'
#' @param version Either 1 for most recent version or 2 for oldest data. If not provided will ask
#'
#' @return A tidy tibble of PPP loan data (~5.2M rows)
#' @export
#'
ppp_assemble=function(version=NULL) {
  cat("This function will assemble a finalized PPP dataset.\n")
  if (is.null(version)) {
    version = menu(c("Most recent (2020-08-08)",
                     "First release (2020-07-06?)"),
                   title = "Which version of the PPP data would you like to ingest?")
  }

  # Download

  # Check if PPP data exists
  if(version==1 &
     length(list.files(here::here("data-raw", "All Data 0808"))) > 0 |
     version==2 &
     length(list.files(here::here("data-raw", "All Data by State"))) > 0 ) {
    cat("Found raw PPP data files.\n")
    redownload = menu(c("Use previously downloaded data",
                        "Redownload the data"),
                      title = "Would you like to use those files?")
    if (redownload==2) {
      ppp_collect(version)
    }
  } else {
    ppp_collect(version)
  }

  # Check if NAICS data exists
  if(length(list.files(here::here("data-raw", "naics"))) > 0) {
    cat("Found raw NAICS data files.\n")
    redownload = menu(c("Use previously downloaded data",
                        "Redownload the data"),
                      title = "Would you like to use those files?")
    if (redownload==2) {
      naics_collect()
    }
  } else {
    naics_collect()
  }

  # Read

  naics_df=naics_read() %>% naics_clean()

  ppp_df=ppp_read(version) %>% ppp_clean()

  # Join

  ppp_final=naics_join(ppp_df, naics_df)

  cat("Final PPP data set assembled.\n")

  remove_raw = menu(c("Yes",
                      "No"),
                    title = "Would you like to delete the downloaded raw PPP data?")

  if (remove_raw==1) ppp_delete_raw_data()

  return(ppp_final)

}
