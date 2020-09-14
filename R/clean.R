#' Read PPP loan data .CSV files into R
#'
#' @param version Either 1 for most recent version or 2 for oldest data. If not provided will ask
#'
#' @return A \code{tibble} of PPP loan data, ready for cleaning or analysis.
#' @export
#'
ppp_read=function(version=NULL) {
  # Ask version of data wanted
  if (is.null(version)) {
    version = menu(c("Most recent (2020-08-08)",
                     "First release (2020-07-06?)"),
                   title = "Which version of the PPP data would you like to ingest?")
  }

  if (version != 1 & version != 2) stop("Must specify version 1 (most recent) or 2 (oldest)")

  file_suffix = ifelse(version==1,
                       "0808",
                       "by State"
  )

  # set relative directory to search then scan through subdirectories for CSVs
  csv_dir <- here::here(paste("data-raw/All Data", file_suffix))
  cat(sprintf("Looking for data files in: %s\n", csv_dir))
  csv_files <- list.files(csv_dir, full.names = T, recursive = T, pattern = ".*.csv")

  # read in each CSV as character values, to allow for a clean import, attach the name of the data source file
  adbs <- purrr::map_df(csv_files,
                        ~readr::read_csv(.x,
                                  col_types = readr::cols(.default = "c")
                        ) %>%
                          dplyr::mutate(
                            source_file = stringr::str_remove_all(.x, ".*/")
                            )
  )

  # Add in version info
  adbs=adbs %>% dplyr::mutate(
    version=ifelse(version==1,
                   "2020-08-07",
                   "2020-07-06")
  )

  return(adbs)

}

#' Read in NAICS index files as single dataframe
#'
#' @return A tibble of NAICS codes
#' @export
#'
naics_read=function(){
  datapath="data-raw/naics"
  naics_files=list.files(here::here(datapath))

  # 2017
  for(i in 4:2){
    assign(
      paste0("naics",stringr::str_extract(naics_files[i],
                                          "\\d+")),
      suppressMessages(
        readxl::read_excel(here::here(datapath,
                                      naics_files[i]),
                           skip=2,
                           col_names = F) %>%
          dplyr::select(NAICSCode=2,
                        Industry=3) %>%
          dplyr::mutate(version=stringr::str_extract(naics_files[i],
                                                     "\\d+"))
      )
    )
  }

  # 2002 text file
  naics2002=suppressMessages(
    readr::read_tsv(here::here(datapath,
                               naics_files[1]),
                    skip=7,
                    col_names = F) %>%
      tidyr::separate(X1,
                      into=c("NAICSCode",
                             "Industry"),
                      sep='\\s+',
                      extra='merge') %>%
      dplyr::mutate(version="2002")
  )

  naics_df=do.call(rbind,
                   mget(rev(ls(pattern="naics\\d+")))
  )

  return(naics_df)

}

#' Clean NAICS dataframe for joining with PPP data
#'
#' @param naics_df The tibble returned by \code{naics_read()}
#'
#' @return A tidied tibble of NAICS codes by year release
#' @export
#'
naics_clean=function(naics_df){
  ## This will be master list
  naics_6L = naics %>%
    dplyr::filter(stringr::str_length(NAICSCode) == 6) %>%
    dplyr::rename(naics_lvl_5 = Industry) %>%
    ## Temp joining cols
    dplyr::mutate(naics_2 = str_trunc(NAICSCode, 2, ellipsis = ""),
                  naics_3 = str_trunc(NAICSCode, 3, ellipsis = ""),
                  naics_4 = str_trunc(NAICSCode, 4, ellipsis = ""),
                  naics_5 = str_trunc(NAICSCode, 5, ellipsis = ""))

  ## Higher level industry codes
  naics_2L = naics %>%
    dplyr::filter(
      stringr::str_length(NAICSCode) == 2 |
        stringr::str_detect(NAICSCode,"\\d{2}-\\d{2}")) %>% # Find ranges
    dplyr::rename(naics_2 = NAICSCode,
                  naics_lvl_1 = Industry)

  ### Handle ranges in naics_2L
  naics_2L_rng = naics_2L %>%
    dplyr::filter(
      stringr::str_detect(naics_2,"\\d{2}-\\d{2}"))

  naics_2L_fix = tibble::tibble(
    naics_2 = rep(
      as.character(c(seq(31,33),44,45,48,49)),
      4),
    naics_lvl_1 = rep(
      c(rep("Manufacturing", 3),
        rep("Retail Trade", 2),
        rep("Transportation and Warehousing", 2)),
      4),
    version=c(rep("2017",7),
              rep("2012",7),
              rep("2007",7),
              rep("2002",7)
              )
  )

  naics_2L = naics_2L %>%
    dplyr::filter(!stringr::str_detect(naics_2,
                                       "\\d{2}-\\d{2}")) %>%
    dplyr::bind_rows(naics_2L_fix) %>%
    dplyr::arrange(dplyr::desc(version), naics_2)

  naics_3L = naics %>%
    dplyr::filter(stringr::str_length(NAICSCode) == 3) %>%
    dplyr::rename(naics_3 = NAICSCode,
                  naics_lvl_2 = Industry)

  naics_4L = naics %>%
    dplyr::filter(stringr::str_length(NAICSCode) == 4) %>%
    dplyr::rename(naics_4 = NAICSCode,
                  naics_lvl_3 = Industry)

  naics_5L = naics %>%
    dplyr::filter(stringr::str_length(NAICSCode) == 5) %>%
    dplyr::rename(naics_5 = NAICSCode,
                  naics_lvl_4 = Industry)

  ## Join together and tidy
  naics_df = naics_6L %>%
    dplyr::left_join(naics_2L) %>%
    dplyr::left_join(naics_3L) %>%
    dplyr::left_join(naics_4L) %>%
    dplyr::left_join(naics_5L) %>%
    dplyr::select(NAICSCode,
                  naics_lvl_1,
                  naics_lvl_2,
                  naics_lvl_3,
                  naics_lvl_4,
                  naics_lvl_5,
                  version)
}

#' Applies recommended cleaning, see docs for details.
#'
#' @param df The PPP dataframe
#'
#' @return A cleaned \code{tibble} of PPP data.
#' @export
#'
ppp_clean=function(df){

  cat('Create unified loan amount / loan range cuts: LoanRange_Unified\n')
  adbs <- df %>%
    dplyr::mutate(LoanRange_Unified = dplyr::case_when(
      !is.na(LoanRange) ~ LoanRange,
      is.na(LoanRange) & as.numeric(LoanAmount) > 125000 & as.numeric(LoanAmount) <= 150000 ~ "f $125,000 - $150,000",
      is.na(LoanRange) & as.numeric(LoanAmount) > 100000 & as.numeric(LoanAmount) <= 125000 ~ "g $100,000 - $125,000",
      is.na(LoanRange) & as.numeric(LoanAmount) >  75000 & as.numeric(LoanAmount) <= 100000 ~ "h  $75,000 - $100,000",
      is.na(LoanRange) & as.numeric(LoanAmount) >  50000 & as.numeric(LoanAmount) <=  75000 ~ "i  $50,000 -  $75,000",
      is.na(LoanRange) & as.numeric(LoanAmount) >  25000 & as.numeric(LoanAmount) <=  50000 ~ "j  $25,000 -  $50,000",
      is.na(LoanRange) & as.numeric(LoanAmount) >   1000 & as.numeric(LoanAmount) <=  25000 ~ "k   $1,000 -  $25,000",
      is.na(LoanRange) & as.numeric(LoanAmount) >    100 & as.numeric(LoanAmount) <=   1000 ~ "l     $100 -    $1000",
      is.na(LoanRange) & as.numeric(LoanAmount) >     10 & as.numeric(LoanAmount) <=    100 ~ "m      $10 -     $100",
      is.na(LoanRange) & as.numeric(LoanAmount) >      0 & as.numeric(LoanAmount) <=     10 ~ "n           Up to $10",
      is.na(LoanRange) & as.numeric(LoanAmount) ==     0                                    ~ "o                Zero",
      is.na(LoanRange) & as.numeric(LoanAmount) <      0                                    ~ "p      Less than Zero",
      TRUE ~ "Unknown"))



  if (unique(df$version)=="2020-08-07") { # If latest version

    cat('Note: this version does not have JobsRetained data.\n')
    adbs$JobsRetained <- "Source Field No Longer Available as of 0808"
    adbs$JobsRetained_Grouped <- "Computed Field No Longer Available as of 0808"

    cat('Create unified jobs reported cuts: JobsReported_Grouped\n')
    adbs <- adbs %>%
      dplyr::mutate(JobsReported_Grouped = dplyr::case_when(
        as.numeric(JobsReported) > 400 & as.numeric(JobsReported) <= 500 ~ "a 400 - 500",
        as.numeric(JobsReported) > 300 & as.numeric(JobsReported) <= 400 ~ "b 300 - 400",
        as.numeric(JobsReported) > 200 & as.numeric(JobsReported) <= 300 ~ "c 200 - 300",
        as.numeric(JobsReported) > 100 & as.numeric(JobsReported) <= 200 ~ "d 100 - 200",
        as.numeric(JobsReported) >  50 & as.numeric(JobsReported) <= 100 ~ "e  50 - 100",
        as.numeric(JobsReported) >  25 & as.numeric(JobsReported) <=  50 ~ "f  25 -  50",
        as.numeric(JobsReported) >  10 & as.numeric(JobsReported) <=  25 ~ "g  10 -  25",
        as.numeric(JobsReported) >   5 & as.numeric(JobsReported) <=  10 ~ "h   5 -  10",
        as.numeric(JobsReported) >   1 & as.numeric(JobsReported) <=   5 ~ "i   2 -   5",
        as.numeric(JobsReported) >   0 & as.numeric(JobsReported) <=   1 ~ "j         1",
        as.numeric(JobsReported) ==     0                                ~ "k      Zero",
        as.numeric(JobsReported) <      0                                ~ "l  Negative",
        is.na(JobsReported) ~ NA_character_,
        TRUE ~ "Unknown"))

  } else if (unique(df$version)=="2020-07-06") { # original version

    cat('Note: this version does not have JobsReported data.\n')
    adbs$JobsReported <- "Source field not available in this version"
    adbs$JobsReported_Grouped <- "Computed field not available in this version"
    # Create Jobs Retained cuts
    cat('Create unified jobs retrained cuts: JobsRetained_Grouped\n')
    adbs = adbs %>%
      dplyr::mutate(JobsRetained_Grouped = dplyr::case_when(
        as.numeric(JobsRetained) > 400 & as.numeric(JobsRetained) <= 500 ~ "a 400 - 500",
        as.numeric(JobsRetained) > 300 & as.numeric(JobsRetained) <= 400 ~ "b 300 - 400",
        as.numeric(JobsRetained) > 200 & as.numeric(JobsRetained) <= 300 ~ "c 200 - 300",
        as.numeric(JobsRetained) > 100 & as.numeric(JobsRetained) <= 200 ~ "d 100 - 200",
        as.numeric(JobsRetained) >  50 & as.numeric(JobsRetained) <= 100 ~ "e  50 - 100",
        as.numeric(JobsRetained) >  25 & as.numeric(JobsRetained) <=  50 ~ "f  25 -  50",
        as.numeric(JobsRetained) >  10 & as.numeric(JobsRetained) <=  25 ~ "g  10 -  25",
        as.numeric(JobsRetained) >   5 & as.numeric(JobsRetained) <=  10 ~ "h   5 -  10",
        as.numeric(JobsRetained) >   1 & as.numeric(JobsRetained) <=   5 ~ "i   2 -   5",
        as.numeric(JobsRetained) >   0 & as.numeric(JobsRetained) <=   1 ~ "j         1",
        as.numeric(JobsRetained) ==     0                                ~ "k      Zero",
        as.numeric(JobsRetained) <      0                                ~ "l  Negative",
        is.na(JobsRetained) ~ NA_character_,
        TRUE ~ "Unknown")
        )
  }

  # Enhancements

  # Create valid Loan Values for all rows

  # Set numeric min, mid and max values for all loan ranges, keep exact values as-is
  cat('Creating minimum loan values: LoanRangeMin\n')
  adbs <- adbs %>%
    dplyr::mutate(LoanRangeMin = dplyr::case_when(
      !is.na(LoanAmount) ~ as.numeric(LoanAmount),
      is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ as.numeric( 5000000),
      is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ as.numeric( 2000000),
      is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ as.numeric( 1000000),
      is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ as.numeric(  350000),
      is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ as.numeric(  150000),
      TRUE ~ NA_real_))

  cat('Creating maximum loan values: LoanRangeMax\n')
  adbs <- adbs %>%
    dplyr::mutate(LoanRangeMax = dplyr::case_when(
      !is.na(LoanAmount) ~ as.numeric(LoanAmount),
      is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ as.numeric(10000000),
      is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ as.numeric( 5000000),
      is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ as.numeric( 2000000),
      is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ as.numeric( 1000000),
      is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ as.numeric(  350000),
      TRUE ~ NA_real_))

  cat('Creating midpoint loan values: LoanRangeMid\n')
  adbs <- adbs %>%
    dplyr::mutate(LoanRangeMid = dplyr::case_when(
      !is.na(LoanAmount) ~ as.numeric(LoanAmount),
      is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ as.numeric( 7500000),
      is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ as.numeric( 3500000),
      is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ as.numeric( 1500000),
      is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ as.numeric(  675000),
      is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ as.numeric(  250000),
      TRUE ~ NA_real_))

  # TODO Coercions

  return(adbs)

}
