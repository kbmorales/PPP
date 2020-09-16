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
          dplyr::mutate(NAICS_version=stringr::str_extract(naics_files[i],
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
      tidyr::separate(.data$X1,
                      into=c("NAICSCode",
                             "Industry"),
                      sep='\\s+',
                      extra='merge') %>%
      dplyr::mutate(NAICS_version="2002")
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
  naics_6L = naics_df %>%
    dplyr::filter(stringr::str_length(.data$NAICSCode) == 6) %>%
    dplyr::rename(naics_lvl_5 = .data$Industry) %>%
    ## Temp joining cols
    dplyr::mutate(naics_2 = stringr::str_trunc(.data$NAICSCode, 2, ellipsis = ""),
                  naics_3 = stringr::str_trunc(.data$NAICSCode, 3, ellipsis = ""),
                  naics_4 = stringr::str_trunc(.data$NAICSCode, 4, ellipsis = ""),
                  naics_5 = stringr::str_trunc(.data$NAICSCode, 5, ellipsis = ""))

  ## Higher level industry codes
  naics_2L = naics_df %>%
    dplyr::filter(
      stringr::str_length(.data$NAICSCode) == 2 |
        stringr::str_detect(.data$NAICSCode,"\\d{2}-\\d{2}")) %>% # Find ranges
    dplyr::rename(naics_2 = .data$NAICSCode,
                  naics_lvl_1 = .data$Industry)

  ### Handle ranges in naics_2L
  naics_2L_rng = naics_2L %>%
    dplyr::filter(
      stringr::str_detect(.data$naics_2,"\\d{2}-\\d{2}"))

  naics_2L_fix = tibble::tibble(
    naics_2 = rep(
      as.character(c(seq(31,33),44,45,48,49)),
      4),
    naics_lvl_1 = rep(
      c(rep("Manufacturing", 3),
        rep("Retail Trade", 2),
        rep("Transportation and Warehousing", 2)),
      4),
    NAICS_version=c(rep("2017",7),
              rep("2012",7),
              rep("2007",7),
              rep("2002",7)
              )
  )

  naics_2L = naics_2L %>%
    dplyr::filter(!stringr::str_detect(.data$naics_2,
                                       "\\d{2}-\\d{2}")) %>%
    dplyr::bind_rows(naics_2L_fix) %>%
    dplyr::arrange(dplyr::desc(.data$NAICS_version), .data$naics_2)

  naics_3L = naics_df %>%
    dplyr::filter(stringr::str_length(.data$NAICSCode) == 3) %>%
    dplyr::rename(naics_3 = .data$NAICSCode,
                  naics_lvl_2 = .data$Industry)

  naics_4L = naics_df %>%
    dplyr::filter(stringr::str_length(.data$NAICSCode) == 4) %>%
    dplyr::rename(naics_4 = .data$NAICSCode,
                  naics_lvl_3 = .data$Industry)

  naics_5L = naics_df %>%
    dplyr::filter(stringr::str_length(.data$NAICSCode) == 5) %>%
    dplyr::rename(naics_5 = .data$NAICSCode,
                  naics_lvl_4 = .data$Industry)

  ## Join together and tidy
  naics_clean = suppressMessages(
    naics_6L %>%
    dplyr::left_join(naics_2L) %>%
    dplyr::left_join(naics_3L) %>%
    dplyr::left_join(naics_4L) %>%
    dplyr::left_join(naics_5L) %>%
    dplyr::select(.data$NAICSCode,
                  .data$naics_lvl_1,
                  .data$naics_lvl_2,
                  .data$naics_lvl_3,
                  .data$naics_lvl_4,
                  .data$naics_lvl_5,
                  .data$NAICS_version)
  )

  return(naics_clean)

}

#' Perform hierarchical join of PPP loan data with NAICS legends
#'
#' This function will attempt to match PPP loan data with NAICS codes
#' hierarchically. I.e., it will first attempt to match with the latest NAICS
#' legend (2017), then the next most recent (2012), and so on. NAICS Codes
#' versions checked are those available for download on the US Census website:
#' 2017, 2012, 2007, and 2002.
#'
#' @param ppp_df The tibble of PPP loan data
#' @param naics_df The tibble resulting from \code{naics_clean()}
#'
#' @return A tibble of tidy PPP loan data with NAICS industry labels
#' @export
#'
naics_join=function(ppp_df, naics_df){
  # Some of the 6-digit NAICS codes in the PPP loan data don't match up to the
  # 2017 NAICS code

  naics2017 <- naics_df %>%
    dplyr::filter(.data$NAICS_version=="2017")
  naics2012 <- naics_df %>%
    dplyr::filter(.data$NAICS_version=="2012")
  naics2007 <- naics_df %>%
    dplyr::filter(.data$NAICS_version=="2007")
  naics2002 <- naics_df %>%
    dplyr::filter(.data$NAICS_version=="2002")

  # 2017 filter joins
  ppp_naics2017_good <- ppp_df %>%
    dplyr::inner_join(naics2017,
                      by = "NAICSCode")
  ppp_naics2017_fails <- ppp_df %>%
    # Filter to those that don't join with 2017 codes
    dplyr::anti_join(naics2017,
                     by = "NAICSCode")

  # 2012 filter joins
  ppp_naics2012_good <- ppp_naics2017_fails %>%
    dplyr::inner_join(naics2012,
                      by = "NAICSCode")
  ppp_naics2012_fails <- ppp_naics2017_fails %>%
    # Filter to those that don't join with 2017 codes
    dplyr::anti_join(naics2012,
                     by = "NAICSCode")

  # 2007 filter joins
  ppp_naics2007_good <- ppp_naics2012_fails %>%
    dplyr::inner_join(naics2007,
                      by = "NAICSCode")
  ppp_naics2007_fails <- ppp_naics2012_fails %>%
    # Filter to those that don't join with 2017 codes
    dplyr::anti_join(naics2007,
                     by = "NAICSCode")

  # 2002 filter joins
  ppp_naics2002_good <- ppp_naics2007_fails %>%
    dplyr::inner_join(naics2002,
                      by = "NAICSCode")
  ppp_naics2002_fails <- ppp_naics2007_fails %>%
    # Filter to those that don't join with 2017 codes
    dplyr::anti_join(naics2002,
                     by = "NAICSCode")

  # Take final fails and append NAs
  ppp_naics2002_fails <- ppp_naics2002_fails %>%
    dplyr::left_join(naics2017,
                     by = "NAICSCode") %>%
    dplyr::mutate(NAICS_valid=F)

  naics_good <- rbind(ppp_naics2017_good,
        ppp_naics2012_good,
        ppp_naics2007_good,
        ppp_naics2002_good
        ) %>%
    dplyr::mutate(NAICS_valid=T)

  final_df <- rbind(naics_good,ppp_naics2002_fails)

  return(final_df)

}

#' Applies recommended cleaning, see docs for details.
#'
#' @param df The PPP dataframe
#'
#' @return A cleaned \code{tibble} of PPP data.
#' @export
#'
ppp_clean=function(df){

  # Coercions
  cat('Coercing variables...\n')
  adbs <- suppressWarnings(
    df %>%
      dplyr::mutate(
        # Logicals
        NonProfit=dplyr::case_when(NonProfit=="Y"~T,
                                   NonProfit=="N"~F,
                                   TRUE ~ NA),
        # Dates
        DateApproved=as.Date(.data$DateApproved,
                             format="%m/%d/%Y"),
        # Numeric
        LoanAmount=as.numeric(.data$LoanAmount)
      )
    )

  if("JobsReported" %in% colnames(adbs)) {
    adbs <- suppressWarnings(
      adbs %>%
        dplyr::mutate(JobsReported=as.numeric(.data$JobsReported))
    )
  }

  if("JobsRetained" %in% colnames(adbs)) {
    adbs <- suppressWarnings(
      adbs %>%
        dplyr::mutate(JobsRetained=as.numeric(.data$JobsRetained))
    )
  }

  cat('Create unified loan amount / loan range cuts: LoanRange_Unified\n')
  adbs <- adbs %>%
    dplyr::mutate(LoanRange_Unified = dplyr::case_when(
      !is.na(LoanRange) ~ LoanRange,
      is.na(LoanRange) & LoanAmount > 125000 & LoanAmount <= 150000 ~ "f $125,000 - $150,000",
      is.na(LoanRange) & LoanAmount > 100000 & LoanAmount <= 125000 ~ "g $100,000 - $125,000",
      is.na(LoanRange) & LoanAmount >  75000 & LoanAmount <= 100000 ~ "h  $75,000 - $100,000",
      is.na(LoanRange) & LoanAmount >  50000 & LoanAmount <=  75000 ~ "i  $50,000 -  $75,000",
      is.na(LoanRange) & LoanAmount >  25000 & LoanAmount <=  50000 ~ "j  $25,000 -  $50,000",
      is.na(LoanRange) & LoanAmount >   1000 & LoanAmount <=  25000 ~ "k   $1,000 -  $25,000",
      is.na(LoanRange) & LoanAmount >    100 & LoanAmount <=   1000 ~ "l     $100 -    $1000",
      is.na(LoanRange) & LoanAmount >     10 & LoanAmount <=    100 ~ "m      $10 -     $100",
      is.na(LoanRange) & LoanAmount >      0 & LoanAmount <=     10 ~ "n           Up to $10",
      is.na(LoanRange) & LoanAmount ==     0                        ~ "o                Zero",
      is.na(LoanRange) & LoanAmount <      0                        ~ "p      Less than Zero",
      TRUE ~ "Unknown"))



  if (unique(df$version)=="2020-08-07") { # If latest version

    cat('Note: this version does not have JobsRetained data.\n')
    adbs$JobsRetained <- "Source Field No Longer Available as of 0808"
    adbs$JobsRetained_Grouped <- "Computed Field No Longer Available as of 0808"

    cat('Create unified jobs reported cuts: JobsReported_Grouped\n')
    adbs <- adbs %>%
      dplyr::mutate(JobsReported_Grouped = dplyr::case_when(
        JobsReported > 400 & JobsReported <= 500 ~ "a 400 - 500",
        JobsReported > 300 & JobsReported <= 400 ~ "b 300 - 400",
        JobsReported > 200 & JobsReported <= 300 ~ "c 200 - 300",
        JobsReported > 100 & JobsReported <= 200 ~ "d 100 - 200",
        JobsReported >  50 & JobsReported <= 100 ~ "e  50 - 100",
        JobsReported >  25 & JobsReported <=  50 ~ "f  25 -  50",
        JobsReported >  10 & JobsReported <=  25 ~ "g  10 -  25",
        JobsReported >   5 & JobsReported <=  10 ~ "h   5 -  10",
        JobsReported >   1 & JobsReported <=   5 ~ "i   2 -   5",
        JobsReported >   0 & JobsReported <=   1 ~ "j         1",
        JobsReported ==  0                       ~ "k      Zero",
        JobsReported <   0                       ~ "l  Negative",
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
        JobsRetained > 400 & JobsRetained <= 500 ~ "a 400 - 500",
        JobsRetained > 300 & JobsRetained <= 400 ~ "b 300 - 400",
        JobsRetained > 200 & JobsRetained <= 300 ~ "c 200 - 300",
        JobsRetained > 100 & JobsRetained <= 200 ~ "d 100 - 200",
        JobsRetained >  50 & JobsRetained <= 100 ~ "e  50 - 100",
        JobsRetained >  25 & JobsRetained <=  50 ~ "f  25 -  50",
        JobsRetained >  10 & JobsRetained <=  25 ~ "g  10 -  25",
        JobsRetained >   5 & JobsRetained <=  10 ~ "h   5 -  10",
        JobsRetained >   1 & JobsRetained <=   5 ~ "i   2 -   5",
        JobsRetained >   0 & JobsRetained <=   1 ~ "j         1",
        JobsRetained ==  0                       ~ "k      Zero",
        JobsRetained <   0                       ~ "l  Negative",
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
      !is.na(LoanAmount) ~ LoanAmount,
      is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ 5000000,
      is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ 2000000,
      is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ 1000000,
      is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ 350000,
      is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ 150000,
      TRUE ~ NA_real_))

  cat('Creating maximum loan values: LoanRangeMax\n')
  adbs <- adbs %>%
    dplyr::mutate(LoanRangeMax = dplyr::case_when(
      !is.na(LoanAmount) ~ LoanAmount,
      is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ 10000000,
      is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ 5000000,
      is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ 2000000,
      is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ 1000000,
      is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ 350000,
      TRUE ~ NA_real_))

  cat('Creating midpoint loan values: LoanRangeMid\n')
  adbs <- adbs %>%
    dplyr::mutate(LoanRangeMid = dplyr::case_when(
      !is.na(LoanAmount) ~ LoanAmount,
      is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ 7500000,
      is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ 3500000,
      is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ 1500000,
      is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ 675000,
      is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ 250000,
      TRUE ~ NA_real_))

  return(adbs)

}
