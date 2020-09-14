
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PPP

<!-- badges: start -->

<!-- badges: end -->

The goal of PPP is to download and assemble a unified, cleaned data set
of Paycheck Protection Program loans issued in 2020.

The data is too large to share on GitHub, but this package will allow
you to recreate the data locally.

The work for this package grew out of a project with [DataKind’s DC
chapter](https://github.com/DataKind-DC/CARES) and the [National Press
Foundation](https://nationalpress.org/).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kbmorales/PPP")
```

## Assembling PPP data

``` r
# Note: this function will download over 600MB of data to your local machine,
# and read a large dataframe into memory!
ppp_data = ppp_assemble()
```

## Data Sources

  - Small Business Administration PPP Loan Data:
      - [Latest
        release](https://sba.app.box.com/s/ahn2exwfebgqruk714v3hnf75qdap3du)
      - [Original
        release](https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp)
  - NAICS code dictionaries [US Census NAICS
    Files](https://www.census.gov/eos/www/naics/downloadables/downloadables.html)

## PPP Data Dictionary

Though there are two versions of the PPP data released, both will have
the same final structure as detailed below. The main structual
difference between these versions is the removal of a `JobsRetained`
variable, and the addition of `JobsReported`. Whether or not this is a
simple semantic change is unknown at this point.

The final PPP dataset output by `ppp_assemble()` will contain the
following columns:

| variable              | original |                                 notes |
| :-------------------- | -------: | ------------------------------------: |
| LoanRange             |      Yes | no values for loan amounts under 150K |
| BusinessName          |      Yes | no values for loan amounts under 150K |
| Address               |      Yes | no values for loan amounts under 150K |
| City                  |      Yes |                                       |
| State                 |      Yes |                                       |
| Zip                   |      Yes |                                       |
| NAICSCode             |      Yes |                                       |
| BusinessType          |      Yes |                                       |
| RaceEthnicity         |      Yes |                    89.3% “Unanswered” |
| Gender                |      Yes |                    77.7% “Unanswered” |
| Veteran               |      Yes |                    84.7% “Unanswered” |
| NonProfit             |      Yes |                                       |
| JobsRetained          |      Yes |            only in 2020-07-06 release |
| JobsReported          |      Yes |            only in 2020-08-08 release |
| DateApproved          |      Yes |                                       |
| Lender                |      Yes |                                       |
| CD                    |      Yes |                                       |
| LoanAmount            |      Yes |  no values for loan amounts over 150K |
| source\_file          |       No |   file name for where data was pulled |
| version               |       No |              release date of PPP data |
| LoanRange\_Unified    |       No |      Loan ranges regardless of amount |
| JobsRetained\_Grouped |       No |    brackets for \# of ‘jobs retained’ |
| JobsReported\_Grouped |       No |    brackets for \# of ‘jobs reported’ |
| LoanRangeMin          |       No |           minimum possible loan value |
| LoanRangeMax          |       No |           maximum possible loan value |
| LoanRangeMid          |       No |           middle estimated loan value |
| naics\_lvl\_1         |       No |     Most general NAICS industry class |
| naics\_lvl\_2         |       No |     Second level NAICS industry class |
| naics\_lvl\_3         |       No |      Third level NAICS industry class |
| naics\_lvl\_4         |       No |      Forth level NAICS industry class |
| naics\_lvl\_5         |       No |    Most specific NAICS industry class |
| NAICS\_version        |       No | version of NAICS where code was found |
| NAICS\_valid          |       No |               was NAICS code matched? |
