
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PPP

<!-- badges: start -->

<!-- badges: end -->

The goal of PPP is to download and assemble a unified, cleaned data set
of Paycheck Protection Program loans issued in 2020.

The data is too large to share on GitHub, but this package will allow
you to recreate the data locally.

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

There are two versions of the PPP data released. Notes for each are
below:

### 0808 Structure

Rows: 5,212,128 (0808 dataset)

### 0706 Structure

Rows: 4,885,388 (0706 dataset)

| variable      | n Missing | % Missing |                        Validation Notes |
| :------------ | --------: | --------: | --------------------------------------: |
| LoanRange     |   4224170 |      86.5 |                               see notes |
| BusinessName  |   4224171 |      86.5 |   no values for loan amounts under 150K |
| Address       |   4224170 |      86.5 |   no values for loan amounts under 150K |
| City          |         1 |       0.0 |                               see notes |
| State         |         0 |       0.0 |                               see notes |
| Zip           |       224 |       0.0 |                               see notes |
| NAICSCode     |    133527 |       2.7 |                      validation pending |
| BusinessType  |      4723 |       0.1 |                                         |
| RaceEthnicity |         0 |       0.0 |                      89.3% “Unanswered” |
| Gender        |         0 |       0.0 |                      77.7% “Unanswered” |
| Veteran       |         0 |       0.0 |                      84.7% “Unanswered” |
| NonProfit     |   4703708 |      96.3 |                               see notes |
| JobsRetained  |    324122 |       6.6 |                               see notes |
| DateApproved  |         0 |       0.0 | earliest: 2020-04-03 latest: 2020-06-30 |
| Lender        |         0 |       0.0 |                                         |
| CD            |         0 |       0.0 |                                         |
| LoanAmount    |    661218 |      13.5 |    no values for loan amounts over 150K |
