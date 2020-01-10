
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minihealth

<!-- badges: start -->

<!-- badges: end -->

The `minihealth` package is a central component of
[JAMES](https://github.com/stefvanbuuren/james). The package

  - Defines S4 classes `xyz`, `bse`, `individual` and `cabinet` for
    analysing individual growth data;
  - Translates `individual`, `donordata` and `bds` formats into each
    other;
  - Extracts data and ranges from `individual` objects.

## Installation

You can install the development version `minihealth` by

``` r
install.packages("remotes")
remotes::install_github("stefvanbuuren/minihealth")
```

There is no release on CRAN.

## Example 1: Automatic Z-score calculation

The S4 class `xyz` stores three variables useful for anthropometric
data:

  - `x`: usually age, but can also be height (in weight-for-height);
  - `y`: measurement, e.g. height or weight;
  - `z`: Z-score of `y` conditional on `x`.

Here are some examples for automatic \(Z\)-score calculation:

``` r
library(minihealth)
#> Registered S3 method overwritten by 'pryr':
#>   method      from
#>   print.bytes Rcpp

# specify length (in cm) for boy at ages 0, 0.2 and 0.5 years
new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4))
#> No reference
#>   age  hgt hgt.z
#> 1 0.0 51.0    NA
#> 2 0.2 54.1    NA
#> 3 0.5 63.4    NA

# at the minimum, specify sex for automatic Z-score calculation
new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "male")
#> package: clopus, library: clopus::nl1997 , member: nl1997.mhgtNL 
#>   age  hgt  hgt.z
#> 1 0.0 51.0 -0.154
#> 2 0.2 54.1 -2.312
#> 3 0.5 63.4 -1.829

# specify weight (in kg) at same ages
d1 <- new("xyz", x = c(0, 0.2, 0.5), y = c(3.2, 5.2, 7.0), 
          sex = "male", yname = "wgt")
d1
#> package: clopus, library: clopus::nl1997 , member: nl1997.mwgtNL 
#>   age wgt  wgt.z
#> 1 0.0 3.2 -0.874
#> 2 0.2 5.2 -0.575
#> 3 0.5 7.0 -1.048

# Obtain reference table used to calculate Z-scores in d3
eval(d1@call)
#> reference -- country: NL   year: 1997   sex: male   yvar: wgt   dist: LMS 
#>          x      L     M     S
#> 1   0.0000  0.629  3.55 0.115
#> 2   0.0192  0.617  3.75 0.115
#> 3   0.0383  0.604  3.94 0.115
#> 4   0.0575  0.592  4.13 0.115
#> 5   0.0767  0.580  4.33 0.115
#> 6   0.0958  0.569  4.53 0.115
#> 7   0.1150  0.557  4.72 0.114
#> 8   0.1342  0.546  4.92 0.114
#> 9   0.1533  0.534  5.11 0.114
#> 10  0.1725  0.523  5.30 0.114
#> 11  0.1916  0.512  5.48 0.114
#> 12  0.2300  0.490  5.84 0.114
#> 13  0.2683  0.468  6.19 0.114
#> 14  0.3066  0.448  6.51 0.113
#> 15  0.3450  0.428  6.82 0.113
#> 16  0.3833  0.409  7.11 0.113
#> 17  0.4216  0.391  7.39 0.113
#> 18  0.4600  0.373  7.65 0.113
#> 19  0.4983  0.357  7.89 0.113
#> 20  0.5366  0.341  8.12 0.113
#> 21  0.6133  0.311  8.55 0.112
#> 22  0.6899  0.285  8.95 0.112
#> 23  0.7666  0.260  9.30 0.112
#> 24  0.8433  0.237  9.63 0.112
#> 25  0.9199  0.216  9.94 0.112
#> 26  0.9966  0.196 10.23 0.112
#> 27  1.0732  0.177 10.50 0.113
#> 28  1.1499  0.160 10.75 0.113
#> 29  1.2266  0.143 11.00 0.113
#> 30  1.5000  0.092 11.78 0.113
#> 31  2.0000  0.014 13.02 0.114
#> 32  2.5000 -0.053 14.12 0.116
#> 33  3.0000 -0.116 15.19 0.117
#> 34  3.5000 -0.177 16.28 0.119
#> 35  4.0000 -0.239 17.42 0.122
#> 36  4.5000 -0.300 18.60 0.124
#> 37  5.0000 -0.359 19.82 0.127
#> 38  5.5000 -0.416 21.08 0.130
#> 39  6.0000 -0.469 22.37 0.134
#> 40  6.5000 -0.518 23.68 0.137
#> 41  7.0000 -0.560 25.03 0.141
#> 42  7.5000 -0.597 26.43 0.144
#> 43  8.0000 -0.627 27.86 0.148
#> 44  8.5000 -0.648 29.30 0.152
#> 45  9.0000 -0.659 30.76 0.155
#> 46  9.5000 -0.661 32.25 0.159
#> 47 10.0000 -0.651 33.79 0.162
#> 48 10.5000 -0.629 35.43 0.165
#> 49 11.0000 -0.591 37.23 0.169
#> 50 11.5000 -0.536 39.24 0.172
#> 51 12.0000 -0.464 41.48 0.175
#> 52 12.5000 -0.383 43.99 0.177
#> 53 13.0000 -0.302 46.77 0.179
#> 54 13.5000 -0.232 49.77 0.178
#> 55 14.0000 -0.180 52.87 0.176
#> 56 14.5000 -0.150 55.94 0.172
#> 57 15.0000 -0.138 58.82 0.167
#> 58 15.5000 -0.139 61.45 0.161
#> 59 16.0000 -0.148 63.77 0.156
#> 60 16.5000 -0.161 65.75 0.151
#> 61 17.0000 -0.174 67.43 0.147
#> 62 17.5000 -0.187 68.85 0.143
#> 63 18.0000 -0.199 70.06 0.140
#> 64 18.5000 -0.208 71.10 0.137
#> 65 19.0000 -0.217 72.04 0.135
#> 66 19.5000 -0.225 72.89 0.133
#> 67 20.0000 -0.232 73.70 0.130
#> 68 20.5000 -0.240 74.49 0.128
#> 69 21.0000 -0.247 75.28 0.126
```

It is also possible to perform the reverse calculation, where `z` is
given and `y` is calculated.

``` r
# Standard weight centiles at age 0.5 year of Dutch boys
new("xyz", x = rep(0.5, 5), z = -2:2, sex = "male", yname = "wgt")
#> package: clopus, library: clopus::nl1997 , member: nl1997.mwgtNL 
#>   age  wgt wgt.z
#> 1 0.5 6.24    -2
#> 2 0.5 7.04    -1
#> 3 0.5 7.90     0
#> 4 0.5 8.83     1
#> 5 0.5 9.82     2

# Extend to grid of ages: 0y, 0.5y and 1y
new("xyz", x = rep(c(0, 0.5, 1), each = 5), z = rep(-2:2, 3), 
    sex = "male", yname = "wgt")
#> package: clopus, library: clopus::nl1997 , member: nl1997.mwgtNL 
#>    age   wgt wgt.z
#> 1  0.0  2.77    -2
#> 2  0.0  3.15    -1
#> 3  0.0  3.55     0
#> 4  0.0  3.97     1
#> 5  0.0  4.40     2
#> 6  0.5  6.24    -2
#> 7  0.5  7.04    -1
#> 8  0.5  7.90     0
#> 9  0.5  8.83     1
#> 10 0.5  9.82     2
#> 11 1.0  8.14    -2
#> 12 1.0  9.15    -1
#> 13 1.0 10.24     0
#> 14 1.0 11.44     1
#> 15 1.0 12.75     2
```

See the help (“?`xyz-class`”) and the
[clopus](https://github.com/stefvanbuuren/clopus) package for more
examples.

## Example 2: Automatic brokenstick estimation

``` r
# specify three height measures
boy <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), 
           sex = "male")
boy
#> package: clopus, library: clopus::nl1997 , member: nl1997.mhgtNL 
#>   age  hgt  hgt.z
#> 1 0.0 51.0 -0.154
#> 2 0.2 54.1 -2.312
#> 3 0.5 63.4 -1.829

# calculate broken stick estimates at observed ages
new("bse", data = boy)
#> package: donordata, model: load_data(dnr = "smocc_bs", element = "hgt") , member: smocc_bs 
#>   age  hgt  hgt.z
#> 1 0.0 50.6 -0.367
#> 2 0.2 54.8 -2.001
#> 3 0.5 63.5 -1.788

# calculate broken stick estimates at all break points
new("bse", data = boy, at = "knots")
#> package: donordata, model: load_data(dnr = "smocc_bs", element = "hgt") , member: smocc_bs 
#>       age  hgt  hgt.z
#> 1  0.0000 50.6 -0.367
#> 2  0.0767 50.8 -1.678
#> 3  0.1533 53.2 -1.964
#> 4  0.2500 56.4 -2.040
#> 5  0.3333 60.1 -1.549
#> 6  0.5000 63.5 -1.788
#> 7  0.6250 66.7 -1.478
#> 8  0.7500 68.2 -1.679
#> 9  0.9167 70.8 -1.653
#> 10 1.1667 74.5 -1.532
#> 11 1.5000 78.6 -1.519
#> 12 2.0000 84.3 -1.410
#> 13 3.0000 92.8 -1.444
```

See the help (“?`bse-class`”) and the
[brokenstick](https://github.com/stefvanbuuren/brokenstick) package for
more examples.

## Example 3: Bundle all measures by individual

The S4 class `individual` bundles four different types of information,
each of which is coded by its own class:

  - `individualID`: ID information, like name, date of birth;
  - `individualBG`: Background, like sex, gestional age or etnicity;
  - `individualAN`: Bundles person’s anthrometric data, like `hgt` and
    `wgt`;
  - `individualBS`: Bundles person’s brokenstick estimates.

Creating an instance of class `individual` can be done by hand in two
steps. First, create one or more of the four subclasses, and then  
tie these together, as follows:

``` r
pid <- new("individualID", name = c("Rob", "Dorchester"),
           dob = as.Date("2014-08-22", "%Y-%m-%d"), 
           id = as.integer(204))
pbg <- new("individualBG", sex = "male", hgtf = 185)
pan <- new("individualAN",
           hgt = new("xyz", 
                     x = c(0, 0.2, 0.5), 
                     y = c(51.0, 54.1, 63.4), 
                     sex = pbg@sex),
           wgt = new("xyz", 
                     x = c(0, 0.5), 
                     y = c(3.2, 7.0), 
                     yname = "wgt", 
                     sex = pbg@sex))
pbs <- new("individualBS",
           bs.hgt = new("bse", yname = "hgt",
                        data = pan@hgt,
                        at = "knots",
                        sex = pbg@sex),
           bs.wgt = new("bse", yname = "wgt", 
                        data = pan@wgt, 
                        at = "knots",
                        sex = pbg@sex))
pds <- new("individualDS")
rob <- new("individual", pid, pbg, pan, pbs, pds)
```

Doing this sequence by hand is somewhat inconvenient. Fortunately, there
are two functions that convert other formats into an object of class
`individual`:

  - `donordata_to_individual()` takes data in `donordata` format, and
    converts it into `individual` format;
  - `convert_bds_individual()` takes data in `bds` format, and converts
    it into `individual` format;

See the respective documentation for more detail. Both functions have
inverse (but lossy) transformations.

## Miscellaneous functionality

  - A `cabinet` is a collection of multiple objects of class
    `individual`;
  - `as(d1, "data.frame")` extracts the data frame from objects of class
    `xyz`;
  - `get_xyz(rob, "hgt")` extracts the data frame from objects of class
    `individual`;
  - `get_range(rob)` extract the age range from objects of class
    `individual`.
