
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minihealth

<!-- badges: start -->
<!-- badges: end -->

The `minihealth` package is a central component of
[JAMES](https://github.com/stefvanbuuren/james). The package

-   Defines S4 classes `xyz`, `bse`, `ird`, `individual` and `cabinet`
    for analysing individual growth data;
-   Translates `individual`, `donordata` and `bds` formats into each
    other;
-   Extracts data and ranges from `individual` objects.

## Installation

Install the development version `minihealth` by

``` r
install.packages("remotes")
remotes::install_github("stefvanbuuren/minihealth")
```

There is no release on CRAN.

## Example 1: Automatic Z-score calculation

The S4 class `xyz` stores three variables useful for anthropometric
data:

-   `x`: usually age, but can also be height (in weight-for-height);
-   `y`: measurement, e.g. height or weight;
-   `z`: Z-score of `y` conditional on `x`.

Here are some examples for automatic *Z*-score calculation:

``` r
library(minihealth)
#> Loading required package: nlreferences
#> Loading required package: donorloader

# specify length (in cm) for boy at ages 0, 0.2 and 0.5 years
new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4))
#>   age  hgt hgt_z
#> 1 0.0 51.0    NA
#> 2 0.2 54.1    NA
#> 3 0.5 63.4    NA

# at the minimum, specify sex for automatic Z-score calculation
new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "male")
#>   age  hgt  hgt_z
#> 1 0.0 51.0 -0.154
#> 2 0.2 54.1 -2.312
#> 3 0.5 63.4 -1.829

# specify weight (in kg) at same ages
d1 <- new("xyz", x = c(0, 0.2, 0.5), y = c(3.2, 5.2, 7.0), 
          sex = "male", yname = "wgt")
d1
#>   age wgt  wgt_z
#> 1 0.0 3.2 -0.874
#> 2 0.2 5.2 -0.575
#> 3 0.5 7.0 -1.048

# View reference names used to calculate Z-scores in d1
data.frame(d1)
#>   xname yname zname   x   y      z          pkg             refcode
#> 1   age   wgt wgt_z 0.0 3.2 -0.874 nlreferences nl_1997_wgt_male_nl
#> 2   age   wgt wgt_z 0.2 5.2 -0.575 nlreferences nl_1997_wgt_male_nl
#> 3   age   wgt wgt_z 0.5 7.0 -1.048 nlreferences nl_1997_wgt_male_nl
```

It is also possible to perform the reverse calculation, where `z` is
given and `y` is calculated.

``` r
# Standard weight centiles at age 0.5 year of Dutch boys
new("xyz", x = rep(0.5, 5), z = -2:2, sex = "male", yname = "wgt")
#>   age  wgt wgt_z
#> 1 0.5 6.24    -2
#> 2 0.5 7.04    -1
#> 3 0.5 7.90     0
#> 4 0.5 8.83     1
#> 5 0.5 9.82     2

# Extend to grid of ages: 0y, 0.5y and 1y
new("xyz", x = rep(c(0, 0.5, 1), each = 5), z = rep(-2:2, 3), 
    sex = "male", yname = "wgt")
#>    age   wgt wgt_z
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
#> 12 1.0  9.14    -1
#> 13 1.0 10.24     0
#> 14 1.0 11.44     1
#> 15 1.0 12.75     2
```

See the help (“?`xyz-class`”) and the
[centile](https://github.com/growthcharts/centile) package for more
examples.

## Example 2: Automatic brokenstick estimation

``` r
# specify three height measures
boy <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), 
           sex = "male")
boy
#>   age  hgt  hgt_z
#> 1 0.0 51.0 -0.154
#> 2 0.2 54.1 -2.312
#> 3 0.5 63.4 -1.829

# calculate broken stick estimates at observed ages
new("bse", data = boy)
#> donorloader::load_data(dnr = "smocc_bs", element = "hgt")
#>   age  hgt  hgt_z
#> 1 0.0 50.6 -0.349
#> 2 0.2 54.9 -1.982
#> 3 0.5 63.5 -1.801

# calculate broken stick estimates at all break points
new("bse", data = boy, at = "knots")
#> donorloader::load_data(dnr = "smocc_bs", element = "hgt")
#>       age  hgt  hgt_z
#> 1  0.0000 50.6 -0.349
#> 2  0.0767 50.9 -1.640
#> 3  0.1533 53.2 -1.945
#> 4  0.2500 56.5 -2.020
#> 5  0.3333 60.0 -1.599
#> 6  0.5000 63.5 -1.801
#> 7  0.6250 66.5 -1.532
#> 8  0.7500 68.1 -1.740
#> 9  0.9167 70.7 -1.708
#> 10 1.1667 74.3 -1.605
#> 11 1.5000 78.3 -1.588
#> 12 2.0000 84.0 -1.483
#> 13 3.0000 92.4 -1.547
```

See the help (“?`bse-class`”) and the
[brokenstick](https://github.com/growthcharts/brokenstick) package for
more examples.

## Example 3: Bundle all measures by individual

The S4 class `individual` bundles four different types of information,
each of which is coded by its own class:

-   `individualID`: ID information, like name, date of birth;
-   `individualBG`: Background, like sex, gestional age or etnicity;
-   `individualAN`: Bundles person’s anthrometric data, like `hgt` and
    `wgt`;
-   `individualBS`: Bundles person’s brokenstick estimates;
-   `individualRW`: Stores raw data, e.g. milestones;

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
data <- data.frame(age = c(0.2, 0.5, 0.7),
                   k1430 = c(1, NA, NA),
                   k1431 = c(2, NA, NA),
                   k1437 = c(3, 1, 1),
                   k1438 = c(0, 1, 1),
                   k1439 = c(0, 1, 1))
map <- data.frame(from = c("k1430", "k1431", "k1437", "k1438", "k1439"),
                  to = c(879, 927, 928, 881, 883))
prw <- new("individualRW",
           ddi = new("ird", mst = data, map = map, instrument = "ddi"))

rob <- new("individual", pid, pbg, pan, pbs, prw)
```

Doing this sequence by hand is somewhat inconvenient. Fortunately, there
are two functions that convert other formats into an object of class
`individual`:

-   `donordata_to_individual()` takes data in `donordata` format, and
    converts it into `individual` format;
-   `convert_bds_individual()` takes data in `bds` format, and converts
    it into `individual` format;

See the respective documentation for more detail. Both functions have
inverse (but lossy) transformations.

## Miscellaneous functionality

-   A `cabinet` is a collection of multiple objects of class
    `individual`;
-   `data.frame(d1)` extracts the data frame from objects of class
    `xyz`;
-   `get_xyz(rob, "hgt")` extracts the data frame from objects of class
    `individual`;
-   `get_range(rob)` extract the age range from objects of class
    `individual`.
