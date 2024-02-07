Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-42. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")

# Read in the met data and fix lat, lon, temp
# code from previous lab
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10

# Relative Humidity
met$rh <- 100*((112-0.1*met$temp+met$dew.point)/(112+0.9*met$temp))^8

# remove suspicious temp data
met <- met[temp > -10]
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met_stations <- merge(
 x = met,
 y = stations,
 by.x = "USAFID",
 by.y = "USAF",
 all.x = TRUE,
 all.y = FALSE
 ) 
```

``` r
colnames(met_stations)
```

    ##  [1] "USAFID"            "WBAN"              "year"             
    ##  [4] "month"             "day"               "hour"             
    ##  [7] "min"               "lat"               "lon"              
    ## [10] "elev"              "wind.dir"          "wind.dir.qc"      
    ## [13] "wind.type.code"    "wind.sp"           "wind.sp.qc"       
    ## [16] "ceiling.ht"        "ceiling.ht.qc"     "ceiling.ht.method"
    ## [19] "sky.cond"          "vis.dist"          "vis.dist.qc"      
    ## [22] "vis.var"           "vis.var.qc"        "temp"             
    ## [25] "temp.qc"           "dew.point"         "dew.point.qc"     
    ## [28] "atm.press"         "atm.press.qc"      "rh"               
    ## [31] "CTRY"              "STATE"             "LAT"              
    ## [34] "LON"

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
# also add in lat/lon and state for later question use
station_medians <- met_stations |>
  group_by(USAFID) |>
  summarise(
    median_temp = median(temp, na.rm = TRUE),
    median_windsp = median(wind.sp, na.rm = TRUE),
    median_pressure = median(atm.press, na.rm = TRUE),
    lat = median(lat, na.rm = TRUE),
    lon = median(lon, na.rm = TRUE),
    state = first(STATE),
  )

# remove stations with na
station_medians <- station_medians[!is.na(station_medians$median_temp), ]
station_medians <- station_medians[!is.na(station_medians$median_windsp), ]
station_medians <- station_medians[!is.na(station_medians$median_pressure), ]

national_medians <- met_stations |>
  summarise(
    median_temp = median(temp, na.rm = TRUE),
    median_windsp = median(wind.sp, na.rm = TRUE),
    median_pressure = median(atm.press, na.rm = TRUE)
  )
```

Next identify the stations have these median values.

``` r
stations_temp <- station_medians[station_medians$median_temp == national_medians$median_temp, "USAFID"]
stations_windsp <- station_medians[station_medians$median_windsp == national_medians$median_windsp, "USAFID"]
stations_pressure <- station_medians[station_medians$median_pressure == national_medians$median_pressure, "USAFID"]

list(
  temp_median_station = stations_temp,
  temp_median_windsp = stations_windsp,
  temp_median_pressure = stations_pressure
)
```

    ## $temp_median_station
    ## # A tibble: 45 × 1
    ##    USAFID
    ##     <int>
    ##  1 722180
    ##  2 722196
    ##  3 722197
    ##  4 723075
    ##  5 723086
    ##  6 723110
    ##  7 723119
    ##  8 723190
    ##  9 723194
    ## 10 723200
    ## # ℹ 35 more rows
    ## 
    ## $temp_median_windsp
    ## # A tibble: 334 × 1
    ##    USAFID
    ##     <int>
    ##  1 720306
    ##  2 720333
    ##  3 720377
    ##  4 720379
    ##  5 720652
    ##  6 722011
    ##  7 722020
    ##  8 722026
    ##  9 722029
    ## 10 722038
    ## # ℹ 324 more rows
    ## 
    ## $temp_median_pressure
    ## # A tibble: 23 × 1
    ##    USAFID
    ##     <int>
    ##  1 720394
    ##  2 722085
    ##  3 722348
    ##  4 723020
    ##  5 723119
    ##  6 723124
    ##  7 723270
    ##  8 723658
    ##  9 724010
    ## 10 724035
    ## # ℹ 13 more rows

``` r
Reduce(intersect, list(stations_temp, stations_windsp, stations_pressure))
```

    ## # A tibble: 1 × 1
    ##   USAFID
    ##    <int>
    ## 1 723119

- We found that 45 stations have the same median temperature as the
  national median temperature, 334 stations have the same median wind
  speed as the national median wind speed, and 23 stations have the same
  median atmospheric pressure as the national median atmospheric
  pressure. I have found one station (USAFID = 723119) that satisfies
  the three median conditions together, so there is one station that
  conincide the median conditions.
- Notice this result is based on that we remove stations with temp below
  -10, so the numbers might not match exactly if you filter the data
  differently at the beginning.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
station_medians$edistance <- sqrt(
  (station_medians$median_temp - national_medians$median_temp)^2 +
  (station_medians$median_windsp - national_medians$median_windsp)^2
)

station_medians |>
  group_by(state) |>
  filter(edistance == min(edistance)) |>
  ungroup()
```

    ## # A tibble: 60 × 8
    ##    USAFID median_temp median_windsp median_pressure   lat   lon state edistance
    ##     <int>       <dbl>         <dbl>           <dbl> <dbl> <dbl> <chr>     <dbl>
    ##  1 720708        23.3           2.6           1004   32.8 -88.8 MS        1.68 
    ##  2 722151        18.9           3.1           1010.  41.4 -71.8 RI        2.8  
    ##  3 722180        21.7           3.1           1011.  33.4 -82.0 GA        0    
    ##  4 722285        22             2.6           1012.  34.0 -86.1 AL        0.583
    ##  5 722488        25.6           2.1           1012.  32.3 -91.0 LA        4.03 
    ##  6 723087        22             3.1           1012.  37.1 -76.6 VA        0.300
    ##  7 723119        21.7           3.1           1012.  34.8 -82.4 SC        0    
    ##  8 723190        21.7           3.1           1011.  34.5 -82.7 SC        0    
    ##  9 723194        21.7           3.1           1012.  35.0 -80.6 NC        0    
    ## 10 723231        22             2.6           1011   34.7 -86.7 AL        0.583
    ## # ℹ 50 more rows

- Here, we end up 60 stations that is more than the number of states in
  US. This is because for some states we have multiple stations that
  have the same minimum euclidean distance.

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
