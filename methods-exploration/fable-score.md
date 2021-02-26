fable-scores
================

Working through fable forecast to EFI score…

``` r
library(scoringRules)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ISOweek) 
library(forecast)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
library(fable)
```

    ## Loading required package: fabletools

    ## 
    ## Attaching package: 'fabletools'

    ## The following objects are masked from 'package:forecast':
    ## 
    ##     accuracy, forecast

``` r
library(tsibble)
```

    ## 
    ## Attaching package: 'tsibble'

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, union

``` r
library(tsibbledata)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:tsibble':
    ## 
    ##     interval

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
```

``` r
## Get the latest beetle target data.  
download.file("https://data.ecoforecast.org/targets/beetles/beetles-targets.csv.gz",
              "beetles-targets.csv.gz")
targets <-  read_csv("beetles-targets.csv.gz")
```

``` r
targets_ts <- targets %>%
  select(siteID, time, richness) %>%
  mutate(iso_week = ISOweek::date2ISOweek(time)) %>%
  separate(iso_week, into = c("year", "week", "day"))  %>%
  mutate(time = ISOweek::ISOweek2date(paste(year,week, "1", sep = "-")))

targets_tsb <- as_tsibble(targets_ts, key = siteID, index = time)
```

``` r
targets_tsb <- targets_tsb %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(richness, .direction = "down") 
```

``` r
train <- targets_tsb %>%
  filter(year(time) < 2020) %>%
  select(siteID, time, richness)
```

Trying simple arima model on richness

``` r
fit <- train %>%
  model(arima = ARIMA(richness)) 
```

    ## Warning in sqrt(diag(best$var.coef)): NaNs produced

Forecast\! Gives a mean and distribution for ricness value weekly for
2020

``` r
fc <- fit %>%
  forecast(h = "12 months",
           simulate= TRUE,
           iterations = 500) 
```

can calculate CRSP score but not in the same format as EFI scoring (one
score per site for the whole year..)

``` r
 crsp_score <- fc  %>%
  accuracy(targets_tsb, list(crps = CRPS))
```

    ## Warning: The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
    ## 73 observations are missing between 2019-08-12 and 2020-12-28
