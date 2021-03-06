
[![Build
Status](https://travis-ci.org/Wences91/testnet.svg?branch=master)](https://travis-ci.org/Wences91/testnet)

## Test network

Network analysis in test results in R

## Installation

``` r
# Install from GitHub
# install.packages('devtools')
devtools::install_github('Wences91/testnet')
```

## Example

Read data.

``` r
# import data
answers <- read.delim('data/answers.tsv',
                      stringsAsFactors = FALSE, check.names = FALSE)
categories <- read.delim('data/categories.tsv',
                         header = FALSE,
                         colClasses = c('character'),
                         stringsAsFactors = FALSE, check.names = FALSE)
c_answers <- read.delim('data/correct_answers.tsv',
                        header = FALSE,
                        colClasses = c('character'),
                        stringsAsFactors = FALSE, check.names = FALSE)
```

Obtain edges and create co-fails
network.

``` r
edges <- testnet::obtain_edges(answers, c_answers, avoid =  c('13', '64', '47'))
tm_net <- testnet::co_fails(edges)
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
