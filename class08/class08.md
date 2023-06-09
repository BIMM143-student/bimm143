Mini Project: PCA
================
Marcos

``` r
wisc.df <- read.csv('WisconsinCancer.csv')
```

Q1

``` r
nrow(wisc.df)
```

    [1] 569

``` r
table(wisc.df$diagnosis)
```


      B   M 
    357 212 

``` r
diagnosis <- wisc.df$diagnosis
wisc.data <- wisc.df[,-1]
```

Q3

``` r
grep("_mean", colnames(wisc.df))
```

     [1]  3  4  5  6  7  8  9 10 11 12
