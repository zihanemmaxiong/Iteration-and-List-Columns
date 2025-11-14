Iteration
================
Zihan Xiong
2025-10-28

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## make a list

``` r
l=
  list(
    vec_numeric = 1: 23,
    char_vec = c("Zihan"),
    mat = matrix(1:8, nrow=2, ncol=4),
    summary=summary(rnorm(1000, mean=4))
  )
l
```

    ## $vec_numeric
    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
    ## 
    ## $char_vec
    ## [1] "Zihan"
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4658  3.3321  3.9130  3.9759  4.6723  7.9232

## 取list的三种等价方式

``` r
l[[1]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l[["vec_numeric"]]
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## make another list

``` r
list_normals=
  list(
    a=rnorm(30, mean=3, sd=1),
    b=rnorm(30, mean=30, sd=1),
    c=rnorm(30, mean=3, sd=10),
    d=rnorm(30, mean=-3, sd=4)
  )
```

``` r
mean_and_sd=function(x){
  c(mean=mean(x), sd=sd(x))
}
```

``` r
mean_and_sd(list_normals [[1]])
```

    ##     mean       sd 
    ## 3.021898 1.169049

``` r
mean_and_sd(list_normals [[2]])
```

    ##      mean        sd 
    ## 29.953865  1.132362

``` r
mean_and_sd(list_normals [[3]])
```

    ##      mean        sd 
    ##  2.369463 10.403310

``` r
mean_and_sd(list_normals [[4]])
```

    ##      mean        sd 
    ## -3.240000  4.715577

## use loop to iterate

``` r
output=vector("list",length = 4)#生成空的list为结果容器, 长度为4
for (i in 1:4) {
  output[[i]]=mean_and_sd(list_normals[[i]]) #把第i次循环的结果放到第i个位置
  
}
output
```

    ## [[1]]
    ##     mean       sd 
    ## 3.021898 1.169049 
    ## 
    ## [[2]]
    ##      mean        sd 
    ## 29.953865  1.132362 
    ## 
    ## [[3]]
    ##      mean        sd 
    ##  2.369463 10.403310 
    ## 
    ## [[4]]
    ##      mean        sd 
    ## -3.240000  4.715577

## now use map

``` r
output=map(list_normals, mean_and_sd)#每一格运行mean sd,并把结果存入一个新的list, 等价于:
```

``` r
output=map(list_normals, median)
output
```

    ## $a
    ## [1] 2.906964
    ## 
    ## $b
    ## [1] 29.8444
    ## 
    ## $c
    ## [1] 1.219202
    ## 
    ## $d
    ## [1] -3.188747

## map_dfr/map_dbl

``` r
#输出一个data frame
map_dfr(list_normals, mean_and_sd, .id="sample")#row bind变成一个data frame
```

    ## # A tibble: 4 × 3
    ##   sample  mean    sd
    ##   <chr>  <dbl> <dbl>
    ## 1 a       3.02  1.17
    ## 2 b      30.0   1.13
    ## 3 c       2.37 10.4 
    ## 4 d      -3.24  4.72

``` r
#输出一个numeric vector
map_dbl(list_normals,median)
```

    ##         a         b         c         d 
    ##  2.906964 29.844403  1.219202 -3.188747

``` r
listcol_df=
  tibble(
    name=c("a","b","c","d"),#每一行sample存的是一个向量,不是一个单值
    sample=list_normals
  )
listcol_df
```

    ## # A tibble: 4 × 2
    ##   name  sample      
    ##   <chr> <named list>
    ## 1 a     <dbl [30]>  
    ## 2 b     <dbl [30]>  
    ## 3 c     <dbl [30]>  
    ## 4 d     <dbl [30]>

``` r
pull(listcol_df,name)#检查数据框是否真的装了list
```

    ## [1] "a" "b" "c" "d"

``` r
pull(listcol_df,sample)
```

    ## $a
    ##  [1] 2.9515839 1.4357671 5.4102472 2.6334359 4.6568134 3.6325430 2.3140053
    ##  [8] 2.9030325 4.4495657 2.1360758 3.2402039 4.6428697 2.9108961 2.8305398
    ## [15] 2.7991071 3.3271292 3.1169380 4.6222964 1.5599158 2.8140721 3.0388268
    ## [22] 0.2281743 2.2114072 1.3943371 1.9958025 4.3862567 4.5902150 2.8489889
    ## [29] 2.2260093 3.3498912
    ## 
    ## $b
    ##  [1] 29.52249 31.38640 30.57367 32.36099 28.93878 30.66496 29.90878 30.26768
    ##  [9] 28.06141 29.43850 29.08462 29.23243 29.55649 30.89274 31.74490 29.75396
    ## [17] 28.39608 29.78003 30.56754 30.12766 30.32101 29.04512 28.06779 31.60900
    ## [25] 31.80087 29.10495 28.59934 30.80700 29.00308 29.99768
    ## 
    ## $c
    ##  [1]  10.7691972   3.1859192  -5.9429789  -3.4668512  19.8106392 -19.6828195
    ##  [7]   2.8639644  10.9633758  16.4541076  -7.8622072  18.7910011  -8.2620644
    ## [13] -11.3408572   1.0225691   8.5141599   0.4924683 -10.3254178  -8.4622521
    ## [19]  -6.9697888  11.3637373  13.0675819   8.7463271  21.2045115  -0.8902201
    ## [25]  -1.4156886   7.8196806   0.1573701   8.1852272   1.4158347  -9.1226340
    ## 
    ## $d
    ##  [1]  -2.6075272  -1.8611592  -5.4444519 -14.6490035   0.5123468  -6.8804847
    ##  [7]  -6.5406166   4.4053669  -3.0966052   5.4054655  -1.8156501  -9.1043838
    ## [13]  -1.4882254  -7.0276410  -5.2387813  -3.6901019  -4.9911748   2.8201243
    ## [19]   0.8541707   0.9927861  -3.2808883  -2.8286190  -7.2571882   2.6092767
    ## [25]  -2.8357165  -8.5674488  -4.4606628  -8.3990573   5.3187765  -8.0529256

``` r
mean_and_sd(pull(listcol_df,sample)[[1]])#对第一组数据向量计算,但是太慢, iterate
```

    ##     mean       sd 
    ## 3.021898 1.169049

``` r
map(pull(listcol_df,sample), mean_and_sd)
```

    ## $a
    ##     mean       sd 
    ## 3.021898 1.169049 
    ## 
    ## $b
    ##      mean        sd 
    ## 29.953865  1.132362 
    ## 
    ## $c
    ##      mean        sd 
    ##  2.369463 10.403310 
    ## 
    ## $d
    ##      mean        sd 
    ## -3.240000  4.715577

\##添加一个新的list column summary

``` r
listcol_df=
  listcol_df |>
  mutate(
    summary=map(sample, mean_and_sd)
  )
pull(listcol_df,summary)
```

    ## $a
    ##     mean       sd 
    ## 3.021898 1.169049 
    ## 
    ## $b
    ##      mean        sd 
    ## 29.953865  1.132362 
    ## 
    ## $c
    ##      mean        sd 
    ##  2.369463 10.403310 
    ## 
    ## $d
    ##      mean        sd 
    ## -3.240000  4.715577

``` r
listcol_df |>
  select(-sample) |>
  unnest(summary)
```

    ## # A tibble: 8 × 2
    ##   name  summary
    ##   <chr>   <dbl>
    ## 1 a        3.02
    ## 2 a        1.17
    ## 3 b       30.0 
    ## 4 b        1.13
    ## 5 c        2.37
    ## 6 c       10.4 
    ## 7 d       -3.24
    ## 8 d        4.72

\##网页抓取+清洗+迭代

``` r
nsduh_url="http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
nsduh_html=read_html(nsduh_url)#这个HTML文件里包含多个tables
```

``` r
nsduh_import=function(html,table_num){
  data=html |> #开始清洗
    html_table() |> #自动抓取表哥,返回一个list, 每个元素是一个表
    nth(table_num) |>#取第几个表
    slice(-1) |> #去掉第一hang
    select(-contains("P value")) |>
    pivot_longer(
      -State,
      names_to = "age_year",
      values_to = "percent") |>
    separate(age_year, into=c("age","year"), sep = "\\(") |>
    mutate(
      year=str_replace(year,"\\)",""),
      percent=str_replace(percent,"[a-c]$",""),
      percent=as.numeric(percent)) |>
    filter(!(State %in% c("Total U.S.", "Northeast","Midwest", "South", "West")))
    data
}
nsduh_import(nsduh_html,table_num = 1)#用iteration运行函数3次, nsduh_import的作用就是从html中抽一张表,清理成人类可读的data,输出一个干净的tibble
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html,table_num = 2)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows

``` r
nsduh_import(nsduh_html,table_num =3)
```

    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows

``` r
#这是手动运行import1 2 3
```

## for loop循环

``` r
output=vector("list", length=3)
for (i in 1:3) {
  output [[i]]=nsduh_import(html=nsduh_html,i)
}
output
```

    ## [[1]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows
    ## 
    ## [[2]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows
    ## 
    ## [[3]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows

``` r
map(1:3, nsduh_import, html=nsduh_html)#遍历1、2、3, 第二个参数 table_num 传给 nsduh_import, 
```

    ## [[1]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    9.98
    ##  2 Alabama 12+   2014-2015    9.6 
    ##  3 Alabama 12-17 2013-2014    9.9 
    ##  4 Alabama 12-17 2014-2015    9.71
    ##  5 Alabama 18-25 2013-2014   27.0 
    ##  6 Alabama 18-25 2014-2015   26.1 
    ##  7 Alabama 26+   2013-2014    7.1 
    ##  8 Alabama 26+   2014-2015    6.81
    ##  9 Alabama 18+   2013-2014    9.99
    ## 10 Alabama 18+   2014-2015    9.59
    ## # ℹ 500 more rows
    ## 
    ## [[2]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    5.57
    ##  2 Alabama 12+   2014-2015    5.35
    ##  3 Alabama 12-17 2013-2014    4.98
    ##  4 Alabama 12-17 2014-2015    5.16
    ##  5 Alabama 18-25 2013-2014   15.0 
    ##  6 Alabama 18-25 2014-2015   14.3 
    ##  7 Alabama 26+   2013-2014    4.03
    ##  8 Alabama 26+   2014-2015    3.86
    ##  9 Alabama 18+   2013-2014    5.63
    ## 10 Alabama 18+   2014-2015    5.37
    ## # ℹ 500 more rows
    ## 
    ## [[3]]
    ## # A tibble: 510 × 4
    ##    State   age   year      percent
    ##    <chr>   <chr> <chr>       <dbl>
    ##  1 Alabama 12+   2013-2014    1.42
    ##  2 Alabama 12+   2014-2015    1.49
    ##  3 Alabama 12-17 2013-2014    4.46
    ##  4 Alabama 12-17 2014-2015    4.36
    ##  5 Alabama 18-25 2013-2014    6.04
    ##  6 Alabama 18-25 2014-2015    6.39
    ##  7 Alabama 26+   2013-2014    0.15
    ##  8 Alabama 26+   2014-2015    0.2 
    ##  9 Alabama 18+   2013-2014    0.95
    ## 10 Alabama 18+   2014-2015    1.05
    ## # ℹ 500 more rows

``` r
nsduh_df=
  tibble(
    name=c("marj year","marj month","marj first"),
    number=1:3
  ) |>
  mutate(
    table=map(number,nsduh_import,html=nsduh_html)
  ) |>
  unnest(table)
```
