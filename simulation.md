simulation
================
Zihan Xiong
2025-11-14

load key packages and source necessary files

``` r
library(tidyverse)
```

writing a function

``` r
sim_mean_sd=function(n_subj){
  x=rnorm(n_subj, mean=0, sd=1)
  
  tibble(
    mu_hat=mean(x),
    sigma_hat=sd(x))
}
sim_mean_sd(n_subj=400)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1 0.0128     0.975

``` r
output=vector("list", length=100)
for (i in 1:100) {
  output[[i]]=sim_mean_sd(30)#从总体中抽30个样本,算一次样本均值,重复100次
}

output |> bind_rows() |> #把list里的100个小tibble叠在一起
  ggplot(aes(x=mu_hat))+
  geom_density()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

``` r
#根据CLT,分布近似正态, 均值接近总体均值
```

``` r
#更系统的验证
sim_results_df=
  expand_grid(#生成所有组合的实验设计表, 得到一个4000*2列的tibble
    sample_size=c(30,60,90,120),
    iter=1:1000
  ) |>
  mutate(
    results=map(sample_size, sim_mean_sd)
  ) |>
  unnest(results)
```

violin plot

``` r
sim_results_df |>
  mutate(
    sample_size=str_c("n=", sample_size),
    sample_size=fct_inorder(sample_size)
  ) |>
  ggplot(aes(x=sample_size, y=mu_hat))+
  geom_violin()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
sim_results_df |>
  group_by(sample_size) |>
  summarize(
    emp_mean=mean(mu_hat),
    emp_se=sd(mu_hat)
  )#用数字验证CLT, 样本均值的平均值接近总体均值
```

    ## # A tibble: 4 × 3
    ##   sample_size  emp_mean emp_se
    ##         <dbl>     <dbl>  <dbl>
    ## 1          30 -0.00356  0.187 
    ## 2          60  0.000316 0.126 
    ## 3          90 -0.00333  0.113 
    ## 4         120  0.00492  0.0897

simple linear regression模拟一份简单线性回归

``` r
sim_df=
  tibble(
    x=rnorm(30,mean=1,sd=1),#x~Normal(1,1)
    y=2+3*x+rnorm(30,0,1)
  )
sim_df
```

    ## # A tibble: 30 × 2
    ##          x      y
    ##      <dbl>  <dbl>
    ##  1  2.04    7.16 
    ##  2  0.213   3.98 
    ##  3  1.56    6.36 
    ##  4 -1.06   -1.20 
    ##  5  0.785   5.62 
    ##  6  2.06    9.74 
    ##  7 -0.277   1.19 
    ##  8  0.155   2.78 
    ##  9  2.14   10.1  
    ## 10  0.0456 -0.806
    ## # ℹ 20 more rows

生成的数据关系

``` r
sim_df |>
  ggplot(aes(x=x,y=y)) +
  geom_point()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

``` r
slr_fit=lm(y~x,data=sim_df)#lm拟合线性回归,
coef(slr_fit)#coef输出估计的beta0 and beta1
```

    ## (Intercept)           x 
    ##    1.565912    3.544752

模拟封装成一个函数sim_regression()

``` r
sim_regression=function(n_subj, beta_0=1.46, beta_1=3.17) {#sample size=n_subj
  sim_df=
    tibble(
      x=rnorm(n_subj,mean=1,sd=1),
      y=beta_0+beta_1*x+rnorm(n_subj,0,1)
    )
  slr_fit=lm(y~x, data = sim_df)
  
  tibble(
    beta0_hat=coef(slr_fit)[1],
    beta1_hat=coef(slr_fit)[2]#长度为2的向量,c("intercept")和斜率
  )#把他们装进tibble,这样可以在for-loop或map里用bind_rows()合并
}
```

验证

``` r
sim_regression(n_subj=30)
```

    ## # A tibble: 1 × 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      1.32      3.43

``` r
output=vector("list", length=500)

for (i in 1:500) {
  output[i]=sim_regression(n_subj=30)
}
output |>
  bind_rows()
```

    ## # A tibble: 500 × 1
    ##    `(Intercept)`
    ##            <dbl>
    ##  1         1.49 
    ##  2         0.818
    ##  3         1.64 
    ##  4         1.75 
    ##  5         1.73 
    ##  6         1.73 
    ##  7         1.77 
    ##  8         1.55 
    ##  9         1.49 
    ## 10         1.45 
    ## # ℹ 490 more rows
