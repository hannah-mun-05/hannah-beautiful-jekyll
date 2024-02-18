HDAT9700: Assessment 1C - Chapters 7 & 8
================
Hannah Mun
12 Nov 2023

### Overview

For this assessment you will use a subset of data from a study of the
impact of a new law on the homicide rate in the state of Florida, USA
(data are from:
[10.1093/ije/dyaa152](https://academic.oup.com/ije/article-abstract/49/6/2010/5917161)).

On 1 October 2005, Florida implemented the “Stand-your-ground law,”
which extends the use of lethal force as a legitimate defense where an
individual perceives a threat of harm, such as for the protection of
private property. For more information see:
<https://en.wikipedia.org/wiki/Stand-your-ground_law>. Florida was one
of the first US states to implement this law.

In this assessment you will evaluate the impact of this law on deaths by
homicide. The main outcome variable is the homicide rate per 100,000
population in Florida from January 2000 to December 2007.

The *flhom.csv* file contains the following variables:

- **date** - Month and year
- **hom_rate** - Homicide rate per 100,000

You will need to have the following packages installed: `readr` (to read
in the csv file), `astsa`, `lmtest`, `forecast`, and `zoo`.

The data are contained in your assignment repo and can be read as
follows:

``` r
flhom <- read.csv("flhom.csv", header=TRUE)
```

------------------------------------------------------------------------

### Assessment questions

1)  Create a plot of the homicide rate over time, as if you were
    preparing it for inclusion in a report or publication. (20%)

``` r
# Quick look of the dataset
head(flhom)
```

    ##     date  hom_rate
    ## 1 Jan-00 0.5732980
    ## 2 Feb-00 0.4611310
    ## 3 Mar-00 0.4486680
    ## 4 Apr-00 0.4798255
    ## 5 May-00 0.5732980
    ## 6 Jun-00 0.4362050

``` r
summary(flhom)
```

    ##      date              hom_rate     
    ##  Length:96          Min.   :0.3139  
    ##  Class :character   1st Qu.:0.4593  
    ##  Mode  :character   Median :0.5016  
    ##                     Mean   :0.5116  
    ##                     3rd Qu.:0.5585  
    ##                     Max.   :0.7840

``` r
is.null(flhom) # no missing value in the dataset
```

    ## [1] FALSE

``` r
# Convert 'date' column from <chr> to <date> format
flhom$date_new = as.Date(x=paste0("01-",flhom$date), format = "%d-%B-%y")
flhom$year_new = year(flhom$date_new)

# Plot   
ggplot(flhom, aes(x=date_new, y=hom_rate, colour = year_new )) +
  geom_smooth(linewidth = 0, colour="lightgrey", alpha=0.3, span=1) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.numeric(as.Date("2005-10-01")), colour = "red")+
  geom_text(x=as.numeric(as.Date("2005-9-01")), y=7, label="Stand-your-ground", hjust ="right", colour = "red", size = 3) +
  geom_text(x=as.numeric(as.Date("2005-9-01")), y=6.7, label="law implementation", hjust ="right", colour = "red", size = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("") + ylab("Homicide rate per 100,000 people") +
  ggtitle("Monthly time series of homicide rate per 100,000 people in the state of Florida, USA from 2000 to 2007") +
  theme(legend.position="none") +
  labs(caption = "Legend. 'Stand-your-ground law' was implemented on 01 OCT 2005")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](submission_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

2)  Describe the homicide rate time series in terms of the trend,
    seasonality, and outliers. Provide appropriate plots and summary
    statistics to support your statements. (20%)

As discovered above, the plot shows non-stationarity with a slight of
increasing trend, probably due to a presence of a trend, or non-constant
variance.

``` r
# Create time series data
rate_ts <- ts(flhom$hom_rate, start = c(2000,1) , end =c(2007,12) , frequency = 12)

# Check stationarity
plot(log(rate_ts),  ylab = "Homicide rate (log)",type="l")
```

![](submission_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

There is a still slight increasing trend over time in log series. Let’s
take the difference of the logged series.

``` r
# Take the difference and plot the data
plot(diff(log(rate_ts)), ylab = "Homicide rate (log difference)", type="l")
```

![](submission_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

With log difference, time series is almost stationary. It is not clear
but seems like there might be a seasonality.

``` r
# Seasonal decomposition
plot(decompose(rate_ts))
```

![](submission_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggseasonplot(rate_ts, year.labels = TRUE)
```

![](submission_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
acf(rate_ts)
```

![](submission_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

From seasonal plot and ACF plot, there is a seasonality that homicide
rate tends to decrease in early of each year. Next, we will remove the
effect of seasonality.

``` r
# Aggregate from monthly to quarterly data
rate_ts.qtr <- aggregate(rate_ts, nfrequency = 4, FUN = sum)
plot(rate_ts.qtr, ylab = "Homicide rate per 100,000 people", xlab = "Quarter")
```

![](submission_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# Aggregate from monthly to yearly data
rate_ts.yr <- aggregate(rate_ts, nfrequency = 1, FUN = sum)
plot(rate_ts.yr, ylab = "Homicide rate per 100,000 people", xlab = "Year")
```

![](submission_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

With the seasonality removed, we can see there was a big drop of
homicide rate in 2005, then it drastically increased again. Considering
the law was implemented October 2005,it implicates that we can’t ignore
the policy change affected to the homicide rate.

From above, we can see that there are some time when homicide rate is
dramatically low or high. We will examine if they should be considered
as outliers or not. Below is a boxplot of homicide rate.

``` r
boxplot(flhom$hom_rate*10)
```

![](submission_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The plot shows that one data is far away from the majority homicide rate
between 3 to 7 people per million people. Grubbs test is examined to
determine whether this was considered as an outliner or not. Normality
of the data was examined in order to apply Grubbs test for outlier.

``` r
# QQ plot to inspect normality before Grubbs test
qqnorm(flhom$hom_rate, pch = 1, frame = FALSE)
qqline(flhom$hom_rate, col = "steelblue")
```

![](submission_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Most of the data points follow a normal distribution except a few data
near the tail. Grubb test is conducted to examine if the highest value
is an outlier.

``` r
# Grubss test for one sided outlier
grubbs.test(flhom$hom_rate, type = 10)
```

    ## 
    ##  Grubbs test for one outlier
    ## 
    ## data:  flhom$hom_rate
    ## G = 3.69357, U = 0.85488, p-value = 0.006176
    ## alternative hypothesis: highest value 0.783980662 is an outlier

P-value of the test is smaller than 0.05, thus we can reject Null
hypothesis(H0= No outliers in the data) and the data where homicide rate
is 0.783980662 an outlier.

3)  Create the necessary vectors and fit the segmented regression models
    listed below. Here, `time` is the time since start of the study,
    `syg` is an indicator for the “Stand-your-ground” law (before=0,
    after=1), `time.after` is the time since the law was implemented,
    `month` is a monthly dummy variable, and `syg.lag1` and
    `time.after.lag1` are `syg` and `time.after` delayed/lagged by one
    month. Comment on the fit/appropriateness of each model, and state
    which model you believe best describes the association between the
    intervention, and the homicide rate over time. Provide evidence to
    justify your answer. (25%)

    - **Model 1** - hom_rate ~ time + syg + time.after + month
    - **Model 2** - hom_rate ~ time + syg + time.after
    - **Model 3** - hom_rate ~ time + syg.lag1 + time.after.lag1 + month
    - **Model 4** - hom_rate ~ time + syg.lag1 + time.after.lag1

``` r
# Create a sequence vector for time since start of the study
time <- seq(1,length(rate_ts)) %>% 
        ts(start=c(2000,01), frequency = 12)

# Create a sequence vector indicator for the "Stand-your-ground" law (before=0, after=1),
syg <- ifelse(as.yearmon(time(rate_ts))>='Oct 2005',1,0) %>% ts(start=c(2000,01), frequency=12)

# Create a sequence vector representing time since intervention
time.after <- append(rep(0,sum(as.yearmon(time(rate_ts))<'Oct 2005')),
              seq(1,sum(as.yearmon(time(rate_ts))>='Oct 2005'))) %>%
       ts(start=c(2000,01),frequency=12)

# Create a monthly dummy variable
month <- seasonaldummy(rate_ts)

# Create a lagged variable of 'syg' by one month
syg.lag1 <- ifelse(as.yearmon(time(rate_ts))>='Oct 2005',1,0) %>% ts(start=c(2000,01), frequency=12)

# Create a lagged variable of 'time.after' by one month
time.after.lag1 <- append(rep(0,sum(as.yearmon(time(rate_ts))<'Oct 2005')),seq(1,sum(as.yearmon(time(rate_ts))>='Oct 2005'))) |>
  ts(start=c(2000,01),frequency=12)

# Check new variables
#time
#syg
#time.after
#month
#syg.lag1
#time.after.lag1
```

``` r
# Fit a model 1 - time, syg, time.after and month as predictor without a month lag.
model1 <- lm(rate_ts ~ time + syg + time.after + month)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = rate_ts ~ time + syg + time.after + month)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.110299 -0.033801  0.000054  0.035107  0.121517 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.5017845  0.0228902  21.921  < 2e-16 ***
    ## time        -0.0003968  0.0003284  -1.208  0.23050    
    ## syg          0.0471736  0.0253000   1.865  0.06586 .  
    ## time.after   0.0040275  0.0013838   2.910  0.00466 ** 
    ## monthJan     0.0211940  0.0271600   0.780  0.43747    
    ## monthFeb    -0.0683255  0.0271290  -2.519  0.01375 *  
    ## monthMar    -0.0379735  0.0271044  -1.401  0.16503    
    ## monthApr    -0.0097168  0.0270864  -0.359  0.72073    
    ## monthMay     0.0354226  0.0270748   1.308  0.19446    
    ## monthJun    -0.0038374  0.0270698  -0.142  0.88762    
    ## monthJul     0.0703831  0.0270714   2.600  0.01108 *  
    ## monthAug     0.0143298  0.0270794   0.529  0.59813    
    ## monthSep    -0.0050336  0.0270940  -0.186  0.85308    
    ## monthOct     0.0151599  0.0270474   0.560  0.57669    
    ## monthNov    -0.0319472  0.0270307  -1.182  0.24071    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05405 on 81 degrees of freedom
    ## Multiple R-squared:  0.5418, Adjusted R-squared:  0.4626 
    ## F-statistic: 6.842 on 14 and 81 DF,  p-value: 5.384e-09

``` r
plot(model1)
```

![](submission_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
# Fit a model 2 - time, syg and time.after as predictor without a month lag.
model2 <- lm(rate_ts ~ time + syg + time.after)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = rate_ts ~ time + syg + time.after)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.170144 -0.038640 -0.002305  0.030320  0.179822 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.4992893  0.0150358  33.207  < 2e-16 ***
    ## time        -0.0003194  0.0003734  -0.855  0.39456    
    ## syg          0.0378202  0.0285367   1.325  0.18835    
    ## time.after   0.0043688  0.0015713   2.780  0.00658 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06177 on 92 degrees of freedom
    ## Multiple R-squared:  0.3203, Adjusted R-squared:  0.2982 
    ## F-statistic: 14.45 on 3 and 92 DF,  p-value: 8.611e-08

``` r
plot(model2)
```

![](submission_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
AIC(model1,model2)
```

    ##        df       AIC
    ## model1 16 -272.0994
    ## model2  5 -256.2400

From model summary, AIC and plot comparison of model 1 and model 2,
model 1 is better fitted. Ljung-Box test was conducted to check
autocorrelation of model residuals.

``` r
# Ljung-Box test of model residuals for autocorrelation
Box.test(model1$residuals, type="Ljung-Box", lag=12)
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  model1$residuals
    ## X-squared = 17.21, df = 12, p-value = 0.1419

P-value is 0.1419 which is not statistically significant. We will
determine that mod1 residuals are not autocorrelated.

Then model 1 and model 2 were fitted with a month of delay since the
intervention applied.

``` r
# Fit a model 3 - model 1 with the intervention variables delayed by 1 month
model3 <- lm(rate_ts ~ time + syg.lag1 + time.after.lag1 + month)
summary(model3)
```

    ## 
    ## Call:
    ## lm(formula = rate_ts ~ time + syg.lag1 + time.after.lag1 + month)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.110299 -0.033801  0.000054  0.035107  0.121517 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.5017845  0.0228902  21.921  < 2e-16 ***
    ## time            -0.0003968  0.0003284  -1.208  0.23050    
    ## syg.lag1         0.0471736  0.0253000   1.865  0.06586 .  
    ## time.after.lag1  0.0040275  0.0013838   2.910  0.00466 ** 
    ## monthJan         0.0211940  0.0271600   0.780  0.43747    
    ## monthFeb        -0.0683255  0.0271290  -2.519  0.01375 *  
    ## monthMar        -0.0379735  0.0271044  -1.401  0.16503    
    ## monthApr        -0.0097168  0.0270864  -0.359  0.72073    
    ## monthMay         0.0354226  0.0270748   1.308  0.19446    
    ## monthJun        -0.0038374  0.0270698  -0.142  0.88762    
    ## monthJul         0.0703831  0.0270714   2.600  0.01108 *  
    ## monthAug         0.0143298  0.0270794   0.529  0.59813    
    ## monthSep        -0.0050336  0.0270940  -0.186  0.85308    
    ## monthOct         0.0151599  0.0270474   0.560  0.57669    
    ## monthNov        -0.0319472  0.0270307  -1.182  0.24071    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05405 on 81 degrees of freedom
    ## Multiple R-squared:  0.5418, Adjusted R-squared:  0.4626 
    ## F-statistic: 6.842 on 14 and 81 DF,  p-value: 5.384e-09

``` r
plot(model3)
```

![](submission_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

``` r
# Fit a model 4 - model 2 with the intervention variables delayed by 1 month
model4 <- lm(rate_ts ~ time + syg.lag1 + time.after.lag1)
summary(model4)
```

    ## 
    ## Call:
    ## lm(formula = rate_ts ~ time + syg.lag1 + time.after.lag1)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.170144 -0.038640 -0.002305  0.030320  0.179822 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.4992893  0.0150358  33.207  < 2e-16 ***
    ## time            -0.0003194  0.0003734  -0.855  0.39456    
    ## syg.lag1         0.0378202  0.0285367   1.325  0.18835    
    ## time.after.lag1  0.0043688  0.0015713   2.780  0.00658 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.06177 on 92 degrees of freedom
    ## Multiple R-squared:  0.3203, Adjusted R-squared:  0.2982 
    ## F-statistic: 14.45 on 3 and 92 DF,  p-value: 8.611e-08

``` r
plot(model4)
```

![](submission_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

Again, the model with month dummy variable shows a better fit with a
month lag.

``` r
# Compare model summary
stargazer(model1,model3, 
          type = "text", 
          dep.var.labels = ("Homicide rate"),
          column.labels  = c("Model 1","Model 4"),
          digits = 2, 
          single.row = TRUE)
```

    ## 
    ## ===============================================================
    ##                                      Dependent variable:       
    ##                               ---------------------------------
    ##                                         Homicide rate          
    ##                                   Model 1          Model 4     
    ##                                     (1)              (2)       
    ## ---------------------------------------------------------------
    ## time                          -0.0004 (0.0003) -0.0004 (0.0003)
    ## syg                             0.05* (0.03)                   
    ## time.after                    0.004*** (0.001)                 
    ## syg.lag1                                         0.05* (0.03)  
    ## time.after.lag1                                0.004*** (0.001)
    ## monthJan                        0.02 (0.03)      0.02 (0.03)   
    ## monthFeb                       -0.07** (0.03)   -0.07** (0.03) 
    ## monthMar                        -0.04 (0.03)     -0.04 (0.03)  
    ## monthApr                        -0.01 (0.03)     -0.01 (0.03)  
    ## monthMay                        0.04 (0.03)      0.04 (0.03)   
    ## monthJun                       -0.004 (0.03)    -0.004 (0.03)  
    ## monthJul                       0.07** (0.03)    0.07** (0.03)  
    ## monthAug                        0.01 (0.03)      0.01 (0.03)   
    ## monthSep                        -0.01 (0.03)     -0.01 (0.03)  
    ## monthOct                        0.02 (0.03)      0.02 (0.03)   
    ## monthNov                        -0.03 (0.03)     -0.03 (0.03)  
    ## Constant                       0.50*** (0.02)   0.50*** (0.02) 
    ## ---------------------------------------------------------------
    ## Observations                         96               96       
    ## R2                                  0.54             0.54      
    ## Adjusted R2                         0.46             0.46      
    ## Residual Std. Error (df = 81)       0.05             0.05      
    ## F Statistic (df = 14; 81)         6.84***          6.84***     
    ## ===============================================================
    ## Note:                               *p<0.1; **p<0.05; ***p<0.01

Addition of syg.lag1 and time.after.lag1 doesn’t make significant
difference or improvement of the goodness of model fit. Thus we will
select model 1 which doesn’t include a month lag as a final model.

4)  Using the “best” model from question (3), summarise your findings
    and write a conclusion describing the impact of the law on the
    homicide rate in Florida. Provide effect estimates and relevant
    statistics to support your answer. (20%)

``` r
# Create a dataframe w/o intervention
df1 <- data.frame(step = rep(0, length(rate_ts)), after = rep(0, length(rate_ts)))

# Predict a model
df2 <- predict(model1, newdata = df1)

# Merge with original data
rate_ts.df2 <- cbind(rate_ts, df2)

# Plot both on the same plot
plot(rate_ts.df2,
     plot.type="s",
     main="Monthly time series of homicide rate per one million people in the state of Florida, USA from 2000 to 2007",
     ylab="Homicide rate per 100,000 people",xlab="Month",
     col=c("maroon", "lightblue"),  xaxt="n", cex=.8)

legend("bottomleft", legend=c("Observed","Predicted"),
       col=c("maroon", "lightblue"), lty=c("solid","solid"), cex=.8)

axis(1, time(rate_ts), at=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007),
     labels=c("Jan 2000","Jan 2001","Jan 2002","Jan 2003","Jan 2004","Jan 2005","Jan 2006","Jan 2007"))

abline(v=c(2005,10), col="grey30", lty="longdash")
```

<img src="submission_files/figure-gfm/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

``` r
# Review selected model summary
stargazer(model1, title = "Model 1 output", 
          type = "text",
          no.space = TRUE, 
          ci = TRUE, ci.level = 0.95,
          single.row = TRUE, 
          digits = 2)
```

    ## 
    ## Model 1 output
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                               rate_ts          
    ## -----------------------------------------------
    ## time                 -0.0004 (-0.001, 0.0002)  
    ## syg                    0.05* (-0.002, 0.10)    
    ## time.after            0.004*** (0.001, 0.01)   
    ## monthJan                0.02 (-0.03, 0.07)     
    ## monthFeb              -0.07** (-0.12, -0.02)   
    ## monthMar                -0.04 (-0.09, 0.02)    
    ## monthApr                -0.01 (-0.06, 0.04)    
    ## monthMay                0.04 (-0.02, 0.09)     
    ## monthJun               -0.004 (-0.06, 0.05)    
    ## monthJul                0.07** (0.02, 0.12)    
    ## monthAug                0.01 (-0.04, 0.07)     
    ## monthSep                -0.01 (-0.06, 0.05)    
    ## monthOct                0.02 (-0.04, 0.07)     
    ## monthNov                -0.03 (-0.08, 0.02)    
    ## Constant               0.50*** (0.46, 0.55)    
    ## -----------------------------------------------
    ## Observations                    96             
    ## R2                             0.54            
    ## Adjusted R2                    0.46            
    ## Residual Std. Error       0.05 (df = 81)       
    ## F Statistic            6.84*** (df = 14; 81)   
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

From the selected **Model 1** - hom_rate ~ time + syg + time.after +
month, 95% confidence intervals for `syg` and `time.after` are (-0.002,
0.10) and (0.001, 0.01) respectively which indicates there is little
positive and almost no step change and a little positive slope change
after the intervention. However, as illustrated in seasonal
decomposition plot, the homicide declined drastically in early October
which is before the implementation was applied. It could be some other
event that affected to big decrease or could be an impact on the new law
will come in near future, in addition to the homicide rate in February
is -0.07 people less (ci=-0.12, -0.02) per 100,000 people than other
months. Post implementation shows the homicide rate has a positive trend
of 0.004 people per 100,000 people.

According to the plot and model summary, it shows that the overall
homicide rate has been increased since ‘Stand-your-ground law’ was
implemented in Oct 2005. One of possible reason is that the law provoked
individual’s self-defense before the law enforcement involved so that
led to higher homicide rate in the end.

5)  Including a negative control series is one way of improving causal
    inference from interrupted time series analysis. Give one example of
    a negative control series for this intervention (be specific), and
    justify your selection. Explain in your own words how a negative
    control series helps with inference. (15%)

To estimate effect of Stand-your-ground law to homicide rate, we can
compare a monthly homicide rate of other state in USA which has a
similar population, socio-economic status, age, gender distribution
where similar law hasn’t implemented. We can also consider other crime
rate change in Florida during the same time of this study. If there was
a similarity in the slope and step found in the negative control series,
there might be something else that caused to the change except the
implementation. If there was no difference in negative control series,
it will be a good evidence of casual inference between the
implementation and the homicide rate change.

------------------------------------------------------------------------

### Student declaration

***Instructions: Indicate that you understand and agree with the
following three statements by typing an x in the square brackets below,
e.g. \[x\].***

I declare that this assessment item is my own work, except where
acknowledged, and has not been submitted for academic credit elsewhere
or previously, or produced independently of this course (e.g. for a
third party such as your place of employment) and acknowledge that the
assessor of this item may, for the purpose of assessing this item: (i)
Reproduce this assessment item and provide a copy to another member of
the University; and/or (ii) Communicate a copy of this assessment item
to a plagiarism checking service (which may then retain a copy of the
assessment item on its database for the purpose of future plagiarism
checking).

- [x] I understand and agree

I certify that I have read and understood the University Rules in
respect of Student Academic Misconduct.

- [x] I understand and agree

I have a backup copy of the assessment.

- [x] I understand and agree
