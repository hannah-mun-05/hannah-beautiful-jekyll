Impact of the media attention on the PBS-subsidised dispensing of
combined and progestogen-only oral contraceptives in Austrlia
================
Hannah Mun
27 Nov 2023

- [Introduction](#introduction)
- [Method](#method)
  - [Time series data](#time-series-data)
  - [Fitting a model](#fitting-a-model)
- [Results](#results)
  - [Final model](#final-model)
  - [Counterfactual](#counterfactual)
- [Discussion](#discussion)

# Introduction

Oral contraceptives are a popular form of birth control and are used to
treat other conditions such as endometriosis or polycystic ovary
syndrome. The most common type of oral contraceptive is the combined
pill, which contains a combination of the hormones oestrogen and
progestogen. However, the combined pill increases the risk of blood clot
formation, such as deep vein thrombosis, pulmonary embolism, and stroke.
In contrast, the progestogen-only pill (also called the “mini pill”)
does not increase the risk of blood clots. A paper published in the BMJ
on 26 May 2015 quantified the risk associated with different
oestrogen/progestogen combinations and received substantial media
attention worldwide (Vinogradova et al., 2015).

The media attention surrounding the increased risk of thrombosis
associated with oral contraceptive use has raised concerns about the
safety of the combined pill. This study aims to investigate the impact
of the media attention on the dispensing of combined and
progestogen-only oral contraceptives in Australia. Specifically, we will
use time series data to explore the changes in PBS-subsidised dispensing
of these contraceptives following the publication of the BMJ paper. In
Australia, several combined contraceptive pills containing
levonorgestrel/ethinylestradiol, norethisterone/ethinylestradiol, and
norethisterone/mestranol are subsidised through the PBS. However, other
combined pill formulations such as those containing drosperinone,
cyproterone, and desogestrel are not subsidised and are not captured in
the data. Several progestogen-only mini pills, including levonorgestrel
and norethisterone, are also subsidised. (ChatGPT used to develop text
in this section)

``` r
# Load data
df <- read.csv("Data/contraceptives.csv")

#is.na(df) # no missing value

# Statistical summary 
summary(df)
```

    ##     month              combined          mini      
    ##  Length:48          Min.   :288.8   Min.   :21.00  
    ##  Class :character   1st Qu.:301.8   1st Qu.:22.18  
    ##  Mode  :character   Median :309.2   Median :22.95  
    ##                     Mean   :309.4   Mean   :22.82  
    ##                     3rd Qu.:316.3   3rd Qu.:23.52  
    ##                     Max.   :331.7   Max.   :24.60

Fig 1.1 illustrates monthly counts of PBS subsidised dispensing of
combined pill and mini pill per 10,000 women of reproductive age in
Australia. The data is collected from January 2013 to December 2016.

``` r
# Convert <chr> to <date> format
df$date <- as.Date(x=paste0("01-",df$month), format = "%d-%B-%y")

# Plot raw data
ggplot(df, aes(x=date)) +
  geom_line(aes(y=combined), colour = "#69b3a2", linewidth = 1) +
  geom_line(aes(y=mini*(100/8)), colour = "#b3697a", linewidth = 1) +
  geom_vline(xintercept = as.numeric(as.Date("2015-06-01")), colour = "grey", lty="dashed", lwd=2)+
  theme_ipsum() +
  scale_x_date(date_labels = "%b-%y", date_minor_breaks = "1 month") +
  scale_y_continuous(name ="Combined",limits = c(250,350), sec.axis = sec_axis(trans=~.*0.08, name="Mini")) + 
  theme(
    axis.title.y = element_text(angle = 360, color = "#69b3a2", size = 12, face = "bold"),
    axis.line.y = element_line (color = "#69b3a2"),
    axis.ticks.y = element_line (color = "#69b3a2"),
    axis.text.y = element_text (color = "#69b3a2"),
    axis.title.y.right = element_text(angle = 360,vjust = 1, color = "#b3697a", size = 12, face = "bold"),
    axis.line.y.right = element_line (color = "#b3697a"),
    axis.ticks.y.right = element_line (color = "#b3697a"),
    axis.text.y.right = element_text (color = "#b3697a"),
    plot.title = element_text(size = 12),
    #plot.subtitle = element_text(size = 12, hjust =0.5, size =8, facel = Italic),
    plot.caption = element_text(size = 10, hjust =0.5, face = "bold")
  ) +
  labs(title = "Monthly counts of dispensing combined pills vs. mini pills") +
  xlab("") 
```

<div class="figure" style="text-align: center">

<img src="submission_files/figure-gfm/unnamed-chunk-2-1.png" alt="Fig 1.1 Monthly dispensing of PBS-subsidised combined and mini pills per 10,000 women of reproductive age between January 2013 and December 2016"  />
<p class="caption">
Fig 1.1 Monthly dispensing of PBS-subsidised combined and mini pills per
10,000 women of reproductive age between January 2013 and December 2016
</p>

</div>

During observation time, combined pills are dispensed between 288.8 to
331.7 times, and 21 to 24.6 times of mini pills are dispensed during the
same time. Use of mini pills are clearly increased from 2013 to 2017
whereas combined pills do not show strong trend of increase or decrease.

# Method

## Time series data

From the raw data, we created a time series data starting from January
2013 to December 2016 by monthly basis. We will examine its trend
(Stationarity), autocorrelation and seasonality.

1 Stationarity

As illustrated in fig 1.1 we can see that there is no obvious increase
or decrease trend in combined pills data over the time. In fig 1.3,
stationarity of mini pills are removed in log difference form, however
as our interest is an effect on combined pills, and mini pills are
considered as a control variable in this study, we will use raw form of
data instead of log form. We can also see there is annual cycle in
combined pills in fig 1.2 and fig 1.3 which will be examined for
seasonality.

``` r
# Create time series data
ts_c <- ts(df$combined, start = c(2013,1) , end =c(2016,12) , frequency = 12) #combined only
ts_m <- ts(df$mini, start = c(2013,1) , end =c(2016,12) , frequency = 12) # mini only 
ts_cm <- ts(df[,2:3], start = c(2013,1) , end =c(2016,12) , frequency = 12) # combined and mini
```

![](submission_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->![](submission_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

Also, Augmented Dickey-Fuller (ADF) test is conducted for a statistical
check. Hypothesis of ADF test is H0: time series is non-stationary (p \>
0.05), H1: time series is stationary (p ≤ 0.05)

``` r
# ADF test
adf.test(ts_c, alternative = "stationary")
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ts_c
    ## Dickey-Fuller = -3.6574, Lag order = 3, p-value = 0.03812
    ## alternative hypothesis: stationary

From ADF test, p-value of combined pill is 0.038. Thus, we take the
alternative hypothesis, considering the time series is stationary.

2.  Seasonality

Fig 2.3 shows the counts of combine pills dispensed in each year. There
is a clear seasonal trend, the number of dispensing combine pills is
drastically jumps in January and from December, and drops largely
towards March. Then it starts increase from March to June. Decomposed
seasonal plot fig 2.4 supports yearly trend in the data where the number
of dispense hits the bottom in early year, then increasing through rest
of the year.

``` r
# Seasonal plot
ggseasonplot(ts_c, polar=TRUE) +  ylab("Counts") +
labs(title = "", caption = "Figure 2.3: Polar seasonal plot of monthly counts of combined pills") +
theme(plot.caption = element_text(size = 10, hjust =0.5, face = "bold"))
```

![](submission_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Decompose seasonality
dec_c <- decompose(ts_c)
plot(dec_c)
title(sub ="Fig 2.4 Decomposed seasonal plot of monthly counts of combined pills") 
```

![](submission_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

Below is Kruskall Wallis test result for the seasonality test.
Hypothesis of the test is H0: all months have the same mean, therefore
the data is no seasonality (p \> 0.05), H1: time series has a
seasonality (p ≤ 0.05)

``` r
# Kruskall Wallis test
kw(ts_c, freq = 12, diff = T, residuals = F, autoarima = T)
```

    ## Test used:  Kruskall Wallis 
    ##  
    ## Test statistic:  36.07 
    ## P-value:  0.0001646953

P-value of the test is less than 0.05 which indicates there is a
seasonality in the data.

In Australia, the PBS subsidises medicines, leading to a yearly pattern
in medicine dispensing claims. Once individuals or families hit the
“Safety Net threshold” in a year, their co-payments are reduced,
incentivising them to refill prescriptions more frequently towards
year-end. This results in a surge in prescriptions in December, followed
by a drop in January(Schaffer et al., 2021). The seasonality of combined
pills counts also following similar pattern due to the PBS safety net
threshold system.

3.  Autocorrelation

Following plots are ACF, PACF and CCF against mini pills. ACF and PACF
interpret delayed impact of combine pills only, and we included mini
pills in CCF as an external regression.

``` r
# Visualize ACF, PACF, and CCF
acf(c(ts_c), main = "ACF of combined pills")
title(sub ="Fig 2.5 ACF plot")
```

![](submission_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
pacf(c(ts_c), main = "Partial ACF of combined pills")
#ccf(ts_c, ts_m, main = "Cross correlation between combined pills and mini pills")
title(sub ="Fig 2.6 PACF plot")
```

![](submission_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
print(ccf(c(ts_c), c(ts_m), plot = FALSE))
```

    ## 
    ## Autocorrelations of series 'X', by lag
    ## 
    ##    -13    -12    -11    -10     -9     -8     -7     -6     -5     -4     -3 
    ## -0.094  0.386 -0.007  0.148 -0.035 -0.136  0.041 -0.337 -0.031 -0.167 -0.082 
    ##     -2     -1      0      1      2      3      4      5      6      7      8 
    ##  0.119 -0.374  0.426 -0.252  0.054 -0.170 -0.208  0.014 -0.429 -0.095 -0.385 
    ##      9     10     11     12     13 
    ## -0.170 -0.073 -0.291  0.183 -0.223

``` r
ccf(c(ts_c), c(ts_m), main = "CCF of combined pills against mini pills")
title(sub ="Fig 2.7 Cross correlation plot between combined pills and mini pills")
```

![](submission_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

In fig 2.5 and 2.6, ACF and PACF shows several peaks then drastic drop
in next a few sequence. This probably indicates its seasonality as
discovered above. The cross correlation at lag -12 and 0 is 0.386 and
0.426 respectively. From fig 2.7, most of the time the correlation
between combined pills are mini pills are negative that tells the number
of dispensing combined pills decrease when more mini pills are dispensed
as observed in the raw data as well.

Most of peak values in the plots are out of 95% confidence interval but
very close, we will conduct statistical test to ensure the delayed
impact. Since Ljung-box test can examine one variable at a time, Ljung
and Box Portmanteau test was used as the fucntions of BoxPierce and
LjungBox are more accurate than Box.test function and can be used in the
univariate or multivariate time series at vector of different lag values
as well as they can be applied on an output object from a fitted model
described in the description of the function BoxPierce. Hypotheses of
the test are, H0: there is no auto/ cross correlation in time series (p
\> 0.05) and H1: there is an auto/cross correlation in time series (p ≤
0.05).

``` r
# Ljung and Box Portmanteau test
LjungBox(ts_cm, lags = 10, sqrd.res = TRUE )
```

    ##  lags statistic df      p-value
    ##    10  125.2702 40 1.012246e-10

P-value of the test less than 0.05, therefore we reject Null hypothesis
and consider that there is a delayed impact in the time series.

## Fitting a model

We’ve determined stationarity, seasonality, and autocorrelation of the
data. While segmented regression can be applied to multivariate data,
our data does not meet the autocorrelation assumption, which requires
the data to be uncorrelated. When comparing exponential smoothing models
with ARIMA, the former are based on a description of the trend and
seasonality of data, while the other aims to describe autocorrelation.
In considering ARIMAX versus VARX, ARIMAX is more appropriate for
examining a single variable of interest and an external variable that
may influence it. On the other hand, VARX is typically used for multiple
interdependent variables that may influence each other. Given that our
study focuses on combined pills, with mini pills considered as an
external influence, ARIMAX is more suitable than VARX. However, in
ARIMAX, the exogenous variables are treated as additional input series
that directly impact the output series, but are not modeled themselves.
Therefore, ARIMA is more suitable than ARIMAX for our purposes.
Therefore, in this study, we will analyse the impact of media attention
on combined pills using the ARIMA model.

Initially, we create 2 vectors to measure the impact of media attention
on mini pills, with the last week of May 2015 marking the peak of this
attention. We designate June 2015 as the boundary for this impact,
meaning we compare the impact of media attention before and after June
2015. ‘step’ variable has a value of 0 for the period prior to June
2015, and a value of 1 for the period after, representing the
intervention, which in this study is media attention. ‘ramp’ variable
represents changes in the slop. Along with the ‘mini’ variable from the
raw data, the three varaiable will be used as external factors on the
dependent variable, which in this case are the combined pills.

``` r
# Create a variable representing the peak of media attention
step <- as.numeric(as.yearmon(time(ts_cm))>='June 2015')

# Create a variable representing ramp (change in slope) and view
ramp <- append(rep(0,sum(as.yearmon(time(ts_cm))<'June 2015')),
              seq(1,sum(as.yearmon(time(ts_cm))>='June 2015'))) %>%
       ts(start=c(2013,01),frequency=12)

# Control variable
mini <- df$mini

# Combine the interruption variable and the time trend variable to create xreg
xreg <- cbind(mini, step, ramp)
```

In order to determine potential AR /MA (p,d,q)(P,D,Q) orders, below
table is referred to select most appropriate autoregressive (p) and
moving average (q) terms from autocorrelation and partial
autocorrelation.

<figure>
<img src="table.png"
alt="Source: Interrupted time series analysis using autoregressive integrated moving average (ARIMA) models: a guide for evaluating large-scale health interventions" />
<figcaption aria-hidden="true">Source: Interrupted time series analysis
using autoregressive integrated moving average (ARIMA) models: a guide
for evaluating large-scale health interventions</figcaption>
</figure>

ACF and PACF plot in fig 2.5 and 2.6 does not fit well criteria in the
table as both do not have a significant tail off, or cut off lag at one
point. We will try auto.arima() function which can decide the best model
for the data.

``` r
# Fit the ARIMA model with auto-fitted
mod1 <- auto.arima(ts_c, xreg = xreg)

# Fit the ARIMA model with seasonality forced
mod2 <- auto.arima(ts_c, xreg = xreg, D=1, seasonal = T)

AIC(mod1, mod2)
```

    ##      df     AIC
    ## mod1  6 211.303
    ## mod2  6 211.303

AIC of auto-fitted model `mod1` and seasonality forced model `mod2` has
no difference, thus a simple model `mod1` is preferred. Residuals of
`mod1` illustrated in fig 2.8.

``` r
checkresiduals(mod1)
```

<figure>
<img src="submission_files/figure-gfm/unnamed-chunk-12-1.png"
alt="Fig 2.8 Time Series graph of the residuals, ACF Graph for the lags and Distribution of the residuals of mod1" />
<figcaption aria-hidden="true">Fig 2.8 Time Series graph of the
residuals, ACF Graph for the lags and Distribution of the residuals of
mod1</figcaption>
</figure>

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Regression with ARIMA(0,0,0)(1,1,0)[12] errors
    ## Q* = 7.9954, df = 9, p-value = 0.5346
    ## 
    ## Model df: 1.   Total lags used: 10

Residuals of `mod1` is has no trend, with all values are sitting within
95% confidence interval in ACF plot, and p-value of Ljung-Box test is
0.53 indicating no autocorrelation. Next, we fit another model `mod3`
with mini and slope variables as an external regressors.

``` r
# Fit a model including external regressor slop and mini variables only
mod3 <- auto.arima(ts_c, xreg = xreg[,1:2], D=1)
AIC(mod1,mod3)
```

    ## Warning in AIC.default(mod1, mod3): models are not all fitted to the same
    ## number of observations

    ##      df      AIC
    ## mod1  6 211.3030
    ## mod3  5 213.2635

``` r
checkresiduals(mod3)
```

<figure>
<img src="submission_files/figure-gfm/unnamed-chunk-14-1.png"
alt="Fig 2.9 Time Series graph of the residuals, ACF Graph for the lags and Distribution of the residuals of mod3" />
<figcaption aria-hidden="true">Fig 2.9 Time Series graph of the
residuals, ACF Graph for the lags and Distribution of the residuals of
mod3</figcaption>
</figure>

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Regression with ARIMA(0,1,1)(1,1,0)[12] errors
    ## Q* = 13.365, df = 8, p-value = 0.09988
    ## 
    ## Model df: 2.   Total lags used: 10

`mod3` did not improve a model fit, therefore we choose the simpler
model `mod1` as a final model.

------------------------------------------------------------------------

# Results

## Final model

``` r
# Final model summary and confidence intervals
summary(mod1)
```

    ## Series: ts_c 
    ## Regression with ARIMA(0,0,0)(1,1,0)[12] errors 
    ## 
    ## Coefficients:
    ##          sar1   drift    mini      step     ramp
    ##       -0.6886  0.2983  5.3458  -12.8968  -0.6121
    ## s.e.   0.1220  0.0646  0.6812    1.8992   0.1313
    ## 
    ## sigma^2 = 13.94:  log likelihood = -99.65
    ## AIC=211.3   AICc=214.2   BIC=220.8
    ## 
    ## Training set error measures:
    ##                     ME     RMSE     MAE        MPE      MAPE     MASE
    ## Training set 0.1290959 3.000215 2.17774 0.03340411 0.7047107 0.295398
    ##                    ACF1
    ## Training set -0.1182088

``` r
confint(mod1)
```

    ##             2.5 %     97.5 %
    ## sar1   -0.9276467 -0.4495172
    ## drift   0.1716407  0.4248968
    ## mini    4.0107408  6.6808544
    ## step  -16.6192297 -9.1744290
    ## ramp   -0.8694549 -0.3547288

Final model is ARIMA(0,0,0)(1,1,0)\[12\]. The model is a seasonal
autoregressive integrated moving average model with no autoregressive
terms (p=0), no differencing (d=0), and no moving average terms (q=0) in
the non-seasonal part of the model. The seasonal part of the model has
one autoregressive term (P=1), one order of differencing (D=1), and no
moving average terms (Q=0), with a seasonal period of 12. The estimated
PBS subsidised mini pills is 5.31 dispensing (95% CI 4.01,6.68 ), step
change is -12.90 dispensings (95% CI -16.62,-9.17) and ramp (changes in
the slope) change is -0.61 (95% CI -0.87,-0.35) per month. The variance
of the residuals is 13.94 with AIC 211.3. In regards to the error terms,
RMSE is 3 that the difference of predicted and observed value is 3, and
ACF1 is -0.12 which indicates there is a sligh negative autocorrelarion
existing in the residuals.

## Counterfactual

Fig 3.1 shows an observed and predicted values of PBS dispensed combined
pills without media attention on mini pills based on ARIMA model. We can
see that the number of dispensed combines pills are reduced by media
attention on the risk associated with combined pills.

``` r
# Create a data without media impact
mod_nomedia <- Arima(window(ts_c, end=c(2015,6)), order=c(0,0,0), seasonal=list(order=c(1,1,0), period=12))

# Forecast 12 months after peak of media attention
predict <- forecast(mod_nomedia, h=12)
predict.ts <- ts(as.numeric(df$combined), start=c(2015,6), end = c(2016,12), frequency=12)

# Combine with observed data
newdf <- ts.union(ts_c, predict.ts)
```

``` r
# Plot
plot(newdf, 
     type="l", 
     plot.type="s", 
     col=c("#69b3a2","#b3697a"), 
     xaxt="n",cex=.8,
     ylab="Combined pills (counts)")
legend("bottomleft", legend=c("Observed","Predicted"),
        col=c("#69b3a2","#b3697a"), lty=c("solid","dashed"), cex=.8)   
axis(1, time(ts_c), at=c(2013, 2014, 2015, 2016, 2017),
     labels=c("Jan 2013","Jan 2014","Jan 2015","Jan 2016","Jan 2017"))
abline(v = c(2015.4), lty="dashed", col="gray", lwd =2)
```

<figure>
<img src="submission_files/figure-gfm/unnamed-chunk-17-1.png"
alt="Fig 3.1 Observed and predicted values of PBS dispesned combined pills without media attention on mini pills based on ARIMA model" />
<figcaption aria-hidden="true">Fig 3.1 Observed and predicted values of
PBS dispesned combined pills without media attention on mini pills based
on ARIMA model</figcaption>
</figure>

------------------------------------------------------------------------

# Discussion

In this study, the impact of media attention on the use of combined
contraceptives and progestogen-only oral contraceptives, commonly known
as mini pills was analysed. The media highlighted the potential risks
associated with the use of combined oral contraceptives. Our study aimed
to quantify the effect of this media attention on the dispensing of
these contraceptives in Australia. The data analysis approach involves
determining stationarity, seasonality, and autocorrelation of the data.
Given that the study focuses on combined pills, with mini pills
considered as an external influence, we have chosen to use the ARIMA
model for analysis. Specifically, we observed a decrease in the
dispensing of PBS-subsidised combined pills and an increase in the
dispensing of mini pills. This trend suggests that the media attention
may have influenced public perception and use of these contraceptives,
leading to a preference for mini pills due to their lower associated
risk of venous thromboembolism. These findings underscore the
significant role that media attention plays in shaping public health
behaviors and decisions. Further research is needed to explore this
relationship in more detail and to develop strategies for ensuring
accurate and balanced information is dispersed to the public.

------------------------------------------------------------------------

#### References

Australia, H. (2019, March 13). The pill (combined oral contraceptive
pill). Www.healthdirect.gov.au.
<https://www.healthdirect.gov.au/the-pill-combined-oral-contraceptive-pill>
Package “seastests.” (2022).
<https://cran.r-project.org/web/packages/seastests/seastests.pdf>
Science, B. (2020, June 17). Time Series in 5-Minutes, Part 2:
Autocorrelation and Cross Correlation \| R-bloggers.
<https://www.r-bloggers.com/2020/06/time-series-in-5-minutes-part-2-autocorrelation-and-cross-correlation/>
plot_acf_diagnostics: Visualize the ACF, PACF, and CCFs for One or More
Time Series in timetk: A Tool Kit for Working with Time Series. (n.d.).
Rdrr.io. Retrieved November 26, 2023, from
<https://rdrr.io/cran/timetk/man/plot_acf_diagnostics.html> 2.8
Autocorrelation \| Forecasting: Principles and Practice. (n.d.). In
otexts.com. <https://otexts.com/fpp2/autocorrelation.html> jung, G.M.
and Box, G.E.P (1978). “On a Measure of Lack of Fit in Time Series
Models”. Biometrika, 65, 297-303. LjungBox function - RDocumentation.
(n.d.). Www.rdocumentation.org. Retrieved November 26, 2023, from
<https://www.rdocumentation.org/packages/portes/versions/6.0/topics/LjungBox>
Schaffer, A.L., Dobbins, T.A. & Pearson, SA. Interrupted time series
analysis using autoregressive integrated moving average (ARIMA) models:
a guide for evaluating large-scale health interventions. BMC Med Res
Methodol 21, 58 (2021). <https://doi.org/10.1186/s12874-021-01235-8>
Hyndman, Rob & Kostenko, Andrey. (2007). Minimum Sample Size
Requirements for Seasonal Forecasting Models. Foresight: The
International Journal of Applied Forecasting. 6. 12-15. The ARIMAX model
muddle \| Rob J Hyndman. (n.d.). Robjhyndman.com.
<https://robjhyndman.com/hyndsight/arimax/>

------------------------------------------------------------------------

#### Student declaration

All assessments should include a copy of the student declaration
regarding plagiarism, student academic misconduct and proper back-up of
your assessment. The text of the declaration is provided below. You
should copy this into your report and tick the boxes to indicate your
agreement with the statements.

> I declare that this assessment item is my own work, except where
> acknowledged, and has not been submitted for academic credit elsewhere
> or previously, or produced independently of this course (e.g. for a
> third party such as your place of employment) and acknowledge that the
> assessor of this item may, for the purpose of assessing this item: (i)
> Reproduce this assessment item and provide a copy to another member of
> the University; and/or (ii) Communicate a copy of this assessment item
> to a plagiarism checking service (which may then retain a copy of the
> assessment item on its database for the purpose of future plagiarism
> checking).  
> - \[x\] I understand and agree I certify that I have read and
> understood the University Rules in respect of Student Academic
> Misconduct.  
> - \[x\] I understand and agree I have a backup copy of the
> assessment.  
> - \[x\] I understand and agree
