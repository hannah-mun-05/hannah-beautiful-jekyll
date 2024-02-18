---
layout: post
title: Linear model data analysis
thumbnail-img: /assets/img/thumb.png
share-img: /assets/img/path.jpg
author: Hannah Mun
---

## Exploratory data analysis 

##### Accessing the data

Let's have a look at raw data of `kidiq` data set.
```{r}
glimpse(kidiq)

# summary of statistics
summary(kidiq)
```
Looking the raw data, there are 4 variables and 434 samples collected. Summary of collected data gives a brief idea of range of sample values.

Let's have a look correlation of each variables. 
``` {r}
# correlation between variables
cor(kidiq)
```
According to calculated correlation score, child IQ and mom IQ has the highest correlation (0.45), following mom's high school status (0.24) and mom's age (0.09). 

##### Visualizing the data

Before visualizing the data, we can check if there is any Null value in the data set.
```{r}
# check for missing data before visualization
colMeans(is.na(kidiq))*100
```

There is no Null value in the data.

Below are histograms of each variable to display its data distribution.
```{r echo=FALSE, warning=FALSE, message=FALSE}
# histogram of each variable
mom_hs <- ggplot(data=kidiq, aes(mom_hs)) +
  geom_bar(color ="blue", fill = "lightblue", binwidth=1) +
  labs(x = 'Mom HS') 
mom_age <- ggplot(data=kidiq, aes(mom_age)) +
  geom_histogram(color ="blue", fill = "lightblue", binwidth=1) +
  labs(x = 'Mom age') 
mom_iq <- ggplot(data=kidiq, aes(mom_iq)) +
  geom_histogram(color ="blue", fill = "lightblue", binwidth=1) +
  labs(x = 'Mom IQ') 
kid_score <- ggplot(data=kidiq, aes(kid_score)) +
  geom_histogram(color ="blue", fill = "lightblue") +
  labs(x = 'Child IQ') 

grid.arrange(mom_age, mom_iq, mom_hs, kid_score, nrow = 2, ncol = 2)
```
We can see that distribution of mom's age has closed to normal distribution. Its median (23.00) and mean (22.79) value is almost same in statistics summary as well. Mom IQ has slightly right-skewed distribution and child IQ has a slight left-skewed distribution. Mom's high school graduation is binary value, (0 indicating no high school graduation and 1 indicating high school graduation) which more than 3/4 are high school graduates and less than 1/4 are not.

We can also check a bivariate distribution, below plots depict a relationship of each variable and child IQ.
```{r echo=FALSE, warning=FALSE, message=FALSE}
# relationship of the outcome variable(kid_score) with other two explanatory variables 
r1 <- ggplot(kidiq, aes(x = mom_hs, y = kid_score)) +
  geom_point() +
  labs(x = "Mom's high school status", y = "Child IQ", 
       title = "Relationship between mom's high school status and child IQ") +
  geom_smooth(method = "lm", se = FALSE)

r2 <- ggplot(kidiq, aes(x = mom_iq, y = kid_score)) +
  geom_point() +
  labs(x = "Mom IQ", y = "Child IQ", 
       title = "Relationship between mom IQ and child IQ") +
  geom_smooth(method = "lm", se = FALSE)
  
r3 <- ggplot(kidiq, aes(x = mom_age, y = kid_score)) +
  geom_point() +
  labs(x = "Mom age", y = "Child IQ", 
       title = "Relationship between mom's age and child IQ") +
  geom_smooth(method = "lm", se = FALSE)

grid.arrange(r1, r2, r3, nrow = 2, ncol = 2)
```
There is a positive relationship between Mom IQ and child IQ. Relationship between child IQ and mom's age and high school status doesn't show strong relationship as much as mom IQ and child IQ, however it still indicates a slight positive relationship. Average child IQ whose mom has a high school graduation is higher than the child whose mom doesn't have a high school graduation, similar to mom's age and child IQ.




##### Fitting a model

`kidiq` data is fitted to a linear model, the model named `kidiqmod`. Below is a model summary of `kidiqmod`.
```{r}
# fit a linear model 
kidiqmod <- lm(kid_score ~ mom_age + mom_iq + mom_hs, data=kidiq)

# model summary
summary(kidiqmod)
```
Residuals are difference between an actual value and predicted value. Median of residuals is 2.4, larger than mean of 0 therefore we can image the model would have a left-skewed distribution.

P-value of `mom_iq` is statistically significant (p < 0.05, also asterisk is another indicator of significance. the more asterisk tells the higher significance). Estimate coefficient of `mom_iq` is 0.56 which means when mom IQ increases 1, child IQ also increases 0.56 times. Intercept is 20.98, which implies when mom's age, IQ is zero and mom has no high school status, child IQ is about 21. It is not applicable scenario in real anyway. Also when mom's high school status is 1, it increases child IQ about 5.6. P- value of intercept and mom's high school status indicates that these variables have less significance compared to mom IQ.

Adjusted R-squared also tells how well the model is fitting the data. There is 21% of variation of mom's age, IQ and high school status variables to outcome, child IQ. Adjusted R-squared value can be increased by additional predictors, even if they aren’t actually related to outcome variable.

F-statistics and P-value returns a hypothesis test result. Based on F-test result and P-value, we reject Null hypothesis ( there is no relationship between mom's age,IQ and high school degree status and child IQ).

All of above component in the model summary can be misleading by itself, so we shouldn't rely on result of single component to interpret the model.

##### Model prediction on child IQ whose mom has average age, IQ and highschool degree.

Predicted child IQ is 87 when mom has average age, IQ and high school degree.
```{r echo= TRUE, warning=FALSE, message=FALSE}
# get all predictors as a matrix
kidiq_matrix <- model.matrix(kidiqmod)

# calculate average maternal age, average IQ and a high school degree
mom_average <- apply(kidiq_matrix, 2, median)

# predict child's IQ on mother had average maternal age, average IQ and a high school degree
predict(kidiqmod, new=as.data.frame(t(mom_average)))
```

Confidence interval and prediction interval of child IQ are calculated as below.
```{r echo=FALSE, warning=FALSE, message=FALSE}
# confidence interval
predict(kidiqmod, new=as.data.frame(t(mom_average)), interval="confidence")

# prediction Interval
predict(kidiqmod, new=as.data.frame(t(mom_average)), interval="prediction")
```

##### Model residuals 

Residuals are used to assess aspects of veracity of a linear regression model, by testing that the attributes meet the requirements for linear regression.

```{r echo=FALSE, warning=FALSE, message=FALSE}
# predicted kid score
p_kid_score <- predict(kidiqmod)

# residuals of predicted kid score
res_kid_score <- kidiq$kid_score - p_kid_score

# histogram of residuals
res_his <- ggplot(data=kidiq, aes(x=res_kid_score)) +
            geom_histogram(fill='lightblue', color='darkblue') +
            labs(x='Estimated Residuals', title='Distribution of Residuals') +
            theme(plot.title = element_text(hjust = 0.5))

# Residuals vs Fitted value
res_fit <- ggplot(data = kidiqmod, aes(x =p_kid_score, y = res_kid_score)) +
            geom_point(color ='grey60') +
            geom_hline(yintercept =0, color ='red') +
            labs(x='Fitted value', y='Residuals', title='Residuals vs Fitted value') +
            theme(plot.title = element_text(hjust = 0.5))

grid.arrange(res_his, res_fit, nrow =2, ncol = 1)
```

The distribution of the residuals are a little right skewed but still much closed to a normal distribution. Also residuals and fitted values are randomly distributed in the plot.
```{r echo=FALSE, warning=FALSE, message=FALSE}
par(mfrow = c(2,1))

# Q-Q Plot
qq <- plot(kidiqmod, which=2, col=c('grey30') )

# Scale-Location Plot
scale_loc <- plot(kidiqmod, which=3, col=c('grey30'))
```

Q-Q plot shows pretty good alignment to the the line with a few offset. Scale-location plot is also used to test the residuals have equal variance along the red regression line. We can see that the residuals are reasonably spread based on the line.

4. What is the predicted IQ range for a specific child whose mother completed high school, has an IQ of 110 and was 27 years old at the time of birth? What is the predicted IQ range for the average child with these characteristics? Explain the difference in your own words. (20%) 
```{r echo=TRUE, message=FALSE}
# create a data frame that mother completed high school, has an IQ of 110 and was 27 years old 
df <- data.frame(mom_hs =1, mom_iq = 110, mom_age = 27)

# add a new variable 'p' to kidiq and store new value
kidiq$p <- predict(kidiqmod)

# predicted child IQ range
predict(kidiqmod, newdata =df, interval ='prediction')

```
The range of predicted child IQ is between 59 to 130. We can be 95% confident that the next sample will fall within this range. 

5. Refit the model using the first 300 records and use this model to predict outcomes in the remaining sample. How do the predictions compare to the known outcomes? (20%) 

##### Split data into train and test set and refit the model

`kidiq` data is split into 2 data set called `train` and `test` data set. `train` data includes the first 300 rows of the original data and `test` data includes the rest of the rows.
Below is a model summary of `train` data.
```{r}
# split `kidiq` data into two - train(first 300 rows) and test(rest of the rows)
train <- kidiq[1:300,]
test <- kidiq[301:434 ,]

# refit the model using the first 300 rows
kidiqmod2 <- lm(kid_score ~ ., data = train)

summary(kidiqmod2)
```

Estimate coefficients of `mom_iq` of the refitted model is 0.59 which is higher than original model 0.56. Intercept of refitted model is 25 whereas original model was 21.

##### Making a prediction on test data
```{r}
# make predictions on test data
p2 <- predict(kidiqmod2, newdata = test)
head(p2)
```

Above is a first 5 data of predicted child's IQ in refitted model, and below is an actual value in the data set.

```{r}
head(test$kid_score)
```

##### Model performance comparison

Below is adjusted R-squared, Residual standard error (sigma) and P-value of `kidiqmod`.
```{r}
# metrics for model kidiqmod
glance(kidiqmod) %>%
  dplyr::select(adj.r.squared, sigma, p.value)
```
Let's look at adjusted R-squared, Residual standard error (sigma) and P-value of refitted model `kidiqmod2`.

```{r}
# metrics for model kidiqmod2
glance(kidiqmod2) %>%
  dplyr::select(adj.r.squared, sigma, p.value)
```

Adjusted R-squared of refitted model is higher than initial model,and RSE of refitted model is lower than the original model. Therefore the refitted model has a better fit to explain relationship between mom's age, IQ and high school status and child IQ. However p-value of the original model is lower than refitted model, because refitted model has less sample size than original model. Large sample size tends to improve p-value.

```{r echo=TRUE, message=FALSE}
# calculate RMSE of original model
sigma(kidiqmod)/mean(kidiq$kid_score)
# calculate RMSE of refitted model
sigma(kidiqmod2)/mean(kidiq$kid_score)
```
We can also tell that the prediction error of refitted model (20.3%) is lower than original model (20.9%).


-- End of document --
