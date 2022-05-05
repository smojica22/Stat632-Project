```{r}

library(readr)
health<- read_csv("HealthyCities - Sheet1-2.csv")
head(health)

```



```{r}
# scatter plot with marginal histograms there is strong right skew in some variables might suggest log transformation 
ggplot(health, aes( Sunshine + Cost_water + Obesity + Pollution + Hours_worked + Happiness + Outdoor + Take_out + Gym , Life_expectancy)) +
  geom_point() +
  stat_smooth(method="lm")

par(mfrow=c(1,2))
hist(health$Life_expectancy)
hist(health$Sunshine)
hist(health$Cost_water)
hist(health$Obesity)
hist(health$Pollution)
hist(health$Hours_worked)
hist(health$Happiness)
hist(health$Outdoor)
hist(health$Take_out)
hist(health$Gym)

```


```{r}
# scatterplots
pairs(Life_expectancy ~ Sunshine + Cost_water + Obesity + Pollution + Hours_worked + Happiness + Outdoor + Take_out + Gym , data=health)

lmfull <- lm(Life_expectancy ~ Sunshine + Cost_water + Obesity + Pollution + Hours_worked + Happiness + Outdoor + Take_out + Gym, data=health)
summary(lmfull)
anova(lmfull)

# diagnostic plots to check assumptions
par(mfrow=c(1,2), mar=c(4.5, 4.5, 2, 2))
plot(predict(lmfull), rstandard(lmfull), xlab="Fitted Values", ylab="Standarized Residuals")
abline(h=0)
qqnorm(rstandard(lmfull))
qqline(rstandard(lmfull))
qqnorm(resid(lmfull))
qqline(resid(lmfull))
hist(resid(lmfull))

```

```{r}

# check for evidence of relationship between health and at least one predictor variable and write hypothesis
nullLife <- lm(Life_expectancy ~ 1 , data=health)
anova(nullLife, lmfull)

#  step function to determine the best model according to the AIC, perform partial F-test and check assumptions and is model useful
health <-na.omit(health)
lm_step1 <- step(lmfull)
anova(lm_step1, lmfull)
performance:: check_model(lm_step1)
summary(lm_step1)
# with $R^2=-0.5478$ 54.78% of lifespan is explained by our model. 

```


```{r}

# starting with happiness  as response
ggplot(health, aes( Sunshine + Cost_water + Obesity + Pollution + Hours_worked + Life_expectancy + Outdoor + Take_out + Gym , Happiness)) +
  geom_point() +
  stat_smooth(method="lm")

pairs(Happiness ~ Sunshine + Cost_water + Obesity + Pollution + Hours_worked + Life_expectancy + Outdoor + Take_out + Gym , data=health)

happy <- lm(Happiness ~ Sunshine + Cost_water + Obesity + Pollution + Hours_worked + Life_expectancy + Outdoor + Take_out + Gym, data=health)
summary(happy)
anova(happy)

# check for evidence of relationship between health and at least one predictor variable and write hypothesis 
nullh <- lm(Happiness ~ 1 , data=health)
anova(nullh, happy)

par(mfrow=c(1,2), mar=c(4.5, 4.5, 2, 2))
plot(predict(happy), rstandard(happy), xlab="Fitted Values", ylab="Standarized Residuals")
abline(h=0)
qqnorm(rstandard(happy))
qqline(rstandard(happy))
qqnorm(resid(happy))
qqline(resid(happy))
hist(resid(happy))


```


```{r}
step_hap <- step(happy)
anova(step_hap, happy)
performance:: check_model(step_hap)
summary(step_hap)

# predicting hayward
newdata <- data.frame(Cost_water=1.11, Obesity=36.2, Life_expectancy=78.8, Pollution=39.9)
predict(step_hap, newdata, type='response')

#We get a happiness score of 6.981113 for hayward

```

