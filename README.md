# Missing-Data-R
Missing Data and how to address it with imputation in R studio

---
title: "HW 03 - Missing Data Assignment"
author: "Kyle Barisone"
output:
  pdf_document: default
  html_document: 
    highlight: pygments
    theme: readable
    toc: yes
    toc_float: yes
---

```{r, include=FALSE, warning=FALSE, message=FALSE}
lib <- c("VIM",  "dplyr", "mice", "missForest", "ggplot2", "gridExtra", "knitr", "scales", "lattice", "forestplot", "kableExtra","pander")
invisible(lapply(lib, library, character.only=T))
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
theme_set(theme_bw())
scale_fill_discrete <- scale_fill_viridis_d
```

# Understanding impact of missing data on parameter estimates. 
 
### 1. Simulate and describe the following population distributions. 

```{r}
set.seed(8675309)
N=1000

# sample data from known/named distributions. 
norm.pop.data <- data.frame(x=rnorm(n=N, mean=10, sd=2), dist =
  "Normal") 
chi2.pop.data <- data.frame(x=rchisq(N,10), dist = "X2")
unif.pop.data <- data.frame(x=runif(N, 0,20 ), dist = "Uniform")

# combine into one data set
pop.data <- rbind(norm.pop.data, chi2.pop.data, unif.pop.data)

# calculate grouped summary stats
pop.ss <- pop.data %>% group_by(dist) %>% 
                    summarise(mean=mean(x), var=var(x), 
                              min=min(x), max=max(x))
kable(pop.ss, digits=3) %>% kable_styling(full_width=FALSE)

# plot pop data - one panel per distribution. 
ggplot(pop.data, aes(x=x, fill=dist)) + geom_histogram()  + 
      geom_vline(data=pop.ss, aes(xintercept=mean), col="darkgreen") +
      geom_text(data=pop.ss, parse=TRUE, hjust=-.5,
                aes(y = 200, x=mean, label= paste("mu == ", round(mean,3)))) + 
      facet_wrap(~dist, ncol=1) + scale_fill_discrete(guide=FALSE)

```

The normal distribution has a mean of 9.94 and a variance of 4.18. The data ranges from 3.1 to 16.83 and resembles the shape of a normal distribution.

The chi square distribution has a mean of 10.01 and a variance of 20.29. The data ranges from .64 to 31.22 and is properly skewed to the right. 

The uniform distribution has a mean of 9.86 and a variance of 32.40. The data ranges from .04 to 20.0 and resembles a uniform distribution. 

### 2. Set p=20% of values missing completely at random (MCAR), then compare the observed distribution to the population. 

```{r, echo=FALSE}
# don't change this function, and don't set a seed.
create.MCAR <- function(dta, pmiss){ 
    N <- length(dta)
    set.missing.idx <- sample(1:N, size=pmiss*N, replace=FALSE)
    observed.data <- dta[-set.missing.idx] 
    return(observed.data)
}
```

Set values to missing
```{r}
# pull sample of n=40
s=40
set.seed(8675309)
norm.samp <- sample(norm.pop.data$x, size=s, replace=TRUE)
chi2.samp <- sample(chi2.pop.data$x, size=s, replace=TRUE)
unif.samp <- sample(unif.pop.data$x, size=s, replace=TRUE)

# Create missing (make sure to match the distribution names exactly)
obs.norm <- data.frame(x=create.MCAR(dta=norm.samp, pmiss=.2), dist="Normal")
obs.chi2 <- data.frame(x=create.MCAR(dta=chi2.samp, pmiss=.2), dist="X2")
obs.unif <- data.frame(x=create.MCAR(dta=unif.samp, pmiss=.2), dist="Uniform")

# combine into one data set
observed <- rbind(obs.norm, obs.chi2, obs.unif)

# plot population distribution, with sample points added on in black. 
ggplot(pop.data, aes(x=x, fill=dist)) + geom_histogram()  + 
      facet_wrap(~dist, ncol=1) + scale_fill_discrete(guide=FALSE)  + 
      geom_dotplot(data=observed, aes(x=x), dotsize=.5, fill="black")

```

The sample distribution resembles the normal population extremely well, however some bins dont have any data in them while the uniform distribution seems slightly less uniform when observing the sample data. The chi sqare sample remains right skewed but the sample data seems slightly more right skewed. 

### 3. How does missing data affect inference? How much bias is there? Does the shape of the distribution matter? 

```{r}
# calculate grouped summary stats & approx 95% CI's
mcar.ci <- observed %>% group_by(dist) %>% 
                    summarise(xbar=mean(x), 
                              lower = xbar-2*sd(x)/sqrt(n()), 
                              upper = xbar+2*sd(x)/sqrt(n()))

# take the true mean and distribution name from the population summary stats
compare.mcar <- pop.ss %>% select(dist, mean) %>% 
# then left join on the sample CI from the mcar observed data
                    left_join(mcar.ci) %>% 
# calculate the bias, and a logical indicator for if the CI covers the mean. 
                    mutate(abs.bias = xbar-mean, 
                           pct.bias = (abs.bias/mean)*100, 
                           cover = ((mean < upper) & (mean > lower)))

# pretty display
kable(compare.mcar, digits=4) %>% kable_styling(full_width=FALSE)

```

The table above tells us that the confidence interval for the 3 sample distribution cover the population parameter. The means seem to stay fairly accurate when comparing the missing data to the complete data. The percentage bias was .75% for the normal distribution, .64% for the chi sqare, and 4.1% for the uniform which are all relatively low. In addition, for all 3 cases the spread of the data when it is changed to missing is drastically more narrow.

### 4. How does the % missing change the above results? 

```{r}
N=1000
s=100
set.seed(1067)

chi2.pop <- data.frame(x=rchisq(N,10), dist = "X2") 
mu = mean(chi2.pop$x)
chi2.0   <- data.frame(x=sample(chi2.pop$x, size=s, replace=TRUE), dist="0%")

# Create missing
chi2.20 <- data.frame(x=create.MCAR(dta=chi2.0$x, pmiss=.2), dist="20%")
chi2.40 <- data.frame(x=create.MCAR(dta=chi2.0$x, pmiss=.4), dist="40%")
chi2.60 <- data.frame(x=create.MCAR(dta=chi2.0$x, pmiss=.6), dist="60%")
chi2.80 <- data.frame(x=create.MCAR(dta=chi2.0$x, pmiss=.8), dist="80%")

chi2.obs <- rbind(chi2.0, chi2.20, chi2.40, chi2.60, chi2.80)

chi2.mcar.ci <- chi2.obs %>% group_by(dist) %>% 
                              summarise(xbar=mean(x), 
                              lower = xbar-2*sd(x)/sqrt(n()), 
                              upper = xbar+2*sd(x)/sqrt(n()))

chi2.mcar.ci %>% mutate(abs.bias = xbar-mu, 
                        pct.bias = (abs.bias/mu)*100, 
                        cover = ((mu < upper) & (mu > lower))) %>% 
  kable(digits=4) %>% kable_styling(full_width=FALSE)

ggplot(chi2.pop, aes(y=x, x=0)) + 
        # pop stuff first
        geom_violin(fill="grey30") +
        geom_point(aes(x=0, y=mu), pch=17, size=3, col="red") + 
        geom_hline(yintercept=mu) + 
        # now samples
        geom_jitter(data=chi2.obs, aes(y=x, x=as.numeric(dist)), width=.08) +
        geom_violin(data=chi2.obs, aes(y=x, x=as.numeric(dist), fill=dist), alpha=.3) + 
        stat_summary(data=chi2.obs, aes(group=dist, x=as.numeric(dist)),
                     fun.y = 'mean', geom='point', pch=17, size=3, col="red") +
        geom_errorbar(data=chi2.obs, aes(y=x, x=as.numeric(dist)), 
                      stat="summary", fun.data="mean_se", col="red",
                      fun.args = list(mult = 2), width=0.3, size=1) + 
        scale_x_continuous(breaks=c(0:5), labels=c("Pop", levels(chi2.obs$dist)))  +
        scale_color_viridis_d(guide=FALSE) + scale_fill_viridis_d(guide=FALSE) + 
        xlab("Percent missing") + ylab("")
```

The graph shows that as more data is set to missing, the size of the confidence interval grows slightly. In all cases, the confidence interval contains the mean and it seems that the sample mean is pretty resistant to change when there is missing data. The table shows us that Bias actually decreases as the missing data changes from 0% to 20% and 20% to 40%. However, after 40% the bias starts to increase substantially goint from 4.83 to 6.88 for 60% missing and finally 17.24 for 80% missing.

### 5. Repeat problems 2 and 3 using a different missing data mechanism.  

```{r, echo=FALSE}
create.NMAR <- function(dta, pmiss){
    N <- length(dta)
    x <- sort(dta)
    prop.miss <- seq(0, pmiss, length.out=N) 
    flag.miss <- rbinom(N, 1, prop.miss)
    x[flag.miss==1] <- NA
    observed.data <- x
    return(observed.data)
}
```


```{r}  
#Sets variables and seed
s=40
set.seed(8675309)

# sample data from known/named distributions.
norm.pop.data <- data.frame(x=rnorm(n=N, mean=10, sd=2), dist = "Normal") 
chi2.pop.data <- data.frame(x=rchisq(N,10), dist = "X2")
unif.pop.data <- data.frame(x=runif(N, 0,20 ), dist = "Uniform")

norm.samp <- sample(norm.pop.data$x, size=s, replace=TRUE)
chi2.samp <- sample(chi2.pop.data$x, size=s, replace=TRUE)
unif.samp <- sample(unif.pop.data$x, size=s, replace=TRUE)

# combine into one data set
pop.data <- rbind(norm.pop.data, chi2.pop.data, unif.pop.data)

# calculate grouped summary stats
pop.ss <- pop.data %>% group_by(dist) %>% 
                    summarise(mean=mean(x), var=var(x), 
                              min=min(x), max=max(x))
N = 1000
# Create missing (make sure to match the distribution names exactly)
obs.norm <- data.frame(x=create.NMAR(dta=norm.samp, pmiss=.2), dist="Normal")
obs.chi2 <- data.frame(x=create.NMAR(dta=chi2.samp, pmiss=.2), dist="X2")
obs.unif <- data.frame(x=create.NMAR(dta=unif.samp, pmiss=.2), dist="Uniform")

# combine into one data set
observed <- rbind(obs.norm, obs.chi2, obs.unif)

# plot population distribution, with sample points added on in black. 
ggplot(pop.data, aes(x=x, fill=dist)) + geom_histogram()  + 
      facet_wrap(~dist, ncol=1) + scale_fill_discrete(guide=FALSE)  + 
      geom_dotplot(data=observed, aes(x=x), dotsize=.5, fill="black")

# calculate grouped summary stats & approx 95% CI's
nmar.ci <- observed %>% group_by(dist) %>% 
                    summarise(xbar=mean(x), 
                              lower = xbar-2*sd(x)/sqrt(n()), 
                              upper = xbar+2*sd(x)/sqrt(n()))

# take the true mean and distribution name from the population summary stats
compare.nmar <- pop.ss %>% select(dist, mean) %>% 
# then left join on the sample CI from the mcar observed data
                    left_join(nmar.ci) %>% 
# calculate the bias, and a logical indicator for if the CI covers the mean. 
                    mutate(abs.bias = xbar-mean, 
                           pct.bias = (abs.bias/mean)*100, 
                           cover = ((mean < upper) & (mean > lower)))

# pretty display
kable(compare.nmar, digits=4) %>% kable_styling(full_width=FALSE)


```

Using the not missing at random method, we can see that the sample data for the normal distribution did not pick many values at the tails of the graph. We can also see that the chi square distribution is slightly more right skew to it. However, the uniform distribution's sample data does not look as uniform as the population. There are more data values towards the ends of the graph and less in the middle of the distribution. Looking at our table, all of our summary statistics are showing up as NA.

### 6. What do you think would happen if the missing data mechanism was negatively correlated with the value of x?

If the missing data mechanism was negatively correlated with x, then the missing data itself could be described by another missing data point.

# Multiple Imputation using Chained Equations
### 1. What percent of the data set overall is missing?

```{r}
hiv.all <- readRDS("C:/Users/KBari/OneDrive/Desktop/Math 456/Hiv_data.rds")
hiv <- hiv.all %>% 
       select(age, gender, bsi_overall, frnds, hookey, likesch, school, 
              jobmo, edumo, howrel, attserv, livwith, finsit)

survey <- MASS::survey 
View(hiv)
round(prop.table(table(is.na(hiv)))*100,1)
```
  
Overall 5.6% of the variables came back as missing.

### 2. How much missing data is there per variable?
```{r}
prop.miss <- apply(hiv, 2, function(x) round(sum(is.na(x))/NROW(x),4))
prop.miss
```

27.1% of the responses for mother's education are missing.
17.1% of the responses for how often they attend religious services are missing.
16.7% of the responses for how religious the individual was are missing.
9.2% of the responses for job of the mother are missing.
1.2% of responses for number of friends as well as BSI are missing.


All other variables are less than 1% missing for this set.

### 3. Describe the missing data patterns. 

```{r}
pmpv <- data.frame(variable = names(hiv), pct.miss =prop.miss)

ggplot(pmpv, aes(x=variable, y=pct.miss)) +
  geom_bar(stat="identity") + ylab("Percent") + scale_y_continuous(labels=scales::percent, limits=c(0,1)) + 
  geom_text(data=pmpv, aes(label=paste0(round(pct.miss*100,1),"%"), y=pct.miss+.025), size=4)

```

I believe the questions about religion would be considered not missing at random because people might not want to comment if they do not go to religious services frequently or practice a certain religion. For missing data regarding the job and education of the mother, i checked to see how many respondants still live with their mother

```{r}
round(prop.table(table(hiv$livwith))*100, 1)
```

The amount of people that no longer live with their mother is at least 9.6%, this could describe why 9.2% of the data for jobmo is missing.

### 4a. Use tables to examine the relationship in missing data between 1) the two religion variables `howrel` and `attserv`, 2) between the two financial variables `jobmo` and `finsit`, and 3) how much they like school `likesch` and if they have played hookey `hookey`. 

```{r}
pander(table(hiv$howrel,hiv$attserv, useNA = "always"))

pander(table(hiv$jobmo,hiv$finsit, useNA = "always"))

pander(table(hiv$likesch,hiv$hookey, useNA = "always"))
```

Looking at the variables for how religious a person is and how often they attend service, we can see that these variables are usually missing together. There were 41 instances where both of these were missing together. 

Looking at the job of the mother and financial situation, we can see that there is no data missing from financial situation. Job of the mother seems to be missing more as their financial situation gets better however this could be due to the fact that a higher quantity of people with comfortable financial situations were polled.

When we look at the table for if they liked school and if they played hookey, we can see there is no data missing from these.

### 4b. Use a margin plot to describe the relationship of missing data between Age and BSI overall.

```{r}
View(hiv)
marginplot(hiv[,c(1,3)])
```

There are no missing data points for the age variable. The mean for the missing data seems to be slightly lower than the true mean age.

### 5. Multiply impute this data set $m=5$ times. State the imputation models used for each variable.

The imputation models used for each variable are

```{r}
imp_hiv <- mice(hiv, m=5, maxit=25, meth = hiv$meth, seed=500, printFlag=FALSE)
summary(imp_hiv)

```

### 6. After controlling for other measures, what is the effect of gender on the likelihood a student will skip school? Adjust the model for fit or stability as needed. Report your results in a nice table and interpret the effect of gender on skipping school. 

```{r}
complete.model <- glm(hookey ~ age + edumo + jobmo + likesch + howrel + gender, data=hiv,family = "binomial")

imp.model <- with(imp_hiv, glm(hookey ~ age + edumo + jobmo + likesch + howrel + gender, family = "binomial"))
```
  
#### 6a. Fit this model on the complete cases (no imputation).
```{r}
pander(summary(complete.model))
```

The model states that while holding other variables constant, Males are .41 times less likely to play hookey.

#### 6b. Fit this model on the multiply imputed data sets and report the pooled estimates and intervals. 
Interpret the effect of gender on playing hookey. Did it change from the complete case model? 

```{r}
pander(summary(pool(imp.model)))
```

The imputed model shows us that males are only .23 times less likely than females to play hookey however neither of the models were statistically significant and both had high p values. (p>.30)

### 7. Create a forestplot to compare the results for all coefficients in the model. 

What are the biggest differences you notice? Would the inference/interpretation of the effect of any covariate on the odds of a student skipping school change depending on what model you use? 

```{r, fig.height=12, fig.width=10}
te.mean <- summary(complete.model)$coefficients[,2]
mi.mean <- summary(pool(imp.model))[,2]
te.ll <- te.mean - 1.96*summary(complete.model)$coefficients[,3]
mi.ll <- mi.mean - 1.96*summary(pool(imp.model))[,3]
te.ul <- te.mean + 1.96*summary(complete.model)$coefficients[,3]
mi.ul <- mi.mean + 1.96*summary(pool(imp.model))[,3]
names <- names(coef(complete.model))

forestplot(names,
legend = c("True Model", "MICE"),
fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
mean = cbind(te.mean, mi.mean),
lower = cbind(te.ll, mi.ll),
upper = cbind(te.ul, mi.ul),
col=fpColors(box=c("blue", "darkred")),
xlab="Regression coefficients", 
boxsize = .1
)
```

Whether or not they like school varies between the imputed and original model, hovwever the models for the rest of the variables are similar between the two models.

