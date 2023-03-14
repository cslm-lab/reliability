---
title: "Reliability Analyses"
author: "Pam Fuhrmeister"
date: "2023-03-13"
output:
  html_document:
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_float: yes
    theme: cosmo
    df_print: kable
---


```r
library(tidyverse)
library(ggthemes)
library(brms)
library(tidybayes)
library(posterior)

knitr::opts_chunk$set(echo = TRUE)
options(width = 120)
theme_set(theme_few(base_size = 10))

# Uses the computer cores available
options(mc.cores = parallel::detectCores())
iter <- 4000
warmup <- 2000
chains <- 4
```


# Experiment 1

In Experiment 1, participants completed a simple picture-naming task (i.e., they saw pictures on a computer screen and were asked to say the name of them aloud) in two different sessions. We tested the split-half reliability in each session as well as the test-retest reliability between sessions. 

## Split-half reliability in Session 1

Read in files for Session 1, filter for correct responses only:


```r
# Read in one participant's data to assign column classes to big data frame
test <- read.csv("../data/Session1_ListA/results_5cd49.csv")
colclasses <- sapply(test, typeof)

file_names_a <- list.files(path = "../data/Session1_ListA/", pattern = "*.csv", full.names = TRUE)

file_names_b <- list.files(path = "../data/Session1_ListB/", pattern = "*.csv", full.names = TRUE)

df_a <- map_dfr(file_names_a, read.csv, colClasses = colclasses)
df_b <- map_dfr(file_names_b, read.csv, colClasses = colclasses)

# Combine data frames into one big one and filter for correct responses only
df <- rbind(df_a, df_b) %>%
    filter(Response_Trial == "correct") %>%
    filter(RT > 0)

# Create a column for even/odd trials
df <- df %>%
    group_by(Participant_ID) %>%
    mutate(TrialType = ifelse(Trial_Nbr%%2 == 0, "even", "odd"))
```


### Fit model to test split-half reliability in Session 1


```r
fit_e1_s1_split <- brm(RT ~ 0 + TrialType + (0 + TrialType | Participant_ID) + (1 | Target), family = lognormal(), prior = c(prior(normal(6,
    1.5), class = b), prior(normal(0, 1), class = sigma), prior(normal(0, 1), class = sd), prior(lkj(2), class = cor)), iter = iter,
    warmup = warmup, data = df)
```

### Get estimate of correlation and 95% credible interval


```r
fit_e1_s1_split %>%
    spread_draws(cor_Participant_ID__TrialTypeeven__TrialTypeodd) %>%
    mean_hdi(cor_Participant_ID__TrialTypeeven__TrialTypeodd)
```

<div class="kable-table">

| cor_Participant_ID__TrialTypeeven__TrialTypeodd|    .lower|    .upper| .width|.point |.interval |
|-----------------------------------------------:|---------:|---------:|------:|:------|:---------|
|                                       0.9911376| 0.9782859| 0.9999021|   0.95|mean   |hdi       |

</div>

### Plot density of split-half reliability estimate


```r
fit_e1_s1_split %>%
    spread_draws(cor_Participant_ID__TrialTypeeven__TrialTypeodd) %>%
    ggplot(aes(x = cor_Participant_ID__TrialTypeeven__TrialTypeodd, fill = stat(abs(x) > 0.81))) + stat_halfeye(.width = 0.95) +
    labs(x = "Correlation between even and odd trials", y = "Density") + geom_vline(xintercept = c(0.4, 0.6, 0.8), linetype = "dashed") +
    theme(legend.position = "none") + scale_fill_manual(values = c("skyblue", "gray80")) + annotate(geom = "text", x = 0.18,
    y = 0.95, label = "Poor") + annotate(geom = "text", x = 0.5, y = 0.95, label = "Moderate") + annotate(geom = "text",
    x = 0.7, y = 0.95, label = "Good") + annotate(geom = "text", x = 0.9, y = 0.95, label = "Excellent") + scale_x_continuous(breaks = c(0,
    0.4, 0.6, 0.8)) + coord_cartesian(xlim = c(0, 1))
```

![](reliability_analyses_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### Check with frequentist correlation

Just in case we didn't have enough data to estimate the random effect correlations well, we'll test this with a frequentist correlation to make sure the results aren't too different.


```r
df_sum <- df %>%
    group_by(Participant_ID, TrialType) %>%
    summarize(RT_mean = mean(RT))


df_sum_wide <- df_sum %>%
    pivot_wider(names_from = TrialType, values_from = RT_mean)

cor.test(df_sum_wide$even, df_sum_wide$odd)
```

```

	Pearson's product-moment correlation

data:  df_sum_wide$even and df_sum_wide$odd
t = 30.602, df = 47, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9572924 0.9863574
sample estimates:
      cor 
0.9758126 
```


## Split-half reliability in Session 2

Read in files for Session 2, filter for correct responses only:


```r
# Read in one participant's data to assign column classes to big data frame
test_2 <- read.csv("../data/Session2_ListA/results_5cbcc.csv")
colclasses <- sapply(test_2, typeof)

file_names_a_2 <- list.files(path = "../data/Session2_ListA/", pattern = "*.csv", full.names = TRUE)

file_names_b_2 <- list.files(path = "../data/Session2_ListB/", pattern = "*.csv", full.names = TRUE)


df_a_2 <- map_dfr(file_names_a_2, read.csv, colClasses = colclasses)
df_b_2 <- map_dfr(file_names_b_2, read.csv, colClasses = colclasses)

# Combine data frames into one big one and filter for correct responses only
df_2 <- rbind(df_a_2, df_b_2) %>%
    filter(Response_Trial == "correct") %>%
    filter(RT > 0)

# Create a column for even/odd trials
df_2 <- df_2 %>%
    group_by(Participant_ID) %>%
    mutate(TrialType = ifelse(Trial_Nbr%%2 == 0, "even", "odd"))
```

### Fit model to test split-half reliability in Session 2


```r
fit_e1_s2_split <- brm(RT ~ 0 + TrialType + (0 + TrialType | Participant_ID) + (1 | Target), family = lognormal(), prior = c(prior(normal(6,
    1.5), class = b), prior(normal(0, 1), class = sigma), prior(normal(0, 1), class = sd), prior(lkj(2), class = cor)), iter = iter,
    warmup = warmup, data = df_2)
```

### Get estimate of correlation and 95% credible interval


```r
fit_e1_s2_split %>%
    spread_draws(cor_Participant_ID__TrialTypeeven__TrialTypeodd) %>%
    mean_hdi(cor_Participant_ID__TrialTypeeven__TrialTypeodd)
```

<div class="kable-table">

| cor_Participant_ID__TrialTypeeven__TrialTypeodd|    .lower|    .upper| .width|.point |.interval |
|-----------------------------------------------:|---------:|---------:|------:|:------|:---------|
|                                        0.985922| 0.9681396| 0.9995249|   0.95|mean   |hdi       |

</div>

### Plot density of split-half reliability estimate


```r
fit_e1_s2_split %>%
    spread_draws(cor_Participant_ID__TrialTypeeven__TrialTypeodd) %>%
    ggplot(aes(x = cor_Participant_ID__TrialTypeeven__TrialTypeodd, fill = stat(abs(x) > 0.81))) + stat_halfeye(.width = 0.95) +
    labs(x = "Correlation between even and odd trials", y = "Density") + geom_vline(xintercept = c(0.4, 0.6, 0.8), linetype = "dashed") +
    theme(legend.position = "none") + scale_fill_manual(values = c("skyblue", "gray80")) + annotate(geom = "text", x = 0.18,
    y = 0.95, label = "Poor") + annotate(geom = "text", x = 0.5, y = 0.95, label = "Moderate") + annotate(geom = "text",
    x = 0.7, y = 0.95, label = "Good") + annotate(geom = "text", x = 0.9, y = 0.95, label = "Excellent") + scale_x_continuous(breaks = c(0,
    0.4, 0.6, 0.8)) + coord_cartesian(xlim = c(0, 1))
```

![](reliability_analyses_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

#### Check with frequentist correlation

Just in case we didn't have enough data to estimate the random effect correlations well, we'll test this with a frequentist correlation as well.


```r
df_sum_2 <- df_2 %>%
    group_by(Participant_ID, TrialType) %>%
    summarize(RT_mean = mean(RT))


df_sum_wide_2 <- df_sum_2 %>%
    pivot_wider(names_from = TrialType, values_from = RT_mean)

cor.test(df_sum_wide_2$even, df_sum_wide_2$odd)
```

```

	Pearson's product-moment correlation

data:  df_sum_wide_2$even and df_sum_wide_2$odd
t = 22.887, df = 47, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9262587 0.9761871
sample estimates:
      cor 
0.9579475 
```

## Test-retest reliability


```r
df_all <- rbind(df, df_2)

df_all$Session <- as.factor(as.character(df_all$Session))
```


### Fit model for test-retest reliability in Experiment 1


```r
fit_e1_t_rt <- brm(RT ~ 0 + Session + (0 + Session | Participant_ID) + (1 | Target), family = lognormal(), prior = c(prior(normal(6,
    1.5), class = b), prior(normal(0, 1), class = sigma), prior(normal(0, 1), class = sd), prior(lkj(2), class = cor)), iter = 10000,
    data = df_all)
```

### Get estimate of correlation and 95% credible interval


```r
fit_e1_t_rt %>%
    spread_draws(cor_Participant_ID__Session1__Session2) %>%
    mean_hdi(cor_Participant_ID__Session1__Session2)
```

<div class="kable-table">

| cor_Participant_ID__Session1__Session2|    .lower|    .upper| .width|.point |.interval |
|--------------------------------------:|---------:|---------:|------:|:------|:---------|
|                               0.759312| 0.6316418| 0.8745616|   0.95|mean   |hdi       |

</div>

### Plot density of split-half reliability estimate


```r
fit_e1_t_rt %>%
    spread_draws(cor_Participant_ID__Session1__Session2) %>%
    ggplot(aes(x = cor_Participant_ID__Session1__Session2, fill = stat(abs(x) > 0.81))) + stat_halfeye(.width = 0.95) + labs(x = "Correlation between first and second sessions",
    y = "Density") + geom_vline(xintercept = c(0.4, 0.6, 0.8), linetype = "dashed") + theme(legend.position = "none") + scale_fill_manual(values = c("gray80",
    "skyblue")) + annotate(geom = "text", x = 0.18, y = 0.95, label = "Poor") + annotate(geom = "text", x = 0.5, y = 0.95,
    label = "Moderate") + annotate(geom = "text", x = 0.7, y = 0.95, label = "Good") + annotate(geom = "text", x = 0.9, y = 0.95,
    label = "Excellent") + scale_x_continuous(breaks = c(0, 0.4, 0.6, 0.8)) + coord_cartesian(xlim = c(0, 1))
```

![](reliability_analyses_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### Check with frequentist correlation

Just in case we didn't have enough data to estimate the random effect correlations well, we'll test this with a frequentist correlation as well.


```r
df_sum_3 <- df_all %>%
    group_by(Participant_ID, Session) %>%
    summarize(RT_mean = mean(RT))


df_sum_wide_3 <- df_sum_3 %>%
    pivot_wider(names_from = Session, values_from = RT_mean)

cor.test(df_sum_wide_3$`1`, df_sum_wide_3$`2`)
```

```

	Pearson's product-moment correlation

data:  df_sum_wide_3$`1` and df_sum_wide_3$`2`
t = 8.4981, df = 46, p-value = 5.513e-11
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.6394698 0.8720619
sample estimates:
      cor 
0.7815926 
```
