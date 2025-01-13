library(tidyverse)
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models
library(car)  # For qqPlot
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/Documents/git/IllusoryMotor/analysis/")

### DATA PREPARATION ###

# Load processed dataset
sdata <- read.csv("../processed_data/sync_data.csv", fileEncoding="UTF-8-BOM")
sdata <- mutate_if(sdata, is.character, as.factor)
sdata$subject <- as.factor(sdata$subject)
sdata$ioi <- as.factor(sdata$ioi)
subj_savgs <- group_by(sdata, subject, register, pitch, ioi) %>%
  summarize(perc_async=mean(perc_async), hearing_threshold=mean(hearing_threshold), years_lessons=mean(years_lessons))

cdata <- read.csv("../processed_data/cont_data.csv", fileEncoding="UTF-8-BOM")
cdata <- mutate_if(cdata, is.character, as.factor)
cdata$subject <- as.factor(cdata$subject)
cdata$ioi <- as.factor(cdata$ioi)
subj_cavgs <- group_by(cdata, subject, register, pitch, ioi) %>%
  summarize(rel_iti=mean(rel_iti), hearing_threshold=mean(hearing_threshold), years_lessons=mean(years_lessons))

# Center hearing thresholds within-subject
subj_avg_thresholds <- group_by(subj_savgs, subject) %>% summarize(subj_avg_threshold=mean(hearing_threshold))
subj_savgs <- subj_savgs %>% left_join(subj_avg_thresholds, by="subject") %>%
  mutate(hearing_threshold_centered = hearing_threshold - subj_avg_threshold)
subj_avg_thresholds <- group_by(subj_cavgs, subject) %>% summarize(subj_avg_threshold=mean(hearing_threshold))
subj_cavgs <- subj_cavgs %>% left_join(subj_avg_thresholds, by="subject") %>%
  mutate(hearing_threshold_centered = hearing_threshold - subj_avg_threshold)

MED_MUS_EXP <- 4.0  # Calculated previously in Python
subj_savgs['highmus'] <- as.factor(subj_savgs$years_lessons > MED_MUS_EXP)
subj_cavgs['highmus'] <- as.factor(subj_cavgs$years_lessons > MED_MUS_EXP)

###
# PERCENT ASYNCHRONY
###

# EXPERIMENT 1
# ioi (n.s.):  Different tempos gave similar tap phases/percent asynchronies
# pitch (***): Positive quadratic effect - middle pitches produced earliest tapping
# ioi * pitch (n.s.): Pitch had similar effect regardless of tempo
# hearing_threshold (n.s.): Asynchrony was not correlated with hearing threshold
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'B',], REML=FALSE)
# Normality of residuals: Okay, but a little skewed - probably related to people anticipating the tone, which makes the positive tail extend less far
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                            Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                          1.13   1.134     1    39  0.1909    0.6646
# poly(pitch, 2)             432.43 216.215     2   390 36.3923 3.226e-15 ***
# hearing_threshold_centered  10.89  10.885     1   390  1.8322    0.1767
# ioi:poly(pitch, 2)          11.91   5.954     2   390  1.0022    0.3680
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                -13.72353    0.81840  39.00164 -16.769  < 2e-16 ***
# ioi1                        -0.15182    0.34746  38.99792  -0.437    0.665
# poly(pitch, 2)1              1.94416    2.83606 390.00021   0.686    0.493
# poly(pitch, 2)2             20.70527    2.44333 390.00021   8.474 4.91e-16 ***
# hearing_threshold_centered   0.03816    0.02819 390.00021   1.354    0.177
# ioi1:poly(pitch, 2)1        -3.24008    2.43746 390.00021  -1.329    0.185
# ioi1:poly(pitch, 2)2        -1.18772    2.43746 390.00021  -0.487    0.626
summary(model)

# EXPERIMENT 2 (LOWER)
# ioi (n.s.): Different tempos gave similar percent asynchronies
# pitch (***): Stronger negative linear + weaker positive quadratic effect - higher pitches produce earlier tapping
# ioi * pitch (n.s.): Pitch had similar effect regardless of tempo
# hearing_threshold (n.s.): Asynchrony was not correlated with hearing threshold
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'L',], REML=FALSE)
# Normality of residuals: Good
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                             Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                          1.394   1.394     1  24.001  0.2930    0.5933
# poly(pitch, 2)             181.947  90.973     2 240.000 19.1178 1.976e-08 ***
# hearing_threshold_centered   4.792   4.792     1 240.000  1.0071    0.3166
# ioi:poly(pitch, 2)           1.500   0.750     2 240.000  0.1576    0.8543
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                -14.39794    1.39416  24.00053 -10.327 2.61e-10 ***
# ioi1                        -0.32875    0.60733  24.00072  -0.541   0.5933
# poly(pitch, 2)1            -13.72824    2.38724 239.99982  -5.751 2.69e-08 ***
# poly(pitch, 2)2              5.28735    2.18575 239.99982   2.419   0.0163 *
# hearing_threshold_centered   0.04648    0.04631 239.99982   1.004   0.3166
# ioi1:poly(pitch, 2)1        -1.08978    2.18141 239.99982  -0.500   0.6178
# ioi1:poly(pitch, 2)2         0.55867    2.18141 239.99982   0.256   0.7981
summary(model)

# EXPERIMENT 2 (UPPER)
# ioi (n.s.): Different tempos give similar percent asynchronies
# pitch (***): Stronger positive linear + weaker positive quadratic effect - higher pitches produce later tapping
# ioi * pitch (n.s.): Pitch has similar effect regardless of tempo
# hearing_threshold (**): Later tapping for pitches the participant was less sensitive to hearing
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'U',], REML=FALSE)
# Normality of residuals: Okay - kurtosis is a little high, not skewed
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                             Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                          9.046   9.046     1    24  1.3694  0.253407
# poly(pitch, 2)             168.673  84.337     2   240 12.7671 5.381e-06 ***
# hearing_threshold_centered  58.632  58.632     1   240  8.8759  0.003186 **
# ioi:poly(pitch, 2)           5.676   2.838     2   240  0.4297  0.651236
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                -14.48776    1.06349  24.00014 -13.623 8.67e-13 ***
# ioi1                         0.53004    0.45294  24.00002   1.170  0.25341
# poly(pitch, 2)1             11.63759    2.65490 239.99999   4.383 1.75e-05 ***
# poly(pitch, 2)2              7.54764    2.67891 239.99999   2.817  0.00524 **
# hearing_threshold_centered   0.09179    0.03081 239.99999   2.979  0.00319 **
# ioi1:poly(pitch, 2)1        -2.18593    2.57017 239.99999  -0.850  0.39589
# ioi1:poly(pitch, 2)2         0.94767    2.57017 239.99999   0.369  0.71266
summary(model)

###
# CONTINUATION ITI
###

# EXPERIMENT 1
# ioi (***): Tapped faster relative to the 600 ms stimulus than to the 400 ms one
# pitch (**): Positive quadratic effect - fastest tapping to middle pitches
# ioi * pitch (n.s.): Pitch has a similar effect regardless of tempo
# hearing_threshold (n.s.): Tapping rate was not correlated with hearing threshold
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'B',], REML=FALSE)
# Normality of residuals: High kurtosis, not skewed
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq   Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                        0.0091344 0.0091344     1    39 48.5109 2.394e-08 ***
# poly(pitch, 2)             0.0023593 0.0011797     2   390  6.2650  0.002099 **
# hearing_threshold_centered 0.0004436 0.0004436     1   390  2.3559  0.125620
# ioi:poly(pitch, 2)         0.0000589 0.0000295     2   390  0.1565  0.855161
anova(model)
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                 9.646e-01  5.392e-03  3.900e+01 178.912  < 2e-16 ***
# ioi1                        2.127e-02  3.054e-03  3.900e+01   6.965 2.39e-08 ***
# poly(pitch, 2)1             2.797e-03  1.597e-02  3.900e+02   0.175  0.86104
# poly(pitch, 2)2             4.851e-02  1.376e-02  3.900e+02   3.527  0.00047 ***
# hearing_threshold_centered  2.436e-04  1.587e-04  3.900e+02   1.535  0.12562
# ioi1:poly(pitch, 2)1       -7.094e-03  1.372e-02  3.900e+02  -0.517  0.60545
# ioi1:poly(pitch, 2)2        2.936e-03  1.372e-02  3.900e+02   0.214  0.83071
summary(model)

# EXPERIMENT 2 (LOWER)
# ioi (***): Tapped faster relative to the 600 ms stimulus than to the 400 ms one
# pitch (**): Negative linear effect - faster tapping to higher pitches
# ioi * pitch (n.s.): Negative linear effect was stronger in the 400 ms condition, but not significantly
# hearing_threshold (*): Faster tapping for pitches the participant was less sensitive to hearing
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'L',], REML=FALSE)
# Normality of residuals: Good
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                                Sum Sq    Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                        0.00217839 0.00217839     1  23.996 18.8175 0.0002237 ***
# poly(pitch, 2)             0.00152145 0.00076073     2 240.000  6.5713 0.0016653 **
# hearing_threshold_centered 0.00074301 0.00074301     1 240.000  6.4183 0.0119328 *
# ioi:poly(pitch, 2)         0.00058220 0.00029110     2 240.000  2.5146 0.0830246 .
anova(model)
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                 9.621e-01  7.675e-03  2.400e+01 125.354  < 2e-16 ***
# ioi1                        1.677e-02  3.867e-03  2.400e+01   4.338 0.000224 ***
# poly(pitch, 2)1            -3.940e-02  1.177e-02  2.400e+02  -3.346 0.000950 ***
# poly(pitch, 2)2             1.595e-02  1.078e-02  2.400e+02   1.480 0.140297
# hearing_threshold_centered -5.787e-04  2.284e-04  2.400e+02  -2.533 0.011933 *
# ioi1:poly(pitch, 2)1       -2.362e-02  1.076e-02  2.400e+02  -2.195 0.029100 *
# ioi1:poly(pitch, 2)2        4.929e-03  1.076e-02  2.400e+02   0.458 0.647298
summary(model)

# EXPERIMENT 2 (UPPER)
# ioi (***): Tapped faster relative to the 600 ms stimulus than to the 400 ms one
# pitch (**): Positive linear effect - slower tapping to higher pitches
# ioi * pitch (n.s.): Pitch has a similar effect regardless of tempo
# hearing_threshold (n.s.): Tapping rate was not correlated with hearing threshold
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'U',], REML=FALSE)
# Normality of residuals: Okay - maybe slightly high kurtosis
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq   Mean Sq NumDF DenDF F value  Pr(>F)
# ioi                        0.0054395 0.0054395     1    24 50.5074 2.4e-07 ***
# poly(pitch, 2)             0.0012646 0.0006323     2   240  5.8713 0.00324 **
# hearing_threshold_centered 0.0000041 0.0000041     1   240  0.0380 0.84566
# ioi:poly(pitch, 2)         0.0000831 0.0000415     2   240  0.3858 0.68034
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                9.668e-01  5.724e-03 2.400e+01 168.892  < 2e-16 ***
# ioi1                       1.632e-02  2.296e-03 2.400e+01   7.107  2.4e-07 ***
# poly(pitch, 2)1            3.218e-02  1.072e-02 2.400e+02   3.002  0.00296 **
# poly(pitch, 2)2            2.012e-02  1.082e-02 2.400e+02   1.860  0.06405 .
# hearing_threshold_centered 2.424e-05  1.244e-04 2.400e+02   0.195  0.84566
# ioi1:poly(pitch, 2)1       6.159e-03  1.038e-02 2.400e+02   0.593  0.55341
# ioi1:poly(pitch, 2)2       6.720e-03  1.038e-02 2.400e+02   0.648  0.51789
summary(model)

###
# Hearing Thresholds
###
subj_savgs2 <- group_by(sdata, subject, pitch) %>%
  summarize(perc_async=mean(perc_async), hearing_threshold=mean(hearing_threshold))
# Center hearing thresholds within-subject
subj_avg_thresholds <- group_by(subj_savgs2, subject) %>% summarize(subj_avg_threshold=mean(hearing_threshold))
subj_savgs2 <- subj_savgs2 %>% left_join(subj_avg_thresholds, by="subject") %>%
  mutate(hearing_threshold_centered = hearing_threshold - subj_avg_threshold)

model <- lmer(hearing_threshold ~ 1 + poly(pitch, 2) + (1 | subject), data=subj_savgs2, REML=FALSE)
# Fixed effects:
# Estimate Std. Error       df t value Pr(>|t|)
# (Intercept)       1.0348     0.4017  86.2044   2.576   0.0117 *
# poly(pitch, 2)1 -48.0086     5.1726 508.5734  -9.281   <2e-16 ***
# poly(pitch, 2)2   0.7930     4.4912 437.7938   0.177   0.8599
summary(model)
cor.test(subj_savgs2$hearing_threshold_centered, subj_savgs2$pitch)

model <- lmer(perc_async ~ 1 + hearing_threshold_centered + (1 | subject),
              data=subj_savgs2, REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                            Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)
# hearing_threshold_centered 31.051  31.051     1   435  7.7601 0.005575 **
anova(model)

subj_cavgs2 <- group_by(cdata, subject, pitch) %>%
  summarize(rel_iti=mean(rel_iti), hearing_threshold=mean(hearing_threshold))
# Center hearing thresholds within-subject
subj_avg_thresholds <- group_by(subj_cavgs2, subject) %>% summarize(subj_avg_threshold=mean(hearing_threshold))
subj_cavgs2 <- subj_cavgs2 %>% left_join(subj_avg_thresholds, by="subject") %>%
  mutate(hearing_threshold_centered = hearing_threshold - subj_avg_threshold)
model <- lmer(rel_iti ~ 1 + hearing_threshold_centered + (1 | subject),
              data=subj_cavgs2, REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                                Sum Sq    Mean Sq NumDF DenDF F value Pr(>F)
# hearing_threshold_centered 1.9841e-05 1.9841e-05     1   435  0.2341 0.6288
anova(model)

###
# SUPPLEMENTAL
###

# Experiment 1: Noninteraction between music training and pitch effect for synchronization
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'B',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                            Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                          1.13   1.132     1    39  0.1909    0.6646
# poly(pitch, 2)             415.70 207.851     2   390 35.0620 9.931e-15 ***
# highmus                     13.63  13.630     1    39  2.2992    0.1375
# hearing_threshold_centered  12.87  12.870     1   390  2.1709    0.1414
# ioi:poly(pitch, 2)          11.91   5.954     2   390  1.0044    0.3672
# poly(pitch, 2):highmus       5.12   2.559     2   390  0.4317    0.6497
anova(model)

# Experiment 2: High musicality had more positive quadratic slope in lower reg than low musicality
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'L',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                             Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                          1.320   1.320     1    24  0.2930  0.593295
# poly(pitch, 2)             169.964  84.982     2   240 18.8577 2.474e-08 ***
# highmus                      1.382   1.382     1    24  0.3068  0.584802
# hearing_threshold_centered   8.063   8.063     1   240  1.7892  0.182288
# ioi:poly(pitch, 2)           1.500   0.750     2   240  0.1664  0.846806
# poly(pitch, 2):highmus      60.503  30.251     2   240  6.7129  0.001456 **
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                -14.45414    1.39154  24.05982 -10.387 2.27e-10 ***
# ioi1                        -0.32875    0.60733  24.00013  -0.541 0.593295
# poly(pitch, 2)1            -13.54607    2.33158 239.99977  -5.810 1.98e-08 ***
# poly(pitch, 2)2              4.60248    2.13551 239.99977   2.155 0.032140 *
# highmus1                    -0.67443    1.21770  24.00035  -0.554 0.584802
# hearing_threshold_centered   0.06051    0.04524 239.99977   1.338 0.182288
# ioi1:poly(pitch, 2)1        -1.08978    2.12284 239.99977  -0.513 0.608172
# ioi1:poly(pitch, 2)2         0.55867    2.12284 239.99977   0.263 0.792644
# poly(pitch, 2)1:highmus1    -1.34081    2.13026 239.99977  -0.629 0.529678
# poly(pitch, 2)2:highmus1    -7.71805    2.13827 239.99977  -3.609 0.000373 ***
summary(model)

# Experiment 2: High music training had stronger linear effect in upper reg
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'U',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                             Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)
# ioi                          8.687   8.687     1    24  1.3694 0.253408
# poly(pitch, 2)             211.102 105.551     2   240 16.6387 1.71e-07 ***
# highmus                      4.105   4.105     1    24  0.6471 0.429050
# hearing_threshold_centered  65.883  65.883     1   240 10.3856 0.001446 **
# ioi:poly(pitch, 2)           5.676   2.838     2   240  0.4474 0.639819
# poly(pitch, 2):highmus      62.900  31.450     2   240  4.9577 0.007766 **
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)                -14.25116    1.07627  24.61280 -13.241 1.08e-12 ***
# ioi1                         0.53004    0.45294  23.99989   1.170  0.25341
# poly(pitch, 2)1             14.55899    2.76248 240.00002   5.270 3.03e-07 ***
# poly(pitch, 2)2              7.15305    2.73044 240.00002   2.620  0.00936 **
# highmus1                    -0.70981    0.88238  24.00003  -0.804  0.42905
# hearing_threshold_centered   0.09965    0.03092 240.00002   3.223  0.00145 **
# ioi1:poly(pitch, 2)1        -2.18593    2.51867 240.00002  -0.868  0.38632
# ioi1:poly(pitch, 2)2         0.94767    2.51867 240.00002   0.376  0.70706
# poly(pitch, 2)1:highmus1    -8.25480    2.67357 240.00002  -3.088  0.00226 **
# poly(pitch, 2)2:highmus1     1.76217    2.73396 240.00002   0.645  0.51983
summary(model)

# Experiment 1: Noninteraction between musicality and pitch for continuation tapping
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'B',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq   Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                        0.0091297 0.0091297     1    39 48.5106 2.394e-08 ***
# poly(pitch, 2)             0.0023939 0.0011970     2   390  6.3600  0.001914 **
# highmus                    0.0001039 0.0001039     1    39  0.5520  0.461951
# hearing_threshold_centered 0.0004232 0.0004232     1   390  2.2486  0.134546
# ioi:poly(pitch, 2)         0.0000589 0.0000295     2   390  0.1566  0.855092
# poly(pitch, 2):highmus     0.0000375 0.0000187     2   390  0.0995  0.905274
anova(model)

# Experiment 2: Noninteraction between musicality and pitch in lower reg
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'L',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                                Sum Sq    Mean Sq NumDF DenDF F value    Pr(>F)
# ioi                        0.00213834 0.00213834     1    24 18.8194 0.0002235 ***
# poly(pitch, 2)             0.00134241 0.00067120     2   240  5.9072 0.0031310 **
# highmus                    0.00000084 0.00000084     1    24  0.0074 0.9321126
# hearing_threshold_centered 0.00067066 0.00067066     1   240  5.9024 0.0158546 *
# ioi:poly(pitch, 2)         0.00058220 0.00029110     2   240  2.5620 0.0792623 .
# poly(pitch, 2):highmus     0.00051366 0.00025683     2   240  2.2604 0.1065293
anova(model)

# Experiment 2: Noninteraction between musicality and pitch in upper reg
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'U',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                               Sum Sq   Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                        0.0053330 0.0053330     1  24.000 50.5062 2.401e-07 ***
# poly(pitch, 2)             0.0014357 0.0007178     2 240.000  6.7984  0.001343 **
# highmus                    0.0000715 0.0000715     1  24.001  0.6776  0.418527
# hearing_threshold_centered 0.0000338 0.0000338     1 240.000  0.3204  0.571869
# ioi:poly(pitch, 2)         0.0000831 0.0000415     2 240.000  0.3935  0.675145
# poly(pitch, 2):highmus     0.0005054 0.0002527     2 240.000  2.3934  0.093494 .
anova(model)
