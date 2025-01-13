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
#                    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                  1.13   1.134     1  39.00  0.1909    0.6646
# poly(pitch, 2)     432.64 216.322     2 391.51 36.4077 3.152e-15 ***
# hearing_threshold    8.50   8.502     1 401.41  1.4309    0.2323
# ioi:poly(pitch, 2)  11.91   5.954     2 389.95  1.0021    0.3680
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)          -13.76437    0.82356  39.06428 -16.713  < 2e-16 ***
# ioi1                  -0.15182    0.34745  38.99996  -0.437    0.665
# poly(pitch, 2)1        1.70248    2.83038 393.04723   0.602    0.548
# poly(pitch, 2)2       20.73348    2.44332 390.00272   8.486 4.52e-16 ***
# hearing_threshold      0.03346    0.02797 401.41112   1.196    0.232
# ioi1:poly(pitch, 2)1  -3.24008    2.43755 389.94522  -1.329    0.185
# ioi1:poly(pitch, 2)2  -1.18772    2.43755 389.94522  -0.487    0.626
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
#                    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                  1.395   1.395     1  24.00  0.2930    0.5933
# poly(pitch, 2)     189.883  94.942     2 240.62 19.9438 9.679e-09 ***
# hearing_threshold    2.338   2.338     1 248.97  0.4910    0.4841
# ioi:poly(pitch, 2)   1.500   0.750     2 239.81  0.1575    0.8543
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)          -14.44541    1.40765  23.93713 -10.262 3.04e-10 ***
# ioi1                  -0.32875    0.60734  24.00002  -0.541   0.5933
# poly(pitch, 2)1      -14.02875    2.38366 241.40407  -5.885 1.32e-08 ***
# poly(pitch, 2)2        5.32998    2.18609 239.84881   2.438   0.0155 *
# hearing_threshold      0.03212    0.04584 248.96803   0.701   0.4841
# ioi1:poly(pitch, 2)1  -1.08978    2.18185 239.81022  -0.499   0.6179
# ioi1:poly(pitch, 2)2   0.55867    2.18185 239.81022   0.256   0.7981
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
#                     Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                  9.046   9.046     1  24.00  1.3694  0.253410
# poly(pitch, 2)     168.236  84.118     2 240.53 12.7340 5.538e-06 ***
# hearing_threshold   58.301  58.301     1 247.61  8.8257  0.003262 **
# ioi:poly(pitch, 2)   5.676   2.838     2 240.00  0.4297  0.651236
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)          -14.51404    1.06207  24.00234 -13.666 8.09e-13 ***
# ioi1                   0.53004    0.45295  23.99958   1.170  0.25341
# poly(pitch, 2)1       11.61516    2.65349 240.49903   4.377 1.79e-05 ***
# poly(pitch, 2)2        7.52218    2.67711 240.63194   2.810  0.00536 **
# hearing_threshold      0.09075    0.03055 247.60794   2.971  0.00326 **
# ioi1:poly(pitch, 2)1  -2.18593    2.57017 239.99810  -0.850  0.39590
# ioi1:poly(pitch, 2)2   0.94767    2.57017 239.99810   0.369  0.71266
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
#                       Sum Sq   Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                0.0091344 0.0091344     1  39.00 48.5109 2.394e-08 ***
# poly(pitch, 2)     0.0023602 0.0011801     2 391.55  6.2672  0.002093 **
# hearing_threshold  0.0004373 0.0004373     1 401.39  2.3225  0.128305
# ioi:poly(pitch, 2) 0.0000589 0.0000295     2 390.00  0.1565  0.855161
anova(model)
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)           9.643e-01  5.402e-03  3.909e+01 178.527  < 2e-16 ***
# ioi1                  2.127e-02  3.054e-03  3.900e+01   6.965 2.39e-08 ***
# poly(pitch, 2)1       2.611e-03  1.593e-02  3.931e+02   0.164 0.869937
# poly(pitch, 2)2       4.854e-02  1.375e-02  3.901e+02   3.529 0.000467 ***
# hearing_threshold     2.400e-04  1.575e-04  4.014e+02   1.524 0.128305
# ioi1:poly(pitch, 2)1 -7.094e-03  1.372e-02  3.900e+02  -0.517 0.605448
# ioi1:poly(pitch, 2)2  2.936e-03  1.372e-02  3.900e+02   0.214 0.830712
summary(model)

# EXPERIMENT 2 (LOWER)
# ioi (***): Tapped faster relative to the 600 ms stimulus than to the 400 ms one
# pitch (**): Negative linear effect - faster tapping to higher pitches
# ioi * pitch (n.s.): Negative linear effect was stronger in the 400 ms condition, but not significantly
# hearing_threshold (**): Faster tapping for pitches the participant was less sensitive to hearing
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold_centered + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'L',], REML=FALSE)
# Normality of residuals: Good
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                        Sum Sq    Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                0.00217894 0.00217894     1  24.001 18.8198 0.0002235 ***
# poly(pitch, 2)     0.00158278 0.00079139     2 240.607  6.8354 0.0012963 **
# hearing_threshold  0.00086099 0.00086099     1 247.558  7.4365 0.0068488 **
# ioi:poly(pitch, 2) 0.00058220 0.00029110     2 239.942  2.5143 0.0830515 .
anova(model)
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)           9.630e-01  7.506e-03  2.402e+01 128.294  < 2e-16 ***
# ioi1                  1.677e-02  3.867e-03  2.400e+01   4.338 0.000224 ***
# poly(pitch, 2)1      -4.022e-02  1.176e-02  2.413e+02  -3.420 0.000735 ***
# poly(pitch, 2)2       1.607e-02  1.078e-02  2.400e+02   1.490 0.137467
# hearing_threshold    -6.177e-04  2.265e-04  2.476e+02  -2.727 0.006849 **
# ioi1:poly(pitch, 2)1 -2.362e-02  1.076e-02  2.399e+02  -2.195 0.029110 *
# ioi1:poly(pitch, 2)2  4.929e-03  1.076e-02  2.399e+02   0.458 0.647319
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
#                       Sum Sq   Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                0.0054398 0.0054398     1  24.00 50.5065 2.401e-07 ***
# poly(pitch, 2)     0.0013010 0.0006505     2 240.16  6.0397  0.002759 **
# hearing_threshold  0.0000119 0.0000119     1 242.87  0.1108  0.739475
# ioi:poly(pitch, 2) 0.0000831 0.0000415     2 239.96  0.3857  0.680363
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)          9.668e-01  5.712e-03 2.396e+01 169.240  < 2e-16 ***
# ioi1                 1.632e-02  2.296e-03 2.400e+01   7.107  2.4e-07 ***
# poly(pitch, 2)1      3.255e-02  1.072e-02 2.401e+02   3.037  0.00265 **
# poly(pitch, 2)2      2.054e-02  1.081e-02 2.402e+02   1.899  0.05871 .
# hearing_threshold    4.129e-05  1.240e-04 2.429e+02   0.333  0.73948
# ioi1:poly(pitch, 2)1 6.159e-03  1.038e-02 2.400e+02   0.593  0.55343
# ioi1:poly(pitch, 2)2 6.720e-03  1.038e-02 2.400e+02   0.648  0.51791
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
model <- lmer(perc_async ~ 1 + hearing_threshold + (1 | subject),
              data=subj_savgs2, REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                   Sum Sq Mean Sq NumDF  DenDF F value  Pr(>F)
# hearing_threshold 25.281  25.281     1 446.66  6.3169 0.01231 *
anova(model)

subj_cavgs2 <- group_by(cdata, subject, pitch) %>%
  summarize(rel_iti=mean(rel_iti), hearing_threshold=mean(hearing_threshold))
model <- lmer(rel_iti ~ 1 + hearing_threshold + (1 | subject),
              data=subj_cavgs2, REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                       Sum Sq    Mean Sq NumDF  DenDF F value Pr(>F)
# hearing_threshold 1.5782e-05 1.5782e-05     1 442.37  0.1862 0.6663
anova(model)

###
# SUPPLEMENTAL
###

# Experiment 1: Noninteraction between music training and pitch effect for synchronization
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'B',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                        Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                      1.13   1.132     1  39.00  0.1909    0.6646
# poly(pitch, 2)         415.82 207.911     2 391.72 35.0701 9.754e-15 ***
# highmus                 14.08  14.084     1  39.02  2.3757    0.1313
# hearing_threshold       10.61  10.608     1 401.93  1.7893    0.1818
# ioi:poly(pitch, 2)      11.91   5.954     2 389.96  1.0044    0.3672
# poly(pitch, 2):highmus   4.87   2.435     2 390.22  0.4107    0.6635
anova(model)

# Experiment 2: High musicality had more positive quadratic slope in lower reg than low musicality
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'L',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                         Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                      1.321   1.321     1  24.00  0.2930  0.593297
# poly(pitch, 2)         177.600  88.800     2 240.59 19.6969 1.197e-08 ***
# highmus                  1.320   1.320     1  23.81  0.2928  0.593437
# hearing_threshold        4.826   4.826     1 248.61  1.0705  0.301828
# ioi:poly(pitch, 2)       1.500   0.750     2 239.81  0.1663  0.846864
# poly(pitch, 2):highmus  59.639  29.820     2 239.84  6.6144  0.001599 **
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)              -14.52208    1.41051  23.98916 -10.296 2.79e-10 ***
# ioi1                      -0.32875    0.60734  24.00002  -0.541 0.593297
# poly(pitch, 2)1          -13.84271    2.32826 241.33666  -5.946 9.60e-09 ***
# poly(pitch, 2)2            4.64936    2.13585 239.85209   2.177 0.030469 *
# highmus1                  -0.66765    1.23377  23.80998  -0.541 0.593437
# hearing_threshold          0.04635    0.04480 248.61152   1.035 0.301828
# ioi1:poly(pitch, 2)1      -1.08978    2.12328 239.80615  -0.513 0.608245
# ioi1:poly(pitch, 2)2       0.55867    2.12328 239.80615   0.263 0.792685
# poly(pitch, 2)1:highmus1  -1.34291    2.13070 239.80624  -0.630 0.529120
# poly(pitch, 2)2:highmus1  -7.66016    2.13855 239.87600  -3.582 0.000413 ***
summary(model)

# Experiment 2: High music training had stronger linear effect in upper reg
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'U',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                         Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                      8.687   8.687     1  24.000  1.3694  0.253407
# poly(pitch, 2)         211.692 105.846     2 240.364 16.6852 1.639e-07 ***
# highmus                  6.216   6.216     1  24.123  0.9798  0.332075
# hearing_threshold       67.405  67.405     1 246.848 10.6255  0.001272 **
# ioi:poly(pitch, 2)       5.676   2.838     2 240.000  0.4474  0.639819
# poly(pitch, 2):highmus  62.946  31.473     2 240.166  4.9613  0.007738 **
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)              -14.22663    1.06628  24.60260 -13.342 9.20e-13 ***
# ioi1                       0.53004    0.45294  24.00000   1.170  0.25341
# poly(pitch, 2)1           14.56761    2.76113 240.46220   5.276 2.95e-07 ***
# poly(pitch, 2)2            7.15997    2.72956 240.30494   2.623  0.00927 **
# highmus1                  -0.87029    0.87922  24.12337  -0.990  0.33208
# hearing_threshold          0.10003    0.03069 246.84822   3.260  0.00127 **
# ioi1:poly(pitch, 2)1      -2.18593    2.51867 239.99968  -0.868  0.38632
# ioi1:poly(pitch, 2)2       0.94767    2.51867 239.99968   0.376  0.70706
# poly(pitch, 2)1:highmus1  -8.25610    2.67354 240.01103  -3.088  0.00225 **
# poly(pitch, 2)2:highmus1   1.76930    2.73302 240.32247   0.647  0.51800
summary(model)

# Experiment 1: Noninteraction between musicality and pitch for continuation tapping
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'B',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq   Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                    0.0091298 0.0091298     1  39.00 48.5111 2.394e-08 ***
# poly(pitch, 2)         0.0023941 0.0011970     2 391.69  6.3605  0.001912 **
# highmus                0.0001155 0.0001155     1  39.06  0.6138  0.438093
# hearing_threshold      0.0004281 0.0004281     1 401.50  2.2748  0.132275
# ioi:poly(pitch, 2)     0.0000589 0.0000295     2 390.00  0.1566  0.855092
# poly(pitch, 2):highmus 0.0000375 0.0000187     2 390.25  0.0996  0.905265
anova(model)

# Experiment 2: Noninteraction between musicality and pitch in lower reg
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'L',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                            Sum Sq    Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                    0.00213861 0.00213861     1  24.000 18.8194 0.0002235 ***
# poly(pitch, 2)         0.00140031 0.00070016     2 240.601  6.1613 0.0024574 **
# highmus                0.00000125 0.00000125     1  23.943  0.0110 0.9175094
# hearing_threshold      0.00078203 0.00078203     1 247.462  6.8817 0.0092490 **
# ioi:poly(pitch, 2)     0.00058220 0.00029110     2 239.942  2.5616 0.0792874 .
# poly(pitch, 2):highmus 0.00050910 0.00025455     2 239.971  2.2400 0.1086801
anova(model)

# Experiment 2: Noninteraction between musicality and pitch in upper reg
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'U',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq   Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                    0.0053337 0.0053337     1  24.002 50.5094 2.398e-07 ***
# poly(pitch, 2)         0.0014644 0.0007322     2 240.108  6.9338  0.001181 **
# highmus                0.0000684 0.0000684     1  24.020  0.6481  0.428676
# hearing_threshold      0.0000496 0.0000496     1 242.610  0.4693  0.493958
# ioi:poly(pitch, 2)     0.0000831 0.0000415     2 239.972  0.3934  0.675163
# poly(pitch, 2):highmus 0.0005148 0.0002574     2 240.034  2.4378  0.089517 .
anova(model)
