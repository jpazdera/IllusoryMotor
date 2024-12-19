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
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
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
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
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
model <- lmer(perc_async ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
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
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
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
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
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
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
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
# SUPPLEMENTAL
###

# Experiment 1: Noninteraction between music training and pitch effect for synchronization
model <- lmer(perc_async ~ 1 + ioi + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'B',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                        Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                      1.14   1.138     1  39.00  0.1909    0.6646
# poly(pitch, 2)         415.82 207.911     2 391.73 34.8905 1.136e-14 ***
# highmus                 14.16  14.157     1  39.02  2.3757    0.1313
# hearing_threshold       10.60  10.596     1 401.99  1.7782    0.1831
# poly(pitch, 2):highmus   4.87   2.434     2 390.22  0.4085    0.6650
anova(model)

# Experiment 2: High musicality had more positive quadratic slope in lower reg than low musicality
model <- lmer(perc_async ~ 1 + ioi + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'L',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                         Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                      1.323   1.323     1  24.000  0.2930  0.593297
# poly(pitch, 2)         177.610  88.805     2 240.585 19.6708 1.224e-08 ***
# highmus                  1.322   1.322     1  23.809  0.2928  0.593435
# hearing_threshold        4.822   4.822     1 248.622  1.0682  0.302357
# poly(pitch, 2):highmus  59.638  29.819     2 239.841  6.6051  0.001613 **
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)              -14.52205    1.41051  23.98883 -10.296 2.79e-10 ***
# ioi1                      -0.32875    0.60734  24.00002  -0.541 0.593297
# poly(pitch, 2)1          -13.84312    2.32987 241.33855  -5.942 9.80e-09 ***
# poly(pitch, 2)2            4.64943    2.13733 239.85199   2.175 0.030581 *
# highmus1                  -0.66765    1.23377  23.80946  -0.541 0.593435
# hearing_threshold          0.04633    0.04483 248.62240   1.034 0.302357
# poly(pitch, 2)1:highmus1  -1.34291    2.13217 239.80608  -0.630 0.529404
# poly(pitch, 2)2:highmus1  -7.66008    2.14003 239.87594  -3.579 0.000417 ***
summary(model)

# Experiment 2: High music training had stronger linear effect in upper reg
model <- lmer(perc_async ~ 1 + ioi + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_savgs[subj_savgs$register == 'U',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                         Sum Sq Mean Sq NumDF   DenDF F value   Pr(>F)
# ioi                      8.719   8.719     1  24.000  1.3694 0.253404
# poly(pitch, 2)         211.694 105.847     2 240.365 16.6234 1.73e-07 ***
# highmus                  6.239   6.239     1  24.124  0.9798 0.332076
# hearing_threshold       67.411  67.411     1 246.872 10.5869 0.001298 **
# poly(pitch, 2):highmus  62.946  31.473     2 240.166  4.9429 0.007876 **
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)              -14.22663    1.06628  24.60278 -13.342 9.19e-13 ***
# ioi1                       0.53004    0.45294  24.00042   1.170  0.25340
# poly(pitch, 2)1           14.56764    2.76626 240.46363   5.266 3.09e-07 ***
# poly(pitch, 2)2            7.15999    2.73464 240.30581   2.618  0.00940 **
# highmus1                  -0.87029    0.87923  24.12386  -0.990  0.33208
# hearing_threshold          0.10003    0.03074 246.87151   3.254  0.00130 **
# poly(pitch, 2)1:highmus1  -8.25611    2.67852 240.01084  -3.082  0.00229 **
# poly(pitch, 2)2:highmus1   1.76933    2.73811 240.32341   0.646  0.51878
summary(model)

# Experiment 1: Noninteraction between musicality and pitch for continuation tapping
model <- lmer(rel_iti ~ 1 + ioi + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'B',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq   Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                    0.0091371 0.0091371     1  39.00 48.5111 2.394e-08 ***
# poly(pitch, 2)         0.0023941 0.0011970     2 391.70  6.3554  0.001922 **
# highmus                0.0001156 0.0001156     1  39.06  0.6138  0.438092
# hearing_threshold      0.0004281 0.0004281     1 401.51  2.2730  0.132429
# poly(pitch, 2):highmus 0.0000375 0.0000187     2 390.25  0.0995  0.905337
anova(model)

# Experiment 2: Noninteraction between musicality and pitch in lower reg
model <- lmer(rel_iti ~ 1 + ioi + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'L',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                            Sum Sq    Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                    0.00218423 0.00218423     1  24.000 18.8191 0.0002236 ***
# poly(pitch, 2)         0.00140155 0.00070077     2 240.613  6.0378 0.0027638 **
# highmus                0.00000127 0.00000127     1  23.942  0.0110 0.9174907
# hearing_threshold      0.00078449 0.00078449     1 247.608  6.7590 0.0098877 **
# poly(pitch, 2):highmus 0.00050900 0.00025450     2 239.971  2.1928 0.1138396
anova(model)

# Experiment 2: Noninteraction between musicality and pitch in upper reg
model <- lmer(rel_iti ~ 1 + ioi + poly(pitch, 2) * highmus + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'U',], REML=FALSE)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq   Mean Sq NumDF   DenDF F value    Pr(>F)
# ioi                    0.0053507 0.0053507     1  24.000 50.5061 2.401e-07 ***
# poly(pitch, 2)         0.0014645 0.0007322     2 240.110  6.9117  0.001206 **
# highmus                0.0000687 0.0000687     1  24.019  0.6481  0.428694
# hearing_threshold      0.0000496 0.0000496     1 242.619  0.4683  0.494421
# poly(pitch, 2):highmus 0.0005149 0.0002574     2 240.035  2.4300  0.090203 .
anova(model)