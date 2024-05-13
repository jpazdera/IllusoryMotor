library(tidyverse)
library(lme4)  # For linear mixed effects modeling
library(lmerTest)  # For F testing of LMER models
library(car)  # For qqPlot
options(contrasts=c("contr.sum","contr.poly"))
setwd("~/git/IllusoryMotor/analysis/")

### DATA PREPARATION ###

# Load processed dataset
sdata <- read.csv("../processed_data/sync_data.csv", fileEncoding="UTF-8-BOM")
sdata <- mutate_if(sdata, is.character, as.factor)
sdata$subject <- as.factor(sdata$subject)
sdata$ioi <- as.factor(sdata$ioi)
subj_savgs <- group_by(sdata, subject, register, pitch, ioi) %>%
  summarize(perc_async=mean(perc_async), hearing_threshold=mean(hearing_threshold))

cdata <- read.csv("../processed_data/cont_data.csv", fileEncoding="UTF-8-BOM")
cdata <- mutate_if(cdata, is.character, as.factor)
cdata$subject <- as.factor(cdata$subject)
cdata$ioi <- as.factor(cdata$ioi)
subj_cavgs <- group_by(cdata, subject, register, pitch, ioi) %>%
  summarize(rel_iti=mean(rel_iti), hearing_threshold=mean(hearing_threshold))

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
# ioi                  2.09   2.091     1  38.00  0.3474    0.5591
# poly(pitch, 2)     426.98 213.489     2 381.48 35.4754 7.411e-15 ***
# hearing_threshold    9.70   9.701     1 391.32  1.6121    0.2050
# ioi:poly(pitch, 2)  14.16   7.080     2 379.98  1.1765    0.3095
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)          -13.48922    0.79700  38.07399 -16.925  < 2e-16 ***
# ioi1                  -0.20754    0.35210  37.99990  -0.589    0.559
# poly(pitch, 2)1        1.55789    2.83473 382.96192   0.550    0.583
# poly(pitch, 2)2       20.61635    2.46154 380.05728   8.375 1.07e-15 ***
# hearing_threshold      0.03606    0.02840 391.31720   1.270    0.205
# ioi1:poly(pitch, 2)1  -3.64822    2.45315 379.97515  -1.487    0.138
# ioi1:poly(pitch, 2)2  -0.92232    2.45315 379.97515  -0.376    0.707
summary(model)

# EXPERIMENT 2 (LOWER)
# ioi (*): Participants tapped later when synchronizing to 600 ms stimuli
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
#                     Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                 20.281  20.281     1  22.00  5.2353   0.03211 *
# poly(pitch, 2)     159.055  79.527     2 220.70 20.5289 6.658e-09 ***
# hearing_threshold    1.791   1.791     1 229.41  0.4622   0.49727
# ioi:poly(pitch, 2)   3.889   1.945     2 219.94  0.5020   0.60601
anova(model)
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)          -12.89513    1.01601  22.00572 -12.692 1.34e-11 ***
# ioi1                  -1.00013    0.43710  22.00003  -2.288  0.03211 *
# poly(pitch, 2)1      -12.31028    2.13035 221.45114  -5.779 2.54e-08 ***
# poly(pitch, 2)2        5.57249    1.96899 219.94935   2.830  0.00508 **
# hearing_threshold      0.02927    0.04305 229.41411   0.680  0.49727
# ioi1:poly(pitch, 2)1  -0.87380    1.96823 219.94129  -0.444  0.65751
# ioi1:poly(pitch, 2)2   1.76800    1.96823 219.94129   0.898  0.37002
summary(model)

# EXPERIMENT 2 (UPPER)
# ioi (n.s.): Different tempos give similar tap phases/percent asynchronies
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
# ioi                0.0083983 0.0083983     1  38.00 45.1585 5.898e-08 ***
# poly(pitch, 2)     0.0021537 0.0010769     2 381.34  5.7904  0.003332 **
# hearing_threshold  0.0004120 0.0004120     1 390.09  2.2151  0.137473
# ioi:poly(pitch, 2) 0.0000364 0.0000182     2 380.00  0.0980  0.906709
anova(model)
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)           9.650e-01  5.505e-03  3.807e+01 175.284  < 2e-16 ***
# ioi1                  2.095e-02  3.117e-03  3.800e+01   6.720  5.9e-08 ***
# poly(pitch, 2)1       1.066e-03  1.576e-02  3.826e+02   0.068 0.946134
# poly(pitch, 2)2       4.648e-02  1.368e-02  3.801e+02   3.397 0.000754 ***
# hearing_threshold     2.352e-04  1.580e-04  3.901e+02   1.488 0.137473
# ioi1:poly(pitch, 2)1 -3.207e-03  1.364e-02  3.800e+02  -0.235 0.814236
# ioi1:poly(pitch, 2)2  5.114e-03  1.364e-02  3.800e+02   0.375 0.707862
summary(model)

# EXPERIMENT 2 (LOWER)
# ioi (***): Tapped faster relative to the 600 ms stimulus than to the 400 ms one
# pitch (**): Negative linear effect - faster tapping to higher pitches
# ioi * pitch (n.s.): Negative linear effect was stronger in the 400 ms condition, but not significantly
# hearing_threshold (***): Faster tapping for pitches the participant was less sensitive to hearing
model <- lmer(rel_iti ~ 1 + ioi * poly(pitch, 2) + hearing_threshold + (1 + ioi | subject),
              data=subj_cavgs[subj_cavgs$register == 'L',], REML=FALSE)
# Normality of residuals: Good
qqPlot(residuals(model))
hist(residuals(model))
# Homoscedasticity: Good
plot(fitted(model), residuals(model), xlab="Fitted Values", ylab="Residuals")
# Type III Analysis of Variance Table with Satterthwaite's method
#                        Sum Sq    Mean Sq NumDF  DenDF F value    Pr(>F)
# ioi                0.00232486 0.00232486     1  22.00 20.9952 0.0001456 ***
# poly(pitch, 2)     0.00113153 0.00056577     2 220.53  5.1093 0.0067755 **
# hearing_threshold  0.00138288 0.00138288     1 226.76 12.4885 0.0004962 ***
# ioi:poly(pitch, 2) 0.00043035 0.00021518     2 220.00  1.9432 0.1456948
anova(model)
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)           9.689e-01  6.463e-03  2.206e+01 149.902  < 2e-16 ***
# ioi1                  1.434e-02  3.130e-03  2.200e+01   4.582 0.000146 ***
# poly(pitch, 2)1      -3.567e-02  1.140e-02  2.211e+02  -3.130 0.001986 **
# poly(pitch, 2)2       7.207e-03  1.053e-02  2.200e+02   0.685 0.494291
# hearing_threshold    -8.164e-04  2.310e-04  2.268e+02  -3.534 0.000496 ***
# ioi1:poly(pitch, 2)1 -1.985e-02  1.052e-02  2.200e+02  -1.886 0.060546 .
# ioi1:poly(pitch, 2)2  6.023e-03  1.052e-02  2.200e+02   0.572 0.567679
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
