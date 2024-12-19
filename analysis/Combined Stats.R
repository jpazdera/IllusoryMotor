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
              data=sdata[sdata$register == 'B',], REML=FALSE)
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
              data=sdata[sdata$register == 'U',], REML=FALSE)
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
