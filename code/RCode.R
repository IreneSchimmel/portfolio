
# R Code for the manuscript entitled:
# "Mental health impacts in Argentinean college students during COVID-19 quarantine".
# López Steinmetz L.C., Leyes C.A., Dutto Florio M.A., Fong S.B., López Steinmetz R.L. & Godoy J.C.

##########################################################################
################## LOAD THE DATASET & PACKAGES ###########################
##########################################################################

library(readxl)

# Load the dataset
# table<-read.table("clipboard",header=TRUE, encoding = "Latin 1", dec=",", sep="\t") 
table <- read_excel("dataset.xlsx")
summary(table)

# Load the packages:
library(moments)
library(gplots)

##########################################################################
###################### METHODS ###########################################
##########################################################################

###### SUB-TITLE: METHOD > Sample and procedure
# SAMPLE N = 2687

# Distribution by sex:
table(table$SEX)
# Absolute frequencies: Women = 2192, Men = 473, Other = 22
prop.table(table(table$SEX))*100
# Percentages: Women = 81.577968%, Men = 17.603275%, Other = 0.818757%

# Central tendency measures by age (total sample)
# mean
mean(table$AGE)
# Mean age = 22.74023
# standard deviation
sd(table$AGE)
# sd age = 3.635612
# median
median(table$AGE)
# median age = 22

# Distribution by provinces
prop.table(table(table$PROVINCE))*100
# JUJ (JUJUY) = 6.6989207%
# SAL (SALTA) = 7.1082992%
# CBA (CÓRDOBA) = 39.0026051%
# STACR (SANTA CRUZ) = 0.9676219%
# TDELF (TIERRA DEL FUEGO) = 2.3446223%
# CABA (CIUDAD AUTÓNOMA DE BUENOS AIRES) = 11.9464086%
# PCIAB (PROVINCIA DE BUENOS AIRES) = 31.9315221%

###### SUB-TITLE: METHOD > Data analysis

### To test Skewness and Kurtosis # Criteria: range of acceptable values or near to (-3 and +3; Brown, 2006).
# Reference: Brown T.A. (2006). Confirmatory factor analysis for applied research. New York: Guilford Press.

# PSYCH.WELLBEING
# skewness(table$PSYCH.WELLBEING)
skewness(table$`PSYCH WELLBEING`)
# skewness PSYCH.WELLBEING = -0.05214941
# kurtosis(table$PSYCH.WELLBEING)
kurtosis(table$`PSYCH WELLBEING`)
# kurtosis PSYCH.WELLBEING = 1.951112

# SOC.FUNC.AND.COPING
# skewness(table$SOC.FUNC.AND.COPING)
skewness(table$`SOC FUNC AND COPING`)
# skewness SOC.FUNC.AND.COPING = 0.5326852
# kurtosis(table$SOC.FUNC.AND.COPING) 
kurtosis(table$`SOC FUNC AND COPING`)
# kurtosis SOC.FUNC.AND.COPING = 2.141538

# K10
skewness(table$K10) 
# skewness K10 = 0.2819622
kurtosis(table$K10)
# kurtosis K10 = 2.248409

# BDI
skewness(table$BDI) 
# skewness BDI = 0.6843361
kurtosis(table$BDI)
# kurtosis BDI = 2.907102

# STAIR
skewness(table$STAIR) 
# skewness STAIR =  0.02036605
kurtosis(table$STAIR)
# kurtosis STAIR = 2.336078

# YAACQ
skewness(table$YAACQ) 
# skewness YAACQ = 1.21683  
kurtosis(table$YAACQ)
# kurtosis YAACQ = 4.238449

# ISO30
skewness(table$ISO) 
# skewness ISO30 = 0.4657512
kurtosis(table$ISO)
# kurtosis ISO30 = 2.569562


### For analyses corresponding to the first aim, we divided the entire sample into four groups: 
table(table$REGIONS)
# NORTH = 371
# CENTER = 1048
# SOUTH = 89
# MOST POPULATED = 1179


### For analyses corresponding to the second aim, we divided the entire sample into four groups: 
#table(table$SUB.PERIODS.IN.PRE.AND.POST)
table(table$`SUB PERIODS IN PRE AND POST`)
# first week pre-quarantine extension (ONE WEEK PRE) = 1508
# second week pre-quarantine extension (TWO WEEK PRE) = 525
# first week post-quarantine extension (ONE WEEK POST) = 364
# remaining weeks post-quarantine extension (REMAINING WEEKS POST) = 290


##########################################################################
###################### RESULTS ###########################################
##########################################################################

##########################################################################
####################### AIM 1 ############################################
##########################################################################

### Differences in mental health aspects (both general and specific) by four regions

# PSYCHOLOGICAL WELL-BEING/DISCOMFORT (OF GENERAL HEALTH)
# anovaregpsychwellbeing <- aov(table$PSYCH.WELLBEING~table$REGIONS)
anovaregpsychwellbeing <- aov(table$`PSYCH WELLBEING`~table$REGIONS)
summary(anovaregpsychwellbeing)
plot(anovaregpsychwellbeing)
TukeyHSD(anovaregpsychwellbeing)
plot(TukeyHSD(anovaregpsychwellbeing))
# significant differences
# MOST POPULATED-CENTER p adj 0.0017158 #### MOST POPULATED mean = 3.206107, CENTER mean = 2.924618
# tapply(table$PSYCH.WELLBEING,factor(table$REGIONS),mean)
tapply(table$`PSYCH WELLBEING`, factor(table$REGIONS), mean)
# tapply(table$PSYCH.WELLBEING,factor(table$REGIONS),sd)
tapply(table$`PSYCH WELLBEING`, factor(table$REGIONS), sd)

# SOCIAL FUNCTIONING AND COPING (OF GENERAL HEALTH)
# anovaregsocfunc <- aov(table$SOC.FUNC.AND.COPING~table$REGIONS)
anovaregsocfunc <- aov(table$`SOC FUNC AND COPING`~table$REGIONS)
summary(anovaregsocfunc)
plot(anovaregsocfunc)
TukeyHSD(anovaregsocfunc)
plot(TukeyHSD(anovaregsocfunc))
# NO significant differences
tapply(table$SOC.FUNC.AND.COPING,factor(table$REGIONS),mean)
tapply(table$SOC.FUNC.AND.COPING,factor(table$REGIONS),sd)


# PSYCHOLOGICAL DISTRESS
anovaregk10 <- aov(table$K10~table$REGIONS)
summary(anovaregk10)
plot(anovaregk10)
TukeyHSD(anovaregk10)
plot(TukeyHSD(anovaregk10))
# NO significant differences
tapply(table$K10,factor(table$REGIONS),mean)
tapply(table$K10,factor(table$REGIONS),sd)


# DEPRESSION
anovaregdepr <- aov(table$BDI~table$REGIONS)
summary(anovaregdepr)
plot(anovaregdepr)
TukeyHSD(anovaregdepr)
plot(TukeyHSD(anovaregdepr))
# NO significant differences
tapply(table$BDI,factor(table$REGIONS),mean)
tapply(table$BDI,factor(table$REGIONS),sd)


# ANXIETY
anovareganx <- aov(table$STAIR~table$REGIONS)
summary(anovareganx)
plot(anovareganx)
TukeyHSD(anovareganx)
plot(TukeyHSD(anovareganx))
# NO significant differences
tapply(table$STAIR,factor(table$REGIONS),mean)
tapply(table$STAIR,factor(table$REGIONS),sd)


# NEGATIVE ALCOHOL-RELATED CONSEQUENCES
anovaregalc <- aov(table$YAACQ~table$REGIONS)
summary(anovaregalc)
plot(anovaregalc)
TukeyHSD(anovaregalc)
plot(TukeyHSD(anovaregalc))
# significant differences
# MOST POPULATED-CENTER p adj 0.0001598 ### MOST POPULATED mean = 3.330789, CENTER mean = 4.017176
tapply(table$YAACQ,factor(table$REGIONS),mean)
tapply(table$YAACQ,factor(table$REGIONS),sd)


# SUICIDAL RISK
anovaregsuic <- aov(table$ISO~table$REGIONS)
summary(anovaregsuic)
plot(anovaregsuic)
TukeyHSD(anovaregsuic)
plot(TukeyHSD(anovaregsuic))
# NO significant differences
tapply(table$ISO,factor(table$REGIONS),mean)
tapply(table$ISO,factor(table$REGIONS),sd)


##########################################################################
####################### AIM 2 ############################################
##########################################################################

### Differences in mental health aspects (both general and specific) by four sub-periods of quarantine

# PSYCHOLOGICAL WELL-BEING (OF GENERAL HEALTH)
anovatemppsychwellbeing <- aov(table$PSYCH.WELLBEING~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatemppsychwellbeing)
plot(anovatemppsychwellbeing)
TukeyHSD(anovatemppsychwellbeing)
plot(TukeyHSD(anovatemppsychwellbeing))
# significant differences
# 4. REMAINING WEEKS POST-1. ONE WEEK PRE p adj 0.0002823
# 3. ONE WEEK POST-2. TWO WEEK PRE p adj 0.0333276
# 4. REMAINING WEEKS POST-2. TWO WEEK PRE p adj 0.0000283
tapply(table$PSYCH.WELLBEING,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$PSYCH.WELLBEING,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S1:
plotmeans(table$PSYCH.WELLBEING~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S1: Psychological well-being/discomfort by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Psychological well-being/discomfort", xlab = "Quarantine's sub periods")


# SOCIAL FUNCTIONING AND COPING (OF GENERAL HEALTH)
anovatempsocfunc <- aov(table$SOC.FUNC.AND.COPING~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatempsocfunc)
plot(anovatempsocfunc)
TukeyHSD(anovatempsocfunc)
plot(TukeyHSD(anovatempsocfunc))
# significant differences
# 3. ONE WEEK POST-1. ONE WEEK PRE  p adj 0.0278280
# 4. REMAINING WEEKS POST-1. ONE WEEK PRE p adj 0.0001652
# 3. ONE WEEK POST-2. TWO WEEK PRE p adj 0.0480422
# 4. REMAINING WEEKS POST-2. TWO WEEK PRE p adj 0.0006543
tapply(table$SOC.FUNC.AND.COPING,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$SOC.FUNC.AND.COPING,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S2:
plotmeans(table$SOC.FUNC.AND.COPING~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S2: Social functioning and coping by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Social functioning and coping", xlab = "Quarantine's sub periods")


# PSYCHOLOGICAL DISTRESS
anovatempk10 <- aov(table$K10~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatempk10)
plot(anovatempk10)
TukeyHSD(anovatempk10)
plot(TukeyHSD(anovatempk10))
# significant differences
# 4. REMAINING WEEKS POST-1. ONE WEEK PRE p adj 0.0379214
# 4. REMAINING WEEKS POST-2. TWO WEEK PRE p adj 0.0068317
tapply(table$K10,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$K10,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S3:
plotmeans(table$K10~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S3: Psychological distress by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Psychological distress", xlab = "Quarantine's sub periods")


# DEPRESSION
anovatempdepr <- aov(table$BDI~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatempdepr)
plot(anovatempdepr)
TukeyHSD(anovatempdepr)
plot(TukeyHSD(anovatempdepr))
# NO significant differences
tapply(table$BDI,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$BDI,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S4:
plotmeans(table$BDI~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S4: Depression by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Depression", xlab = "Quarantine's sub periods")


# ANXIETY
anovatempanx <- aov(table$STAIR~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatempanx)
plot(anovatempanx)
TukeyHSD(anovatempanx)
plot(TukeyHSD(anovatempanx))
# NO significant differences
tapply(table$STAIR,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$STAIR,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S5:
plotmeans(table$STAIR~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S5: Anxiety by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Anxiety", xlab = "Quarantine's sub periods")


# NEGATIVE ALCOHOL-RELATED CONSEQUENCES
anovatempalc <- aov(table$YAACQ~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatempalc)
plot(anovatempalc)
TukeyHSD(anovatempalc)
plot(TukeyHSD(anovatempalc))
# significant differences
# 4. REMAINING WEEKS POST-1. ONE WEEK PRE p adj 0.0580130
tapply(table$YAACQ,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$YAACQ,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S6:
plotmeans(table$YAACQ~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S6: Negative alcohol-related consequences by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Negative alcohol-related consequences", xlab = "Quarantine's sub periods")


# SUICIDAL RISK
anovatempsuic <- aov(table$ISO~table$SUB.PERIODS.IN.PRE.AND.POST)
summary(anovatempsuic)
plot(anovatempsuic)
TukeyHSD(anovatempsuic)
plot(TukeyHSD(anovatempsuic))
# NO significant differences
tapply(table$ISO,factor(table$SUB.PERIODS.IN.PRE.AND.POST),mean)
tapply(table$ISO,factor(table$SUB.PERIODS.IN.PRE.AND.POST),sd)
# Figure S7:
plotmeans(table$ISO~table$SUB.PERIODS.IN.PRE.AND.POST, main="Fig. S7: Suicidal risk by quarantine sub-periods. Mean plot with 95% Confidence Interval", cex.main = 0.8, ylab = "Suicidal risk", xlab = "Quarantine's sub periods")

##########################################################################
############################# THE END ####################################
##########################################################################
