########################################
# Project:  Niranjan Poudel MS Thesis
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Niranjan Poudel
# File:     get_results.R
# Date:     2023 Summer
# About:    Get results of analysis
########################################

# Major edits
# 2022-04-25 created PAS, based on similar script in PSU PUT project
# 2022-05-06 updated PAS to reflect new Model Bs, figures
# 2022-05-22 updated PAS to use log-scale for figures
# 2023-08-19 updated PAS to use new models
# 2023-09-03 updated PAS to test WTP calculations

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
library(readr)

########################################
# Model B MMNL

# Load conditionals
# note: files were saved wrong (using save() instead of saveRDS()), so need to use load() instead of read.rds()
# cond <- read.rds(file.path("Analysis", "B_MMNL", "B_conditionals.rds"))
# uncond <- read.rds(file.path("Analysis", "B_MMNL", "B_unconditionals.rds"))
load(file.path("Analysis", "B_MMNL", "B_conditionals.rds"))
load(file.path("Analysis", "B_MMNL", "B_unconditionals.rds"))
cond <- conditionals; rm(conditionals)
uncond <- unconditionals; rm(unconditionals)

# Inspect conditionals
# summary
str(cond)
cond$time <- cond$g_TT
cond$work <- cond$g_WT
summary(cond$time$post.mean)
summary(cond$work$post.mean)
# descriptive statistics
mean(cond$time$post.mean)
sd(cond$time$post.mean)
mean(cond$work$post.mean)
sd(cond$work$post.mean)
cor(cond$time$post.mean, cond$work$post.mean)
# plots
plot(ecdf(cond$time$post.mean))
plot(ecdf(cond$work$post.mean))
plot(ecdf(log(cond$time$post.mean)))
plot(ecdf(log(cond$work$post.mean)))
# --> use conditionals

# Inspect unconditionals
# summary
str(uncond)
uncond$time <- uncond$g_TT
uncond$work <- uncond$g_WT
summary(rowMeans(uncond$time))
summary(rowMeans(uncond$work))
# descriptive statistics
mean(as.vector(uncond$time))
sd(as.vector(uncond$time))
mean(as.vector(uncond$work))
sd(as.vector(uncond$work))
cor(as.vector(uncond$time), as.vector(uncond$work))
# plots
# plot(ecdf(as.vector(uncond$time)))
# plot(ecdf(as.vector(uncond$work)))
# --> don't use unconditionals

# Figure: Cumulative distribution of conditional means
wtpWT <- cond$work$post.mean
wtpTT <- cond$time$post.mean
range(wtpWT) # 0.005151004 1.957329801
range(wtpTT) # 0.01887198 3.33589029
range(wtpWT*60) # 0.3090603 117.4397880
range(wtpTT*60) #  1.132319 200.153417
range(log(wtpWT*60)) # -1.174219  4.765926
range(log(wtpTT*60)) # 0.1242674 5.2990842
# regular figure
plot(0, las=1, type="n", xlim=c(0,100), ylim=c(0,1), 
     main="Cumulative distributions of conditional means (Model B1)", 
     xlab="Conditional mean of parameter estimate ($/hr)", 
     ylab="Cumulative probability")
lines(c(0,0), c(-1,2))
lines(ecdf(wtpWT*60), col="orange", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpTT*60), col="blue", lwd=3, lty="solid", do.points=F)
legend("bottomright", legend=c("WTP for work time", "WTP for travel time"), bty="n", 
       col=c("orange", "blue"), lwd=c(5,3), lty=c("solid", "solid"))
# log-scale x-axis figure
plot(0, las=1, type="n", xlim=c(0.1,200), ylim=c(0,1), log="x", 
     main="Cumulative distributions of conditional means (Model B1)", 
     xlab="Conditional mean of parameter estimate ($/hr), log-scale", 
     ylab="Cumulative probability")
lines(ecdf(wtpWT*60), col="orange", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpTT*60), col="blue", lwd=3, lty="solid", do.points=F)
legend("bottomright", legend=c("WTP for work time", "WTP for travel time"), bty="n", 
       col=c("orange", "blue"), lwd=c(5,3), lty=c("solid", "solid"))

# Figure: Scatterplot of conditional means
# regular figure
plot(0,0, las=1, type="n", xlim=c(0,100), ylim=c(0, 100), 
     main="Scatterplot of conditional means (Model B)", 
     xlab="Conditional mean of WTP for travel time ($/hr)", 
     ylab="Conditional mean of WTP for work time ($/hr)")
lines(c(0,100), c(0,100))
lines(c(0,100), c(0,0))
lines(c(0,0), c(0,100))
points(wtpTT*60, wtpWT*60, col=NA, bg=rgb(0,0,0,alpha=0.20), pch=21, cex=1.5)
# log-scale x-axis figure
plot(0,0, las=1, type="n", xlim=c(0.5,200), ylim=c(0.1,100), log="xy", 
     main="Scatterplot of conditional means (Model B)", 
     xlab="Conditional mean of WTP for travel time ($/hr), log-scale", 
     ylab="Conditional mean of WTP for work time ($/hr), log-scale")
lines(c(0.1,200), c(0.1,200))
points(wtpTT*60, wtpWT*60, col=NA, bg=rgb(0,0,0,alpha=0.20), pch=21, cex=1.5)
# save as image: Fig3a.png, 800x600

# Save
wtpTTB <- wtpTT
wtpWTB <- wtpWT

# Cleanup
rm(cond, uncond, wtpTT, wtpWT)
gc()

########################################
# Model C MMNL preference heterogeneity

# Load conditionals
# note: files were saved wrong (using save() instead of saveRDS()), so need to use load() instead of read.rds()
# cond <- read.rds(file.path("Analysis", "C_MMNL_het", "C_conditionals.rds"))
# uncond <- read.rds(file.path("Analysis", "C_MMNL_het", "C_unconditionals.rds"))
load(file.path("Analysis", "C_MMNL_het", "C_conditionals.rds"))
load(file.path("Analysis", "C_MMNL_het", "C_unconditionals.rds"))
cond <- conditionals; rm(conditionals)
uncond <- unconditionals; rm(unconditionals)

# Inspect conditionals
# summary
str(cond)
cond$time <- cond$g_TT
cond$work <- cond$g_WT
summary(cond$time$post.mean)
summary(cond$work$post.mean)
# descriptive statistics
mean(cond$time$post.mean)
sd(cond$time$post.mean)
mean(cond$work$post.mean)
sd(cond$work$post.mean)
cor(cond$time$post.mean, cond$work$post.mean)
# plots
plot(ecdf(cond$time$post.mean))
plot(ecdf(cond$work$post.mean))
plot(ecdf(log(cond$time$post.mean)))
plot(ecdf(log(cond$work$post.mean)))
# --> use conditionals

# Inspect unconditionals
# summary
str(uncond)
uncond$time <- uncond$g_TT
uncond$work <- uncond$g_WT
summary(rowMeans(uncond$time))
summary(rowMeans(uncond$work))
# descriptive statistics
mean(as.vector(uncond$time))
sd(as.vector(uncond$time))
mean(as.vector(uncond$work))
sd(as.vector(uncond$work))
cor(as.vector(uncond$time), as.vector(uncond$work))
# plots
# plot(ecdf(as.vector(uncond$time)))
# plot(ecdf(as.vector(uncond$work)))
# --> don't use unconditionals

# Figure: Cumulative distribution of conditional means
wtpWT <- cond$work$post.mean
wtpTT <- cond$time$post.mean
range(wtpWT) # 0.001800846 1.510890962
range(wtpTT) # 0.009428589 2.377524179
range(wtpWT*60) # 0.1080508 90.6534577
range(wtpTT*60) # 0.5657154 142.6514507
range(log(wtpWT*60)) # -2.225154  4.507044
range(log(wtpTT*60)) # -0.5696642  4.9604042
# regular figure
plot(0, las=1, type="n", xlim=c(0,100), ylim=c(0,1), 
     main="Cumulative distributions of conditional means (Model C1)", 
     xlab="Conditional mean of parameter estimate ($/hr)", 
     ylab="Cumulative probability")
lines(c(0,0), c(-1,2))
lines(ecdf(wtpWT*60), col="orange", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpTT*60), col="blue", lwd=3, lty="solid", do.points=F)
legend("bottomright", legend=c("WTP for work time", "WTP for travel time"), bty="n", 
       col=c("orange", "blue"), lwd=c(5,3), lty=c("solid", "solid"))
# log-scale x-axis figure
plot(0, las=1, type="n", xlim=c(0.1,200), ylim=c(0,1), log="x", 
     main="Cumulative distributions of conditional means (Model C1)", 
     xlab="Conditional mean of parameter estimate ($/hr), log-scale", 
     ylab="Cumulative probability")
lines(ecdf(wtpWT*60), col="orange", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpTT*60), col="blue", lwd=3, lty="solid", do.points=F)
legend("bottomright", legend=c("WTP for work time", "WTP for travel time"), bty="n", 
       col=c("orange", "blue"), lwd=c(5,3), lty=c("solid", "solid"))

# Figure: Scatterplot of conditional means
# regular figure
plot(0,0, las=1, type="n", xlim=c(0,100), ylim=c(0, 100), 
     main="Scatterplot of conditional means (Model C)", 
     xlab="Conditional mean of WTP for travel time ($/hr)", 
     ylab="Conditional mean of WTP for work time ($/hr)")
lines(c(0,100), c(0,100))
lines(c(0,100), c(0,0))
lines(c(0,0), c(0,100))
points(wtpTT*60, wtpWT*60, col=NA, bg=rgb(0,0,0,alpha=0.20), pch=21, cex=1.5)
# log-scale x-axis figure
plot(0,0, las=1, type="n", xlim=c(0.5,200), ylim=c(0.1,100), log="xy", 
     main="Scatterplot of conditional means (Model C)", 
     xlab="Conditional mean of WTP for travel time ($/hr), log-scale", 
     ylab="Conditional mean of WTP for work time ($/hr), log-scale")
lines(c(0.1,200), c(0.1,200))
points(wtpTT*60, wtpWT*60, col=NA, bg=rgb(0,0,0,alpha=0.20), pch=21, cex=1.5)
# save as image: Fig3b.png, 800x600

# Save
wtpTTC <- wtpTT
wtpWTC <- wtpWT

# Cleanup
rm(cond, uncond, wtpTT, wtpWT)
gc()

########################################
# Models B & C MMNL combined plots

# Figure: Cumulative distribution of conditional means
# regular figure
plot(0, las=1, type="n", xlim=c(0,100), ylim=c(0,1), 
     main="Cumulative distributions of conditional means (Models B and C)", 
     xlab="Conditional mean of parameter estimate ($/hr)", 
     ylab="Cumulative probability")
lines(c(0,0), c(-1,2))
lines(ecdf(wtpWTB*60), col="orange", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpWTC*60), col="black", lwd=2, lty="solid", do.points=F)
lines(ecdf(wtpTTB*60), col="blue", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpTTC*60), col="black", lwd=2, lty="solid", do.points=F)
legend("bottomright", bty="n", 
       legend=c("WTP for work time, Model B", "WTP for work time, Model C", 
                "WTP for travel time, Model B", "WTP for travel time, Model C"), 
       col=c("orange", "black", "blue", "black"), lwd=c(5,2,5,2), lty=c("solid", "solid", "solid", "solid"))
# save as image: Fig2a.png, 800x600
# log-scale x-axis figure
plot(0, las=1, type="n", xlim=c(0.1,200), ylim=c(0,1), log="x", 
     main="Cumulative distributions of conditional means (Models B and C)", 
     xlab="Conditional mean of parameter estimate ($/hr), log-scale", 
     ylab="Cumulative probability")
lines(ecdf(wtpWTB*60), col="orange", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpWTC*60), col="black", lwd=2, lty="solid", do.points=F)
lines(ecdf(wtpTTB*60), col="blue", lwd=5, lty="solid", do.points=F)
lines(ecdf(wtpTTC*60), col="black", lwd=2, lty="solid", do.points=F)
legend("bottomright", bty="n", 
       legend=c("WTP for work time, Model B", "WTP for work time, Model C", 
                "WTP for travel time, Model B", "WTP for travel time, Model C"), 
       col=c("orange", "black", "blue", "black"), lwd=c(5,2,5,2), lty=c("solid", "solid", "solid", "solid"))
# save as image: Fig2b.png, 800x600

# Comparisons for paper
table(wtpTTB < wtpWTB)
prop.table(table(wtpTTB < wtpWTB))
table(wtpTTC < wtpWTC)
prop.table(table(wtpTTC < wtpWTC))

# Remove
rm(wtpTTB, wtpTTC, wtpWTB, wtpWTC)
gc()

########################################
# Model B MMNL - WTP Calculations

# Load conditionals
# note: files were saved wrong (using save() instead of saveRDS()), so need to use load() instead of read.rds()
# cond <- read.rds(file.path("Analysis", "B_MMNL", "B_conditionals.rds"))
# uncond <- read.rds(file.path("Analysis", "B_MMNL", "B_unconditionals.rds"))
load(file.path("Analysis", "B_MMNL", "B_conditionals.rds"))
load(file.path("Analysis", "B_MMNL", "B_unconditionals.rds"))
cond <- conditionals; rm(conditionals)
uncond <- unconditionals; rm(unconditionals)

# Analytical calculations
# - mean = exp(mu+(sigma^2/2))
# - sd = sqrt((exp(sigma^2)-1)*(exp(2*mu+sigma^2)))
# - median = exp(mu)
# WTP for TT
exp(-2.039159214+(1.316257935^2/2))
sqrt((exp(1.316257935^2)-1)*(exp(2*-2.039159214+1.316257935^2)))
exp(-2.039159214)
# WTP for WT
exp(-3.460307544+(0.542721495^2/2)+(1.394326235^2/2))
# sd equation not calculated
exp(-3.460307544)
# MRS for IN
-exp(-1.573253691+(1.835140056^2/2))
sqrt((exp(1.835140056^2)-1)*(exp(2*-1.573253691+1.835140056^2)))
-exp(-1.573253691)
# correlations
# equations not calculated

# Empirical calculations
# WTP for TT
mean(uncond$g_TT)
sd(uncond$g_TT)
median(uncond$g_TT)
# WTP for WT
mean(uncond$g_WT)
sd(uncond$g_WT)
median(uncond$g_WT)
# MRS for IN
mean(uncond$g_IN)
sd(uncond$g_IN)
median(uncond$g_IN)
# correlations
cor(as.vector(uncond$g_TT), as.vector(uncond$g_WT))
cor(as.vector(uncond$g_TT), as.vector(uncond$g_IN))
cor(as.vector(uncond$g_WT), as.vector(uncond$g_IN))

# Remove
rm(cond, uncond)

########################################
# Model C MMNL preference heterogeneity - WTP Calculations

# Load conditionals
# note: files were saved wrong (using save() instead of saveRDS()), so need to use load() instead of read.rds()
# cond <- read.rds(file.path("Analysis", "C_MMNL_het", "C_conditionals.rds"))
# uncond <- read.rds(file.path("Analysis", "C_MMNL_het", "C_unconditionals.rds"))
load(file.path("Analysis", "C_MMNL_het", "C_conditionals.rds"))
load(file.path("Analysis", "C_MMNL_het", "C_unconditionals.rds"))
cond <- conditionals; rm(conditionals)
uncond <- unconditionals; rm(unconditionals)

# Analytical calculations
# equations not calculated

# Empirical calculations
# WTP for TT
mean(uncond$g_TT)
sd(uncond$g_TT)
median(uncond$g_TT)
# WTP for WT
mean(uncond$g_WT)
sd(uncond$g_WT)
median(uncond$g_WT)
# MRS for IN
mean(uncond$g_IN)
sd(uncond$g_IN)
median(uncond$g_IN)
# correlations
cor(as.vector(uncond$g_TT), as.vector(uncond$g_WT))
cor(as.vector(uncond$g_TT), as.vector(uncond$g_IN))
cor(as.vector(uncond$g_WT), as.vector(uncond$g_IN))

# Remove
rm(cond, uncond)

########################################
# END
########################################