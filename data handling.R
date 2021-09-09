rm(list=ls())
library(plm)
library(stargazer)
library(tidyverse)
options(scipen = 10)

setwd("C:/Users/user/Dropbox/Uni study file/Sem 5/ECON 4274/Paper/Ticker Symbol/Final stage/19Dec")
df = read.csv('Master Excel - Ready to R.csv', na.strings = '#N/A N/A')
df_backup = df
df = df_backup
head(df)
df = df[,-1]

nrow(df)
# Create Levearge variable and Operating Cash Flow
df$CAPEX = abs(df$CAPEX)
df$OCF = df$FCF + df$CAPEX
df$CAPEX[df$CAPEX==0] = NA
df$lCAPEX = log(df$CAPEX)

df$Invest = round(df$CAPEX/df$OCF, 3)
df$Invest = abs(df$Invest)                       # this is risky but abs also reflect the proportion, meaning not changed
df$Leverage = round(df$LT_Debt/df$Asset, 3)
max(df$Leverage, na.rm = T)

# All variables lag one period
df$Invest[2:nrow(df)] = df$Invest[1:nrow(df)-1]
df[df$Year=="1995",]$Invest = NA

df$Capital[2:nrow(df)] = df$Capital[1:nrow(df)-1]
df[df$Year=="1995",]$Capital = NA

df$Asset[2:nrow(df)] = df$Asset[1:nrow(df)-1]
df[df$Year=="1995",]$Asset = NA

df$ST_Debt[2:nrow(df)] = df$ST_Debt[1:nrow(df)-1]
df[df$Year=="1995",]$ST_Debt = NA

df$LT_Debt[2:nrow(df)] = df$LT_Debt[1:nrow(df)-1]
df[df$Year=="1995",]$LT_Debt = NA

df$FCF[2:nrow(df)] = df$FCF[1:nrow(df)-1]
df[df$Year=="1995",]$FCF = NA

df$CAPEX[2:nrow(df)] = df$CAPEX[1:nrow(df)-1]
df[df$Year=="1995",]$CAPEX = NA

# 2. Leverage lag 1, 3, 5, 10
df$Lev_lag1 = -1
df$Lev_lag1[2:nrow(df)] = df$Leverage[1:nrow(df)-1]
df[df$Year=="1995",]$Lev_lag1 = NA

df$Lev_lag3 = -1
df$Lev_lag3[4:nrow(df)] = df$Leverage[1:(nrow(df)-3)]
df[df$Year<1998,]$Lev_lag3 = NA

df$Lev_lag5 = -1
df$Lev_lag5[6:nrow(df)] = df$Leverage[1:(nrow(df)-5)]
df[df$Year<2000,]$Lev_lag5 = NA

df$Lev_lag10 = -1
df$Lev_lag10[11:nrow(df)] = df$Leverage[1:(nrow(df)-10)]
df[df$Year<2005,]$Lev_lag10 = NA



# creating variable
# 1. log sales, log NI, log Total Capital, log Asset
# need to handle 0 sales case
df$Sales[df$Sales==0] = NA
df$lRv = log(df$Sales)
which(df$lRv == -Inf)

df$NI[df$NI==0] = NA
df$lNI = log(df$NI)

df$Capital[df$Capital==0] = NA
df$lCap = log(df$Capital)

df$Asset[df$Asset==0] = NA
df$lAsset = log(df$Asset)


# 6. create dummy variable, is EM, is CH
df$is_em = FALSE
df$is_em[which(df$Mkt == 'VN' & df$is_em == FALSE)] = TRUE
df$is_em[which(df$Mkt == 'CH' & df$is_em == FALSE)] = TRUE
df$is_em[which(df$Mkt == 'MK' & df$is_em == FALSE)] = TRUE
df$is_em[which(df$Mkt == 'TB' & df$is_em == FALSE)] = TRUE
df$is_em[which(df$Mkt == 'IN' & df$is_em == FALSE)] = TRUE
df$is_em[which(df$Mkt == 'IJ' & df$is_em == FALSE)] = TRUE
tail(df[df$is_em,])

plot(Leverage~Year, data=subset(df, is_em=TRUE), ylim=c(0,2))

clean_df = df %>% select(Firmid, Year, lRv, lNI,lCap, lAsset, Invest, lCAPEX, Lev_lag1, Lev_lag3,
              Lev_lag5, Lev_lag10, Mkt, is_em, N_Yr_Listed, Sector_Code)

clean_df = clean_df[complete.cases(clean_df),]

write.csv(clean_df, "asiafirms2.csv", row.names = F)

#------------------------------------------------------------------------------
#                               Models
#------------------------------------------------------------------------------
rm(list=ls())
cdf = read.csv('asiafirms4.csv')
head(cdf)

pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)
head(pdf)

m8 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
         +Invest+lAsset+lCap+N_Yr_Listed+is_em, data=pdf, model='pooling')
summary(m8)

fe9 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+N_Yr_Listed+is_em, data=pdf, model='within',effect='time')

re9 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+N_Yr_Listed+is_em, data=pdf, model='random')

# bp test
plmtest(re9, type='bp')
# There is significant effect, RE > FE

phtest(fe9, re9)
# Two model is not consistent, we prefer 

fe1 = plm(lRv~Lev_lag1, data=pdf, model='within',effect='time')
fe2 = plm(lRv~Lev_lag1+Lev_lag3, data=pdf, model='within',effect='time')
fe3 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5, data=pdf, model='within',effect='time')
fe4 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10, data=pdf, model='within',effect='time')
# Add control variables
fe5 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest, data=pdf, model='within',effect='time')
fe6 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset, data=pdf, model='within',effect='time')
fe7 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap, data=pdf, model='within',effect='time')
fe8 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+lCAPEX, data=pdf, model='within',effect='time')
fe9 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed, data=pdf, model='within',effect='time')
fe10 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=pdf, model='within',effect='time')
fe11 = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=pdf, model='within',effect='time')
stargazer(fe1,fe4,fe5,fe6,fe7,fe8,fe9,fe10, type='html', out='overall_lNI.html')

Fe1 = plm(lNI~Lev_lag1, data=pdf, model='within',effect='time')
Fe2 = plm(lNI~Lev_lag1+Lev_lag3, data=pdf, model='within',effect='time')
Fe3 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5, data=pdf, model='within',effect='time')
Fe4 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10, data=pdf, model='within',effect='time')
# Add control variables
Fe5 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest, data=pdf, model='within',effect='time')
Fe6 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset, data=pdf, model='within',effect='time')
Fe7 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap, data=pdf, model='within',effect='time')
Fe8 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+lCAPEX, data=pdf, model='within',effect='time')
Fe9 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
          +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed, data=pdf, model='within',effect='time')
Fe10 = plm(lNI~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
           +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=pdf, model='within',effect='time')

stargazer(Fe1,Fe4,Fe5,Fe6,Fe7,Fe8,Fe9,Fe10, type='html', out='overall_lNI.html')

stargazer(fe1,fe4,fe5,fe6,fe7,fe8,fe9,fe10,Fe1,Fe4,Fe5,Fe6,Fe7,Fe8,Fe9,Fe10, type='html', out='overallLL.html')

fixef(m1)

# DID

# evaluating leverages
mean_lev_seq = NULL

for (i in 1:nrow(pdf)){
  if (pdf[i,]$Year==2019){
    mean_lev_seq = rbind(mean_lev_seq, c(pdf[i,1],sum(pdf[i,9:12])/4))
  }
}
mean_lev_seq_nz = mean_lev_seq[mean_lev_seq[,2]>0,]
write.csv(mean_lev_seq, 'mean_lev_seq backup.csv', row.names = F)
write.csv(mean_lev_seq_nz, 'mean_lev_seq_nz backup.csv', row.names = F)

#should use non-zero

plot(density(mean_lev_seq_non_zero[,2]), main='Density of average leverage at 2019')
hist(mean_lev_seq_non_zero[,2], breaks=25)


summary(mean_lev_seq_nz[,2])

low_lev = 0.02075 
hi_lev = 0.15025 

sum(mean_lev_seq_nz[,2]<=low_lev)
sum(mean_lev_seq_nz[,2]>=hi_lev)

mean_lev_seq_nz = cbind(mean_lev_seq_nz, 0)

for (i in 1:nrow(mean_lev_seq)){
  if(mean_lev_seq[i,2]<=low_lev){
    mean_lev_seq[i,3] = 1              # 1 for low
  } else if(mean_lev_seq[i,2]>=hi_lev){
    mean_lev_seq[i,3] = 2              # 2 for hi 
  }
}

# DiD Model
cdf = read.csv('asiafirms2.csv')
# 
# temp = cdf$Lev_lag10
# temp[temp==0] = NA
# summary(temp)
# sum(temp < 0.03, na.rm = T)
# temp[is.na(temp)] = 0
# temp = temp > 0.18
# length(temp) ; nrow(cdf)
# 
# cdf$D_Hi_Lev = 1*temp

treatment_mean = NULL

i = 1
for (j in 1995:2020){
  treatment_mean = rbind(treatment_mean, c(i, j, mean(cdf[cdf$Year == j & cdf$D_Hi_Lev==i,]$lRv, na.rm = T)))
}

control_mean = NULL
i = 0
for (j in 1995:2020){
  control_mean = rbind(control_mean, c(i, j, mean(cdf[cdf$Year == j & cdf$D_Hi_Lev==i,]$lRv, na.rm = T)))
}

plot(treatment_mean[,3]~treatment_mean[,2], type='l', lwd=2,
     ylim=c(4, 6.5), xlim=c(2000, 2020),
     main="Mean lRevenue Across Time",
     ylab='Average lRevenue', xlab='Year')
lines(control_mean[,3]~control_mean[,2], col=4, lwd=2)
points(treatment_mean[,3]~treatment_mean[,2], pch=16)
points(control_mean[,3]~control_mean[,2], col=4, pch=16)
abline(v=2008, col=2, lty=5)
legend('topleft',c('Hi_Lev Group', 'Not Hi_Lev Group'), col=c(1,4), lwd=c(2,2))

# cdf$D_After = 1*(cdf$Year >= 2009)
cdf = read.csv('asiafirms4.csv')
cdf = cdf[cdf$Year<2020,]

pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)
head(pdf)
length(unique(pdf[pdf$D_Hi_Lev == 1,]$Firmid))


did1 = plm(lRv~D_After+D_Hi_Lev+D_After*D_Hi_Lev, data=pdf, model='within')
did2 = plm(lRv~D_After+D_Hi_Lev+D_After*D_Hi_Lev
           +lAsset, data=pdf, model='within')
did3 = plm(lRv~D_After+D_Hi_Lev+D_After*D_Hi_Lev
           +lAsset+lCap, data=pdf, model='within')
did4 = plm(lRv~D_After+D_Hi_Lev+D_After*D_Hi_Lev
           +lAsset+lCap+lCAPEX, data=pdf, model='within')
did5 = plm(lRv~D_After+D_Hi_Lev+D_After*D_Hi_Lev
           +lAsset+lCap+lCAPEX+N_Yr_Listed, data=pdf, model='within')
did6 = plm(lRv~D_After+D_Hi_Lev+D_After*D_Hi_Lev
           +lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=pdf, model='within')
summary(did6)
stargazer(did1, did2, did3, did4, did5, did6, type='html', out='didtab2.html')


# Market specific effect
rm(list=ls())
cdf = read.csv('asiafirms4.csv')
# cdf$is_ch = 1*(cdf$Mkt == 'CH')
# cdf$is_tigers = 1*(cdf$Mkt == 'HK'| cdf$Mkt == 'SP'|cdf$Mkt == 'KS'|cdf$Mkt == 'TT')
# cdf$is_asean = 1*(cdf$Mkt == 'IJ'| cdf$Mkt == 'MK'|cdf$Mkt == 'SP'|cdf$Mkt == 'TB'|cdf$Mkt == 'VN')
pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)

# m1 = plm(lRv~Lev_lag1+Lev_lag1*is_ch+Lev_lag5+Lev_lag5*is_ch
#          +Lev_lag10+Lev_lag10*is_ch+lAsset+lCap+lCAPEX, data=pdf, model='within')
m1 = plm(lRv~Lev_lag1*is_ch+Lev_lag5*is_ch
         +Lev_lag10*is_ch+lAsset+lCap+lCAPEX*is_ch+N_Yr_Listed, data=pdf, model='within',effect='time')
m2 = plm(lRv~Lev_lag1*is_em+Lev_lag5*is_em
         +Lev_lag10*is_em+lAsset+lCap+lCAPEX*is_em+N_Yr_Listed, data=pdf, model='within',effect='time')
m3 = plm(lRv~Lev_lag1*is_tigers+Lev_lag5*is_tigers
         +Lev_lag10*is_tigers+lAsset+lCap+lCAPEX*is_tigers+N_Yr_Listed, data=pdf, model='within',effect='time')
m4 = plm(lRv~Lev_lag1*is_asean+Lev_lag5*is_asean
         +Lev_lag10*is_asean+lAsset+lCap+lCAPEX*is_asean+N_Yr_Listed, data=pdf, model='within',effect='time')
stargazer(m1, m2, m3, m4, type='html', out='geotab1.html')

# Time fixed effect and Market specific effect
cdf = read.csv('asiafirms3.csv')
cdf=cdf[complete.cases(cdf),]
pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)
head(pdf)

fe_ov = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
           +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=pdf, model='within',effect='time')
summary(fe_ov)
fixef(fe_ov)


cdf = subset(pdf, Mkt=='CH')
fe_ch = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='HK')
fe_hk = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='AU')
fe_au = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='KS')
fe_ks = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='JP')
fe_jp = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='TT')
fe_tt = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='SP')
fe_sp = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='TB')
fe_tb = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='IN')
fe_in = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='IJ')
fe_ij = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='MK')
fe_mk = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')
cdf = subset(pdf, Mkt=='VN')
fe_vn = plm(lRv~Lev_lag1+Lev_lag3+Lev_lag5+Lev_lag10
            +Invest+lAsset+lCap+lCAPEX+N_Yr_Listed+is_em, data=cdf, model='within',effect='time')

fe_ov = fixef(fe_ov)

fe_ch = fixef(fe_ch)
fe_hk = fixef(fe_hk)
fe_ks = fixef(fe_ks)
fe_jp = fixef(fe_jp)
fe_tt = fixef(fe_tt)
fe_sp = fixef(fe_sp)

fe_au = fixef(fe_au)
fe_tb = fixef(fe_tb)
fe_in = fixef(fe_in)
fe_ij = fixef(fe_ij)
fe_vn = fixef(fe_vn)
fe_mk = fixef(fe_mk)

regions = c('ch','hk','ks','jp','tt','sp','au','tb','in','ij','mk','vn')
year = 2005:2019

fef_seq = NULL

# [1] "ch" "15"
# [1] "hk" "16"
# [1] "ks" "16"
# [1] "jp" "16"
# [1] "tt" "15"
# [1] "sp" "16"
# [1] "au" "16"
# [1] "tb" "15"
# [1] "in" "16"
# [1] "ij" "15"
# [1] "vn" "5"
# [1] "mk" "16"
par(mfrow=c(1,2))
plot(2005:2020, fe_ov, type ='l',lwd=2, col=2, lty=2, ylim=c(-0.5,2.8),
     main='East Asia', xlab='Year', ylab='Year Fixed Effect')
lines(2005:2019, fe_ch, col=1, lwd=2)
lines(2005:2020, fe_hk, col=2, lwd=2)
lines(2005:2020, fe_ks, col=3, lwd=2)
lines(2005:2020, fe_jp, col=4, lwd=2)
lines(2005:2019, fe_tt, col=5, lwd=2)
lines(2005:2020, fe_au, col=8, lwd=2)
abline(v=2008)
abline(v=2015)
abline(v=2019)
legend('topleft', c('CH','HK','KS','JP','TT','AU','Overall'), col=c(1,2,3,4,5,8,2),
       lty=c(rep(1,6),2),lwd=2, cex=0.8,text.width = 1)

plot(2005:2020, fe_ov, type ='l',lwd=2, col=2, lty=2, ylim=c(-0.5,2.8),
     main='South Asia', xlab='Year', ylab='Year Fixed Effect')
lines(2005:2019, fe_tb, col=1, lwd=2)
lines(2005:2020, fe_in, col=2, lwd=2)
lines(2005:2019, fe_ij, col=3, lwd=2)
lines(2005:2020, fe_mk, col=4, lwd=2)
lines(2015:2019, fe_vn, col=5, lwd=2)
lines(2005:2020, fe_sp, col=8, lwd=2)
abline(v=2008)
abline(v=2015)
abline(v=2019)
legend('topleft', c('TB','IN','IJ','MK','VN','SP','Overall'), col=c(1,2,3,4,5,8,2),
       lty=c(rep(1,6),2),lwd=2, cex=0.8,text.width = 1)

par(mfrow=c(1,1))

# Summary statistic table
cdf = read.csv('asiafirms4.csv')
pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)
regions = c('CH','HK','KS','JP','TT','SP','AU','TB','IN','IJ','MK','VN')

for (i in regions){
  cdf = subset(pdf, Mkt==i)
  # stargazer(cdf, type='html', out=paste0('summary_',i,'.html'))
  print(nrow(cdf))
}
stargazer(pdf, type='html', out='summary.html')

# c('HK','KS','JP','TT','AU','SP')
cdf = subset(pdf, is_em==0)
stargazer(cdf, type='html', out=paste0('summary_DM.html'))

cdf = subset(pdf, is_em==1)
stargazer(cdf, type='html', out=paste0('summary_EM.html'))

# Correcting the N_Yr_Listed variable
# cdf = read.csv('asiafirms3.csv')
# cdf=cdf[complete.cases(cdf),]
# for (i in 1:nrow(cdf)){
#   cdf[i,]$N_Yr_Listed = (cdf[i,]$N_Yr_Listed-(2020-cdf[i,]$Year)+1)
# }
# write.csv(cdf, 'asiafirm4.csv', row.names = F)

# Average leverage variable
# cdf$Avg_lev = (cdf$Lev_lag1+cdf$Lev_lag3+cdf$Lev_lag5+cdf$Lev_lag10)/4
# write.csv(cdf, 'asiafirms3.csv', row.names = F)
cdf = read.csv('asiafirms4.csv')
pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)

par(mfrow=c(2,6))

x = subset(pdf, Mkt == 'HK')
x = sort(x$Avg_lev, decreasing = T)
x = x[10:length(x)]
hist(x, breaks=10, main = paste('HK', 'Average Leverage'), xlab='Avg_lev', plot=T)

x = subset(pdf, Mkt == 'AU')
x = sort(x$Avg_lev, decreasing = T)
x = x[2:length(x)]
hist(x, breaks=10, main = paste('AU', 'Average Leverage'), xlab='Avg_lev', plot=T)

for (i in c('KS','JP','TT')){
  x = subset(pdf, Mkt == i)
  hist(x$Avg_lev, breaks=10, main = paste(i, 'Average Leverage'), xlab='Avg_lev', plot=T)
}

x = subset(pdf, Mkt == 'SP')
x = sort(x$Avg_lev, decreasing = T)
x = x[3:length(x)]
hist(x, breaks=10, main = paste('SP', 'Average Leverage'), xlab='Avg_lev', plot=T)

for (i in c('CH','IN','IJ','MK','VN')){
  x = subset(pdf, Mkt == i)
  hist(x$Avg_lev, breaks=10, main = paste(i, 'Average Leverage'), xlab='Avg_lev')
}

x = subset(pdf, Mkt == 'TB')
x = sort(x$Avg_lev, decreasing = T)
x = x[5:length(x)]
hist(x, breaks=10, main = paste('TB', 'Average Leverage'), xlab='Avg_lev', plot=T)





par(mfrow=c(1,2))
x = pdf[pdf$Mkt =='HK'|pdf$Mkt =='KS'|pdf$Mkt =='TT'|pdf$Mkt =='SP',]
y = t(table(x$Sector_Code,x$Mkt))
y = y / rowSums(y)
barplot(y, main='Four Asian Tigers', xlab='Sectors', ylab='% among economy', beside = T,
        col=c(2,3,4,5),legend = rownames(t(table(x$Sector_Code,x$Mkt))))

x = pdf[pdf$Mkt =='TB'|pdf$Mkt =='SP'|pdf$Mkt =='IJ'|pdf$Mkt =='MK'|pdf$Mkt=='VN',]
y = t(table(x$Sector_Code,x$Mkt))
y = y / rowSums(y)
barplot(y, main='ASEAN', xlab='Sectors', ylab='% among economy', beside = T,
        col=c(2,3,4,5),legend = rownames(t(table(x$Sector_Code,x$Mkt))))

x = pdf[pdf$Mkt =='CH'|pdf$Mkt =='AU'|pdf$Mkt=='IN',]
y = t(table(x$Sector_Code,x$Mkt))
y = y / rowSums(y)
barplot(y, main='CH, AU, IN', beside = T,
        col=c(2,3,4,5),legend = rownames(t(table(x$Sector_Code,x$Mkt))))
par(mfrow=c(1,1))



# General leverage trend
rm(list=ls())
cdf = read.csv('asiafirms2.csv')
pdf = pdata.frame(cdf, index = c('Firmid', 'Year'), row.names = F, drop.NA.series = T)
x = subset(cdf, select=c(Year, Leverage_lag1, is_em))

y_seq = 1995:2020
y_seq = cbind(y_seq, lev = 0)
y_seq = cbind(y_seq, num = 0)

for(i in 1:nrow(x)){
  roww = (x[i, ]$Year - 1994)
  if (!is.na(x[i,2]) & x[i,]$Leverage_lag1 < 100){
    y_seq[roww,2] = y_seq[roww,2] + x[i,]$Leverage_lag1
    y_seq[roww,3] = y_seq[roww,3] + 1
  }
}

y_seq = y_seq[2:(nrow(y_seq)-1),]
lev_avg = y_seq[,2]/y_seq[,1]
# plot(1996:2019, lev_avg, type='l', xlab='Year', ylab='Avg Leverage', main='Average Leverage Trend', ylim=c(0,0.4))
# abline(v=c(1997,2008))

y_em_seq = 1995:2020
y_em_seq = cbind(y_em_seq, lev = 0)
y_em_seq = cbind(y_em_seq, num = 0)

y_dm_seq = 1995:2020
y_dm_seq = cbind(y_dm_seq, lev = 0)
y_dm_seq = cbind(y_dm_seq, num = 0)

for(i in 1:nrow(x)){
  roww = (x[i, ]$Year - 1994)
  if (!is.na(x[i,2]) & x[i,]$Leverage_lag1 < 100){
    if(x[i,]$is_em==1){
      y_em_seq[roww,2] = y_em_seq[roww,2] + x[i,]$Leverage_lag1
      y_em_seq[roww,3] = y_em_seq[roww,3] + 1
    }
    else{
      y_dm_seq[roww,2] = y_dm_seq[roww,2] + x[i,]$Leverage_lag1
      y_dm_seq[roww,3] = y_dm_seq[roww,3] + 1
    }
  }
}

y_em_seq = y_em_seq[2:(nrow(y_em_seq)-1),]
y_dm_seq = y_dm_seq[2:(nrow(y_dm_seq)-1),]
lev_em_avg = y_em_seq[,2]/y_em_seq[,1]
lev_dm_avg = y_dm_seq[,2]/y_dm_seq[,1]
plot(1996:2019, lev_avg, type='l', xlab='Year', ylab='Avg Leverage', main='Average Leverage Trend',
     ylim=c(0,0.4), lwd=2)
lines(1996:2019, lev_em_avg, col=2, lwd=2)
lines(1996:2019, lev_dm_avg, col=4, lwd=2)
abline(v=c(1997,2008), lty=2, col=8, lwd=2)
legend('topleft', c('Overall', 'DM','EM'), col=c(1,4,2),lwd=2)
