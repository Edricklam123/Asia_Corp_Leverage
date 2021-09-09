library(readxl)
df = read_excel('HKEX.xlsx')
df = read.csv('HKEX.csv')

setwd("C:/Users/dickl/Dropbox/Uni study file/Sem 5/ECON 4274/Paper/Ticker Symbol/300 tickers")

df = read.csv('HK_300.csv')
nrow(df)
select = sample(1:nrow(df),600)
df = df[select,]
write.csv(df,'HK_300.csv')

df = read.csv('CH_SH.csv')
head(df); ncol(df)
df = df[,-9]
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'CH_SH_300.csv')

df = read.csv('CH_SZ.csv')
head(df); nrow(df)
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'CH_SZ_300.csv')

df = read.csv('SZEX.csv')
head(df)
select = sample(1:nrow(df),300)
select = sort(select)
df = df[select,]
write.csv(df,'SZ_300.csv')

df = read.csv('SG_300.csv')
head(df); nrow(df)

df = read.csv('Thailand tickers.csv')
head(df)
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'Thai_300.csv', row.names = F)

df = read.csv('Malaysia.csv')
head(df)
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'Malay_300.csv', row.names = F)

df = read.csv('NSE.csv')
head(df)
df = df[as.integer(str_sub(df[,4], -2,-1)) < 10,]
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'NSE_300.csv', row.names = F)

df = read.csv('INDO.csv')
head(df)
df = df[as.integer(str_sub(df[,4], -2,-1)) < 18,]
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'INDO_300.csv', row.names = F)

df = read.csv('TT_raw.csv')
head(df); nrow(df)
select = sample(1:nrow(df),600)
df = df[select,]
write.csv(df,'TT_600.csv', row.names = F)

df = read.csv('VN.csv')
head(df)
nrow(df[complete.cases(df),])
df = df[complete.cases(df),]
select = sample(1:nrow(df),300)
df = df[select,]
write.csv(df,'VN_300.csv', row.names = F)

df = read.csv('AU_300.csv')
head(df) ; nrow(df)
select = sample(1:nrow(df),600)
df = df[select,]
write.csv(df,'AU_300.csv', row.names = F)

df = read.csv('IN_raw.csv')
head(df); nrow(df)
write.csv(df,'IN_300.csv', row.names = F)

df = read.csv('JP_raw.csv')
head(df); nrow(df)
select = sample(1:nrow(df),600)
df = df[select,]
write.csv(df,'JP_600.csv', row.names = F)

df = read.csv('KR_raw.csv')
head(df); nrow(df)
select = sample(1:nrow(df),600)
df = df[select,]
write.csv(df,'KR_600.csv', row.names = F)
