# df_mean = df.mean()
# dfm = dffillna(df_mean)
# print(dfm)

# 결측치 제거 : dropna(axis=0),  dropna(axis=1)

# NCS 05 PDF p71
phone02 <- read.table('c:/Java/phone-02.csv', header=F, sep=',')
phone02

df_phone02 <- phone02
df_phone02[2, 7] <- NA
df_phone02[4, 7] <- NA
df_phone02[17, 7] <- NA
df_phone02

is.na(df_phone02[,7])
!is.na(df_phone02[,7])
