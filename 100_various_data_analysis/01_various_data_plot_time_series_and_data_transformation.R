setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# read data
# ------------------------------------------------------------------------------


hakusan <- as.ts(read.csv(".\\data\\HAKUSAN.txt", header = T, sep = "\t"))

sunspot <- as.ts(read.csv(".\\data\\Sunspot.txt", header = T, sep = "\t"))

maxtemp <- as.ts(read.csv(".\\data\\maxtemp.txt", header = T, sep = "\t"))

blsfood <- as.ts(read.csv(".\\data\\BLSALLFOOD.txt", header = T, sep = "\t"))

whard <- as.ts(read.csv(".\\data\\WHARD.txt", header = T, sep = "\t"))

nikkei225 <- as.ts(read.csv(".\\data\\nikkei225.txt", header = T, sep = "\t"))

mye1f <- as.ts(read.csv(".\\data\\MYE1F.txt", header = T, sep = "\t"))

haibara <- as.ts(read.csv(".\\data\\haibara.txt", header = T, sep = "\t"))

linx <- as.ts(read.csv(".\\data\\Lynx.txt", header = T, sep = "\t"))

temperature <- as.ts(read.csv(".\\data\\Temperature.txt", header = T, sep = "\t"))

# average in 2 years
rainfall <- as.ts(read.csv(".\\data\\rainfall.txt", header = T, sep = "\t") / 2)



# ----------
head(hakusan)

head(sunspot)

head(maxtemp)

head(blsfood)

head(whard)

head(nikkei225)

head(mye1f)

head(haibara)

head(linx)

head(temperature)

head(rainfall)




# ------------------------------------------------------------------------------
# plot all data:  various characteristics
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,3))


# stationary, periodic
plot(hakusan[,"YawRate"], main = "船舶の方向角速度", ylab = "")


# positive, asymmetric (upper, lower, right, left), Periodic
plot(sunspot, main = "太陽黒点数データ", ylab = "")


# trend (long-term Periodic),  stationary around trend
plot(maxtemp, main = "東京の日最高気温データ", ylab = "")


# yearly periodic, trend
plot(blsfood, main = "アメリカの食品産業に従事する労働者人口", ylab = "")



# yearly periodic, positive, trend + variance increasing
plot(whard, main = "工業製品の卸売り高", ylab = "")



# No trend, variance non-stationary, covariance non-stationary, locally stationary
plot(mye1f, main = " 地震データ(東西方向)", ylab = "")




# ----------
# trend, variance change, increasing in down trending
plot(nikkei225, main = "日経225", ylab = "")



# mean stationary, periodic, asymmetric (left, right)
plot(linx, main = "カナダオオヤマネコ生息数", ylab = "")



# discrete, mean probability non-stationary
plot(rainfall, main = "東京降水量データ", ylab = "")




# ----------
# multi-variate, negative cross-correlation, outliers, missing values
plot(haibara[,c("地下水位", "気圧")], main = "榛原地下水位データ")



# ----------
# multi-variate
# Input:  Rudder
# Output: Rolling and Pitching
plot(hakusan[,c("Rolling", "Pitching", "Rudder")], main = "船舶データ 縦揺れ 横揺れ 舵角")




# ------------------------------------------------------------------------------
# Data Transformation for WHARD
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(3,2))



# ----------
plot(whard, main = "original WHARD")

plot(log(whard), main = "log")

plot(diff(log(whard)), main = "log and diff")




# ----------
plot(whard, main = "original WHARD")

plot(diff(log(whard)), main = "log and diff")

plot(diff(log(whard), 12), main = "log and diff12 (seasonalily removed)")




# ----------
plot(whard, main = "original WHARD")

plot(whard / stats::lag(whard, -1), main = "ratio to previous")

plot(whard / stats::lag(whard, -12), main = "ratio to previous year")




# ------------------------------------------------------------------------------
# Data Transformation for nikkei225
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(3,1))

plot(nikkei225, main = "original nikkei225")

plot(log(nikkei225), main = "log")

plot(diff(log(nikkei225)), main = "log and diff")



