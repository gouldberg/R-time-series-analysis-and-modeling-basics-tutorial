
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# raw periodogram
# ------------------------------------------------------------------------------

library(astsa)


graphics.off()

par(mfcol = c(2,3))


mvspec(hakusan[,"YawRate"], main = "船舶の方向角速度", ylab = "", log = "yes", type = "h")


mvspec(sunspot, main = "太陽黒点数データ", ylab = "", log = "yes", type = "h")


mvspec(maxtemp, main = "東京の日最高気温データ", ylab = "", log = "yes", type = "h")


mvspec(blsfood, main = "アメリカの食品産業に従事する労働者人口", ylab = "", log = "yes", type = "h")


mvspec(whard, main = "工業製品の卸売り高", ylab = "", log = "yes", type = "h")


mvspec(mye1f, main = " 地震データ(東西方向)", ylab = "", log = "yes", type = "h")




# ----------
mvspec(nikkei225, main = "日経225", ylab = "", log = "yes", type = "h")


mvspec(linx, main = "カナダオオヤマネコ生息数", ylab = "", log = "yes", type = "h")


mvspec(rainfall, main = "東京降水量データ", ylab = "", log = "yes", type = "h")




# ----------
# mvspec(haibara[,c("地下水位", "気圧")], log = "yes", type = "h")




# ----------
graphics.off()

par(mfcol = c(2,2))

mvspec(hakusan[,c("Rolling")], log = "yes", type = "h")

mvspec(hakusan[,c("Pitching")], log = "yes", type = "h")

mvspec(hakusan[,c("Rudder")], log = "yes", type = "h")




# ------------------------------------------------------------------------------
# smoothed spetral
# ------------------------------------------------------------------------------

( ker <- kernel("modified.daniell", c(9,9)) )

plot(ker)




# ----------
graphics.off()

par(mfcol = c(2,3))


mvspec(hakusan[,"YawRate"], main = "船舶の方向角速度", ylab = "", log = "yes", type = "h")
mvspec(hakusan[,"YawRate"], main = "船舶の方向角速度", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(sunspot, main = "太陽黒点数データ", ylab = "", log = "yes", type = "h")
mvspec(sunspot, main = "太陽黒点数データ", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(maxtemp, main = "東京の日最高気温データ", ylab = "", log = "yes", type = "h")
mvspec(maxtemp, main = "東京の日最高気温データ", ylab = "", log = "yes", type = "l", kernel = ker)



# ----------
mvspec(blsfood, main = "アメリカの食品産業に従事する労働者人口", ylab = "", log = "yes", type = "h")
mvspec(blsfood, main = "アメリカの食品産業に従事する労働者人口", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(whard, main = "工業製品の卸売り高", ylab = "", log = "yes", type = "h")
mvspec(whard, main = "工業製品の卸売り高", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(mye1f, main = " 地震データ(東西方向)", ylab = "", log = "yes", type = "h")
mvspec(mye1f, main = " 地震データ(東西方向)", ylab = "", log = "yes", type = "l", kernel = ker)




# ----------
mvspec(nikkei225, main = "日経225", ylab = "", log = "yes", type = "h")
mvspec(nikkei225, main = "日経225", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(linx, main = "カナダオオヤマネコ生息数", ylab = "", log = "yes", type = "h")
mvspec(linx, main = "カナダオオヤマネコ生息数", ylab = "", log = "yes", type = "l", kernel = ker)


mvspec(rainfall, main = "東京降水量データ", ylab = "", log = "yes", type = "h")
mvspec(rainfall, main = "東京降水量データ", ylab = "", log = "yes", type = "l", kernel = ker)




# ----------
# mvspec(haibara[,c("地下水位", "気圧")], log = "yes", type = "h")




# ----------
graphics.off()

par(mfcol = c(2,3))

mvspec(hakusan[,c("Rolling")], log = "yes", type = "h")
mvspec(hakusan[,c("Rolling")], log = "yes", type = "l", kernel = ker)

mvspec(hakusan[,c("Pitching")], log = "yes", type = "h")
mvspec(hakusan[,c("Pitching")], log = "yes", type = "l", kernel = ker)

mvspec(hakusan[,c("Rudder")], log = "yes", type = "h")
mvspec(hakusan[,c("Rudder")], log = "yes", type = "l", kernel = ker)
