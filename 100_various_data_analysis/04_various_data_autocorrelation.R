
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# auto-correlation
# ------------------------------------------------------------------------------

graphics.off()

par(mfcol = c(2,3))


acf(hakusan[,"YawRate"], main = "船舶の方向角速度", ylab = "", lag.max = 50)


acf(sunspot, main = "太陽黒点数データ", ylab = "", lag.max = 50)


acf(maxtemp, main = "東京の日最高気温データ", ylab = "", lag.max = 50)


acf(blsfood, main = "アメリカの食品産業に従事する労働者人口", ylab = "", lag.max = 50)


acf(whard, main = "工業製品の卸売り高", ylab = "", lag.max = 50)


acf(mye1f, main = " 地震データ(東西方向)", ylab = "", lag.max = 50)




# ----------
acf(nikkei225, main = "日経225", ylab = "", lag.max = 50)


acf(linx, main = "カナダオオヤマネコ生息数", ylab = "", lag.max = 50)


acf(rainfall, main = "東京降水量データ", ylab = "", lag.max = 50)




# ----------
# acf(haibara[,c("地下水位", "気圧")], lag.max = 50)

car::scatterplotMatrix(formula = ~ 地下水位 + 気圧, data = haibara, smooth = FALSE)




# ------------------------------------------------------------------------------
# autocorrelation and cross correlation
# ------------------------------------------------------------------------------

acf(hakusan[,c("Rolling", "Pitching", "Rudder")], lag.max = 50)


# -->
# ACF of Rolling and Rudder are very similar


car::scatterplotMatrix(formula = ~ Rolling + Pitching + Rudder, data = hakusan, smooth = FALSE)



