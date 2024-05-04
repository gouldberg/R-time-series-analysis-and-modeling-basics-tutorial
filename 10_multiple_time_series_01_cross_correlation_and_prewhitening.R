
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# cross correlation of SOI and REC
# ------------------------------------------------------------------------------


data(soi, package = "astsa")

data(rec, package = "astsa")





