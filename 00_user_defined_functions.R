
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# spectral analysis:  method = "ar" and "pgram", extract large spectral
# ------------------------------------------------------------------------------

spectral_udf <- function(x, method_spec){
  
  if(method_spec) { spec.out <- spectrum(x, method = "ar")} else
  { spec.out <- spectrum(x, method = "pgram") }
  
  
  # ----------
  power<- spec.out$spec
  
  frequency <- spec.out$freq
  
  cycle <- 1 / frequency

  
  # ----------
  # Sort cycles in order of magnitude of power spikes
  
  hold <- matrix(0, (length(power) - 2), 1)
  
  for(i in 1:(length(power) - 2)){
    
    max1 <- if(power[i + 1] > power[i] && power[i + 1] > power[i + 2]) 1 else (0)
    
    print(max1)
    
    hold[i,] <- max1
  }
  
  max <- which(hold == 1) + 1
  
  power.max <- power[max]
  
  cycle.max <- cycle[max]
  
  o <- order(power.max, decreasing = TRUE)
  
  cycle.max.o <- cycle.max[o]
  
  results <- list(cycle.max.o)
  
  return(results)
  
}



# ----------
# example
data(soi, package = "astsa")


# shows power (db) and frequency
results <- spectral_udf(soi, method_spec = FALSE)

results


