# Meteorological Normalisation --------------------------------------------

library(deweather)
library(worldmet)

aq.list_validsites <- aq.list_validspan[valid_sites]

heathrow_meteo <- importNOAA(year=year_period)
meteo_vars <- c("air_temp", 
                "wd", 
                "ws",
                "atmos_pres", 
                "precip", 
                "RH") #exclude Monin-Obukov Length for now.
seasonality_vars <- c("trend", "hour", "doy")
meteo_conf <- heathrow_meteo %>% select(date,
                                        air_temp,
                                        wd,
                                        ws,
                                        atmos_pres,
                                        precip,
                                        RH) #meteorological confounders

#add meteorological confounders to air concentration data
aq.list_meteo <- lapply(aq.list_validsites, 
                        FUN=function(X){merge(x=X,
                                              y=meteo_conf,
                                              by="date",
                                              all.x=TRUE) %>% mutate(doy = yday(date))})


# Example NO2 Normalisation -----------------------------------------------

testmod <-testMod(aq.list_meteo[[1]], 
                  vars = c(meteo_vars, seasonality_vars),
                  pollutant = "no2")

mod_no2_test <- buildMod(aq.list_meteo[[1]], 
                         vars = c(meteo_vars, seasonality_vars),
                         pollutant = "no2",
                         n.core = 6)

plotAllPD(dw_model = mod_no2_test) #partial dependencies plot 

demet_no2_test <- metSim(mod_no2_test, newdata = aq.list_meteo[[1]], 
                         metVars = c(seasonality_vars, meteo_vars))

p <- ggplot(demet_no2_test, aes(date, no2)) + geom_line()