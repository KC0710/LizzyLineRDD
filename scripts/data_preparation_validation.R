library(here)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

source(here("scripts", "helper.R"))

sites.valid.coordinates <- read.csv(here("data",
                                         "processed",
                                         "_00_selected_AQE_KCL_sites_valid_coordinates_DROP_DUPLICATE_2023.csv"))
sites <- sites.valid.coordinates %>% select(-OpeningDate,
                                            -ClosingDate,
                                            -SamplingPoint,
                                            -Countrycode)

KCL_list_files <- list.files(path=here("data", "processed", "02_Candidate_R_data"),
                             pattern="00.*KCL.*\\.pickle", full.names=TRUE)
AQE_list_files <- list.files(path=here("data", "processed", "02_Candidate_R_data"),
                             pattern="00.*AQE.*\\.pickle", full.names=TRUE)


llub <- ymd_hms("2018-07-01 00:00:00")
ullb <- ymd_hms("2022-12-01 23:00:00")
KCLdata <- preprocess(KCL_list_files, llub, ullb)
AQEdata <- preprocess(AQE_list_files, llub, ullb)

save(KCLdata, file=here("data", "processed", "KCLdata_preprocessed.RData"))
save(AQEdata, file=here("data", "processed", "AQEdata_preprocessed.RData"))

lizzy_line_opening <- ymd_hms("2022-05-24 06:30:00")
bond_st_opening <- ymd_hms("2022-10-24 06:30:00")
lb <- ymd_hms("2018-01-01 00:00:00")
ub <- ymd_hms("2022-12-31 00:00:00")
KCLvalidpol <- lapply(KCLdata$data_list_pp, FUN=site_valid,
                      lizzy_line_opening,
                      lb,
                      ub)
AQEvalidpol <- lapply(AQEdata$data_list_pp, FUN=site_valid,
                      lizzy_line_opening, 
                      lb,
                      ub)

KCL_validated <- purrr::map2(.x=KCLdata$data_list_pp,
                             .y=KCLvalidpol,
                             .f=select_valid_sitepol)
KCL_validated <- purrr::discard(KCL_validated, function(df) nrow(df) == 0)
AQE_validated <- purrr::map2(.x=AQEdata$data_list_pp,
                             .y=AQEvalidpol,
                             .f=select_valid_sitepol)
AQE_validated <- purrr::discard(AQE_validated, function(df) nrow(df) == 0)

#remove duplicates if any
AQE_dupnames <- c("KX004", "NEW1", "NEW3", "TH001", "TH002")
KCL_dupnames <- c("KX4", "NM1", "NM3", "TH6", "TH5")

for(i in 1:5){
  print(AQE_dupnames[i])
  print(KCL_dupnames[i])
  print((AQE_dupnames[i] %in% names(KCL_validated) &
      KCL_dupnames[i] %in% names(AQE_validated)))
  print("---------------")
}

save(KCL_validated, file=here("data", "processed", "KCL_validated_sites.RData"))
save(AQE_validated, file=here("data", "processed", "AQE_validated_sites.RData"))
