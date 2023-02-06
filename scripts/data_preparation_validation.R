library(here)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

source(here("scripts", "helper.R"))

# read in the csv file containing the selected sites
sites.valid.coordinates <- read.csv(here("data",
                                         "processed",
                                         "_00_selected_AQE_KCL_sites_valid_coordinates_DROP_DUPLICATE_2023.csv"))

# select the columns we want to keep
sites <- sites.valid.coordinates %>% select(-OpeningDate,
                                            -ClosingDate,
                                            -SamplingPoint,
                                            -Countrycode)

# read in the list of files containing the site data
KCL_list_files <- list.files(path=here("data", "processed", "02_Candidate_R_data"),
                             pattern="00.*KCL.*\\.pickle", full.names=TRUE)
AQE_list_files <- list.files(path=here("data", "processed", "02_Candidate_R_data"),
                             pattern="00.*AQE.*\\.pickle", full.names=TRUE)
# set the lower and upper bound of the time period we want to consider
llub <- ymd_hms("2018-07-01 00:00:00")
ullb <- ymd_hms("2022-12-01 23:00:00")


# preprocess the site data
KCLdata <- preprocess(KCL_list_files, llub, ullb)
AQEdata <- preprocess(AQE_list_files, llub, ullb)

# save the preprocessed site data
save(KCLdata, file=here("data", "processed", "KCLdata_preprocessed.RData"))
save(AQEdata, file=here("data", "processed", "AQEdata_preprocessed.RData"))

# set the lower and upper bound of the time period we want to consider
lizzy_line_opening <- ymd_hms("2022-05-24 06:30:00")
bond_st_opening <- ymd_hms("2022-10-24 06:30:00")
lb <- ymd_hms("2018-01-01 00:00:00")
ub <- ymd_hms("2022-12-31 00:00:00")

# check if the site data is valid
KCLvalidpol <- lapply(KCLdata$data_list_pp, FUN=site_valid,
                      lizzy_line_opening,
                      lb,
                      ub)
AQEvalidpol <- lapply(AQEdata$data_list_pp, FUN=site_valid,
                      lizzy_line_opening, 
                      lb,
                      ub)

# select the valid site data
KCL_validated <- purrr::map2(.x=KCLdata$data_list_pp,
                             .y=KCLvalidpol,
                             .f=select_valid_sitepol)
KCL_validated <- purrr::discard(KCL_validated, function(df) nrow(df) == 0)
AQE_validated <- purrr::map2(.x=AQEdata$data_list_pp,
                             .y=AQEvalidpol,
                             .f=select_valid_sitepol)
AQE_validated <- purrr::discard(AQE_validated, function(df) nrow(df) == 0)

# remove duplicates if any
AQE_dupnames <- c("KX004", "NEW1", "NEW3", "TH001", "TH002")
KCL_dupnames <- c("KX4", "NM1", "NM3", "TH6", "TH5")

for(i in 1:5){
  print(AQE_dupnames[i])
  print(KCL_dupnames[i])
  print((AQE_dupnames[i] %in% names(KCL_validated) &
      KCL_dupnames[i] %in% names(AQE_validated)))
  print("---------------")
}

# save the validated site data
save(KCL_validated, file=here("data", "processed", "KCL_validated_sites.RData"))
save(AQE_validated, file=here("data", "processed", "AQE_validated_sites.RData"))

# append the validated site data
all_validated <- append(KCL_validated, AQE_validated)
save(all_validated, file=here("data", "processed", "all_validated_sites.RData"))

# read in the weather data
meteo_data <- read.csv(here("data",
                            "processed",
                            "00_average_across_Sites_2018_2022_year_month_day_hour.csv"))
meteo_data <- meteo_data %>% rename(Year = year, Month = month, Day = day, Hour = hour)

# read in the MOL data
MOL <- read.csv(here("data",
                     "processed",
                     "00_LDN_2018_2022_MOL_result.csv"))

# merge the weather and MOL data
meteo_full <- merge(meteo_data, MOL,
                    by = c("Year", "Month", "Day", "Hour")) %>% 
  arrange(Year, Month, Day, Hour) %>%
  mutate(date = ymd_h(paste(Year, Month, Day, Hour, sep = " ")))

save(meteo_full, file=here("data", "processed", "meteo_full.RData"))
