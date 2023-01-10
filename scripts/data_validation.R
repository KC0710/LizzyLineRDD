library(openair)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)


# Data Loading ------------------------------------------------------------
load("sites.RData")

#defining latitude and longitude of LAQN
lat.min <- 51.288768; lat.max <- 51.693059 
lng.min <- -0.510434; lng.max <- 0.337168

sites.laqn <- sites %>% drop_na(Latitude, Longitude) %>% 
  filter((Latitude <= lat.max & Latitude >= lat.min) &
                                      (Longitude <= lng.max & Longitude >= lng.min))
site.code <- sites.laqn$SiteCode 

lizzy_line_opening <- ymd_hms("2022-05-24 06:30:00")
bond_street_opening <- ymd_hms("2022-10-24 06:30:00")
start <- year(lizzy_line_opening - months(39))
end <- year(now())
opening_bondst_period <- start:year(bond_street_opening)

aq.list_laqn_prebondst <- importKCL(site=site.code,
                                    year=opening_bondst_period,
                                    pollutant="all")

save(aq.list_laqn_prebondst, file="aq.list_laqn_raw_prebondst.RData")

# Duplicates --------------------------------------------------------------
load(here("data", "raw", "aq.list_laqn_raw_prebondst.RData"))

aq.list_laqn <- split(aq.list_laqn_prebondst, aq.list_laqn_prebondst$code)

ulez_pollutants <- c("no2", "nox", "o3", "pm10", "pm25") #pollutants used in ULEZ paper
reqcols <- c("date", "site", "code", "no2", "nox", "pm10")

aq.list_laqn_ulezpoll <- lapply(aq.list_laqn,
                                FUN=function(x){x %>% select(all_of(reqcols))})
aq.list <- aq.list_laqn_ulezpoll[!unlist(lapply(aq.list_laqn_ulezpoll, is.null))] #non null elements of list

#extract data frames with duplicate times
duplicated_times <- function(aq_dat){
  
  is_duplicated <- if(sum(duplicated(aq_dat$date) > 0)) TRUE else FALSE
  print(is_duplicated)
  return(is_duplicated)
  
}

#extract rows which have duplicates in date column
duplicated_rows <- function(aq_dat){
  
  duplicated_rows <- aq_dat[duplicated(aq_dat$date)|duplicated(aq_dat$date, fromLast=TRUE),]
  return(duplicated_rows)
  
}

dats_w_duptimes <- unlist(lapply(aq.list, FUN=function(x){duplicated_times(x)}))
duprows <- lapply(aq.list[dats_w_duptimes], FUN=function(x){duplicated_rows(x)}) #beginning of the year is where we find duplicates

remove_duplicates <- function(aq_dat){
  aq_dat <- aq_dat[!(duplicated(aq_dat$date, fromLast=TRUE)), ] #first duplicate contains missing data
  return(aq_dat)
}

aq.list <- lapply(aq.list, FUN=function(x){remove_duplicates(x)})

# Validate on either side of intervention ---------------------------------

#ensure sufficient data on either side of the intervention
llub <- ymd_hms("2022-10-01 00:00:00") #lower limit for the upper bound
ullb <- lizzy_line_opening - years(1) #upper limit for the lower bound

data_window_check <- function(aq_dat){
  if(min(aq_dat$date) <= ullb && max(aq_dat$date) >= llub){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

valid_span <- unlist(lapply(aq.list, FUN=function(x){data_window_check(x)})) #data with valid windows
aq.list_validspan <- aq.list[valid_span] 

count_missing_daily <- function(aq.dat){
  tmp <- aq.dat %>% mutate(cal_date = date(date),
                           complete = complete.cases(aq.dat)) %>%
    group_by(cal_date) %>% 
    summarise(non_na_prop = sum(complete)/24) %>%
    mutate(valid_day = case_when(non_na_prop < 0.75 ~ 0,
                                 non_na_prop >= 0.75 ~ 1))
  
  valid_site <- if(mean(tmp$valid_day) >= 0.8) TRUE else FALSE
  return(valid_site)
}

valid_sites <- lapply(aq.list_validspan, FUN=count_missing_daily)
aq.list_prebondst_valid <- aq.list_validspan[unlist(valid_sites)]


save(aq.list_prebondst_valid, file=here("data", "processed", "aq.list_prebondst_valid.RData"))





