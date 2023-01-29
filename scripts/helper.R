read.pickle <- function(filename){
  pd <- reticulate::import("pandas")
  return(pd$read_pickle(filename))
}

reorder_by_group <- function(dat){
  dat_ordered <- dat %>% group_by(Pollutant) %>%
    arrange(DateTime) %>% ungroup()
  return(dat_ordered)
}

# fill in rows with missing timestamps
fill_missing_rows <- function(df){
  df_imp <- df %>% padr::pad(interval = "hour",
                             group = "Pollutant") %>%
    group_by(Pollutant) %>%
    tidyr::fill(Site, Classification, Site_combine, Latitude, Longitude,
         .direction="updown") %>%
    ungroup() %>%
    mutate(Year = lubridate::year(DateTime),
           Month = lubridate::month(DateTime),
           Day = lubridate::yday(DateTime),
           Hour = lubridate::hour(DateTime))
  return(df_imp)
}

data_window_check <- function(aq.dat, llub, ullb){
  good_span <- aq.dat %>% group_by(Pollutant) %>%
    summarise(good_span = case_when((min(aq.dat$DateTime) <= ullb 
                                     && max(aq.dat$DateTime) >= llub) ~ 1,
                                    !(min(aq.dat$DateTime) <= ullb 
                                      && max(aq.dat$DateTime) >= llub) ~ 0))
                                  
  return(good_span)
}

preprocess <- function(file.list, llub, ullb) {
  data_list <- lapply(file.list, read.pickle)
  c_data_list <- data.table::rbindlist(data_list) %>%
    select(-any_of("Value_fetched"))
  c_data_list <- transform(c_data_list, 
                           Value=as.numeric(unlist(Value)),
                           Latitude=as.numeric(unlist(Latitude)),
                           Longitude=as.numeric(unlist(Longitude)))
  
  data_by_site <- split(c_data_list, c_data_list$Site)
  site_names <- names(data_by_site)
  
  chron_data_by_site <- lapply(data_by_site, FUN=reorder_by_group)
  chron_data_by_site_filled <- lapply(chron_data_by_site, FUN=fill_missing_rows)
  
  good_span <- lapply(chron_data_by_site, FUN=data_window_check, llub, ullb)
  
  return(list(good_span=good_span,
              data_list_pp=chron_data_by_site_filled))
}

site_valid <- function(aq.dat, intervention, lb, ub){
  
  #sufficient amount of data on either side of intervention
  tmp <- aq.dat %>% mutate(cal_date = date(DateTime),
                           complete = complete.cases(aq.dat)) %>%
    group_by(cal_date, Pollutant) %>% 
    summarise(non_na_prop = sum(complete)/24, .groups="keep") %>%
    mutate(valid_day = case_when(non_na_prop < 0.75 ~ 0,
                                 non_na_prop >= 0.75 ~ 1)) %>%
    group_by(Pollutant) %>%
    summarise(valid_pol = if_else(mean(valid_day[cal_date <= intervention & lb <= cal_date]) >= 0.8 &
                                    mean(valid_day[cal_date > intervention & cal_date <= ub]) >= 0.8, 1, 0))
                                    
  return(tmp)
}

select_valid_sitepol <- function(aq.dat, pol){
  valid_pollutants <- pol$Pollutant[pol$valid_pol == 1]
  tmp <- aq.dat %>% filter(Pollutant %in% valid_pollutants)
  
  return(tmp)
}
