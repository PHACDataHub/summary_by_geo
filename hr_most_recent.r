
library(tidyverse)
library(lubridate)
library(cansim)
library(janitor)
library(stringr)
library(zoo)
library(glue)
library(cansim)



dat_cit = "Berry I, Soucy J-PR, Tuite A, Fisman D.\n Open access epidemiologic data and an interactive dashboard\n to monitor the COVID-19 outbreak in Canada.\nCMAJ. 2020 Apr 14;192(15):E420.\ndoi: https://doi.org/10.1503/cmaj.75262"  

#dat_raw %>% count(date_report) %>% view()
theme_set(theme_minimal())


PREV_PER_N = 1000


read_u_of_t_covid_cases <- function(){
  ###################################################
  # READ IN ALL DATA
  fn = "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/cases_2020.csv"
  fn2 = "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/individual_level/cases_2021.csv"
  dat_raw <- read_csv(fn)
  dat_raw2 <- read_csv(fn2)

  
  
  hr_pop_fn <- "https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/other/hr_map.csv"
  hr_info <- read_csv(hr_pop_fn) %>% 
    clean_names() %>%
    mutate(pt = str_to_lower(province_short)) %>%
    distinct(pt, health_region, pop, hr_uid) %>%
    rename(pop_hr = pop)
  
  pt_abbr <- read.csv(stringsAsFactors = F,  
  text="pt, province
qc,Quebec
on,Ontario
ab,Alberta
nl,NL
sk,Saskatchewan
bc,BC
mb,Manitoba
nu,Nunavut
nt,NWT
pe,PEI
yt,Yukon
ns,Nova Scotia
nb,New Brunswick") %>% 
    mutate_all(str_trim) %>%
    as_tibble()
  
  
  
  pop_raw <- cansim::get_cansim("17-10-0005", refresh = FALSE)
  pop <- cansim::add_provincial_abbreviations(pop_raw) %>% clean_names()
  pop <- 
    pop %>% 
    group_by(geo_abb) %>%
    filter(age_group    ==  "All ages") %>% 
    filter(sex    ==  "Both sexes") %>% 
    filter(ref_date == max(ref_date)) %>% 
    ungroup() %>%
    mutate(pt = str_to_lower(geo_abb)) %>%
    select(value, ref_date, pt) %>%
    rename(pop_pt := value, 
           pop_yr := ref_date)
  
  
  dat <- 
    bind_rows(dat_raw, dat_raw2) %>%
    mutate(date_report_dt = lubridate::dmy(date_report)) %>% 
    count(health_region, province, date_report_dt) %>%
    mutate(date_report_dt_wk = week(date_report_dt)) %>%
    mutate(date_report_dt_yr = year(date_report_dt)) %>%
    filter(date_report_dt_yr >= 2021 & date_report_dt_wk >= 8) %>%
    left_join(pt_abbr, by = "province") %>% 
    left_join(pop, by = "pt") %>% 
    left_join(hr_info, by = c("health_region", "pt")) %>%
    group_by(pt) %>%  
    mutate(n_pt = sum(n, na.rm = T)) %>%
    mutate(prev_pt = n_pt/(pop_pt/PREV_PER_N)) %>% 
    ungroup() %>%
    mutate(hr_pt = glue("{health_region}, {str_to_upper(pt)}")) %>% 
    group_by(hr_pt) %>%  
    mutate(n_hr = sum(n, na.rm = T)) %>% 
    mutate(prev_hr = n_hr/(pop_hr/PREV_PER_N)) %>% 
    ungroup() %>%
    mutate(pt = fct_reorder(pt, prev_pt)) %>%
    mutate(health_region = fct_reorder(health_region, prev_hr)) %>%
    mutate(hr_pt = fct_reorder(hr_pt, prev_hr)) %>%
    select(-province)
  
  dat
  
}





