
library(tidyverse)
library(lubridate)
library(cansim)
library(janitor)
library(stringr)
library(zoo)
library(glue)
library(cansim)



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
  distinct(pt, health_region, pop) %>%
  rename(pop_hr = pop)

dat_cit = "Berry I, Soucy J-PR, Tuite A, Fisman D.\n Open access epidemiologic data and an interactive dashboard\n to monitor the COVID-19 outbreak in Canada.\nCMAJ. 2020 Apr 14;192(15):E420.\ndoi: https://doi.org/10.1503/cmaj.75262"

#dat_raw %>% count(date_report) %>% view()
theme_set(theme_minimal())


PREV_PER_N = 1000

pt_abbr <- read.csv(stringsAsFactors = F,  text="pt, province
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
  
  

min_date <- min(dat$date_report_dt)


dat %>% 
  distinct(pt, prev_pt) %>%
  ggplot(aes(x = prev_pt, y = pt, fill = -prev_pt )) +
  geom_col(color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  legend.position = "none") + 
  labs(caption = dat_cit, title = glue("Cases per {PREV_PER_N} Since {min_date}"), y = "", x = glue("Cases per {PREV_PER_N} Since {min_date}"), fill = "")

dat %>% 
  distinct(pt, n_pt, prev_pt) %>%
  ggplot(aes(x = n_pt, y = pt, fill = -prev_pt )) +
  geom_col(color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") + 
  labs(caption = dat_cit, title = glue("Cases Since {min_date}"), subtitle = glue("sorted by Cases per {PREV_PER_N}"), y = "", x = glue("Cases"), fill = glue("Cases per {PREV_PER_N}"))





dat %>% 
  count(pt, date_report_dt, pop_pt, wt = n) %>% 
  mutate(prev_n = n/(pop_pt/PREV_PER_N)) %>%   
  mutate(prev_n7 = zoo::rollmean(prev_n, k = 7, fill = NA)) %>%
  ggplot(aes(x = date_report_dt, y = prev_n, color = pt)) +
  #geom_point() +
  geom_line(aes(y = prev_n7), size=1.5) + 
  #facet_wrap(~pt) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(caption = dat_cit, title = glue("Cases per {PREV_PER_N} Since {min_date}"), y = glue("Cases per {PREV_PER_N}"), x = "Reported Date", color = "") +
  guides(color = guide_legend(reverse = TRUE))




dat %>% 
  count(pt, date_report_dt, pop_pt, wt = n) %>% 
  #mutate(prev_n = n/(pop_pt/PREV_PER_N)) %>%   
  mutate(n7 = zoo::rollmean(n, k = 7, fill = NA)) %>%
  ggplot(aes(x = date_report_dt, y = n, color = pt)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = n7), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(caption = dat_cit, title = glue("Cases Since {min_date}"), y = glue("Cases"), x = "Reported Date", color = "") +
  guides(color = guide_legend(reverse = TRUE))



N_HR_2_USE = 15
N_HR_2_USE_BY_PT = 3






dat %>%
  distinct(health_region, pt, hr_pt, prev_hr, prev_pt) %>%
  group_by(pt) %>%
  slice_max(prev_hr,n =N_HR_2_USE_BY_PT) %>% 
  arrange(desc(prev_hr)) %>% 
  mutate(pt = fct_rev(pt)) %>% 
  #head(15) %>% 
  ggplot(aes(x = prev_hr, y = hr_pt, fill = pt , alpha = prev_hr)) +
  geom_col(color = "black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(caption = dat_cit , subtitle = glue("Cases per {PREV_PER_N} Since {min_date}"), 
       title = glue("Top {N_HR_2_USE_BY_PT} Health Regions for each pt") , 
       y = "", 
       x = glue("Cases per {PREV_PER_N}"), 
       fill = "", alpha = "prevalence by hr") +
  facet_wrap(~ pt, scales = "free_y")




top_hr_by_prev <-   
  dat %>% 
  count(health_region, pt, hr_pt, wt = prev_hr) %>% 
  mutate(hr_pt = fct_reorder(hr_pt, n)) %>% 
  #group_by(pt) %>%
  slice_max(n,n =N_HR_2_USE) %>% 
  arrange(desc(hr_pt)) %>%
  pull(hr_pt)



top_hr_by_n <-   
  dat %>% 
  count(health_region, pt, hr_pt, prev_hr, wt = n) %>% 
  mutate(hr_pt = fct_reorder(hr_pt, n)) %>% 
  #group_by(pt) %>%
  slice_max(n,n =N_HR_2_USE) %>% 
  arrange(desc(prev_hr)) %>%
  mutate(pt = fct_rev(pt)) %>%
  pull(hr_pt)

dat %>% 
  count(health_region, pt, hr_pt, prev_hr, wt = n) %>% 
  mutate(hr_pt = fct_reorder(hr_pt, n)) %>% 
  #group_by(pt) %>%
  slice_max(n,n =N_HR_2_USE) %>% 
  arrange(desc(prev_hr)) %>%
  mutate(pt = fct_rev(pt)) %>% 
  ggplot(aes(x = n, y = hr_pt, fill = pt)) +
  geom_col(color = "black") +
  #facet_wrap(~ pt, scales = "free_y") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(caption = dat_cit , subtitle = glue("Cases Since {min_date}"), 
       title = glue("Top {N_HR_2_USE} Health Regions") , 
       y = "", 
       x = glue("Cases, sorted by prevalence"), 
       fill = "", alpha = "prevalence by hr") 



dat_lbls <- 
dat %>% 
  count(health_region, pt, hr_pt, date_report_dt, pop_hr, wt = n) %>% 
  group_by(hr_pt) %>% arrange(date_report_dt) %>%
  mutate(n7 = zoo::rollmean(n, k = 7, fill = NA)) %>%
  mutate(nn = sum(n, na.rm = T)) %>%
  ungroup() %>% 
  filter(hr_pt %in% top_hr_by_n ) %>% 
  filter(!is.na(n7)) %>%
  mutate(hr_pt = fct_rev(hr_pt)) %>%
  group_by(hr_pt) %>%
  slice(which.max(date_report_dt))
library(ggrepel)

dat %>% 
  count(health_region, pt, hr_pt, date_report_dt, pop_hr, wt = n) %>% 
  group_by(hr_pt) %>% arrange(date_report_dt) %>%
  mutate(n7 = zoo::rollmean(n, k = 7, fill = NA)) %>%
  mutate(nn = sum(n, na.rm = T)) %>%
  ungroup() %>% 
  filter(hr_pt %in% top_hr_by_n ) %>% 
  mutate(hr_pt = fct_rev(hr_pt)) %>% 
  ggplot(aes(x = date_report_dt, y = n, color = pt, group = hr_pt)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = n7), size = 1.5) +
  geom_text_repel(data =dat_lbls, aes(label =hr_pt, y = n7 ), nudge_x = 100) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(caption = dat_cit , subtitle = glue("Cases Since {min_date}"), 
     title = glue("Top {N_HR_2_USE} Health Regions") , 
     y = "Cases", 
     x = glue("Reported Date"))  +
  scale_x_date(breaks = "weeks") #+
  #facet_wrap(~pt)
  







dat_lbls <- 
  dat %>% 
  count(health_region, pt, hr_pt, date_report_dt, pop_hr, wt = n) %>% 
  mutate(prev_n = n/(pop_hr/PREV_PER_N)) %>%  #filter(health_region == "North")
  group_by(hr_pt) %>% arrange(date_report_dt) %>%
  mutate(prev_n7 = zoo::rollmean(prev_n, k = 7, fill = NA)) %>%
  #mutate(prev_n = sum(prev_n, na.rm = T)) %>%
  ungroup() %>% 
  filter(hr_pt %in% top_hr_by_prev ) %>% 
  filter(!is.na(prev_n7)) %>%
  mutate(hr_pt = fct_rev(hr_pt)) %>%
  group_by(hr_pt) %>%
  slice(which.max(date_report_dt))





  dat %>% 
  count(health_region, pt, hr_pt, date_report_dt, pop_hr, wt = n) %>% 
  mutate(prev_n = n/(pop_hr/PREV_PER_N)) %>%  #filter(health_region == "North")
  group_by(hr_pt) %>% arrange(date_report_dt) %>%
  mutate(prev_n7 = zoo::rollmean(prev_n, k = 7, fill = NA)) %>%
  #mutate(prev_n = sum(prev_n, na.rm = T)) %>%
  ungroup() %>% 
  filter(hr_pt %in% top_hr_by_prev ) %>% 
  mutate(pt = fct_rev(pt)) %>% 
  mutate(hr_pt = fct_rev(hr_pt)) %>% #filter(health_region == "North")
    ggplot(aes(x = date_report_dt, y = prev_n, color = pt, group = hr_pt)) +
    geom_point() +
    geom_line(aes(y = prev_n7), size = 1.5) +
    geom_text_repel(data =dat_lbls, aes(label =hr_pt, y = prev_n7 ), nudge_x = 100) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs(caption = dat_cit , subtitle = glue("Cases  per {PREV_PER_N} Since {min_date}"), 
         title = glue("Top {N_HR_2_USE} Health Regions") , 
         y = glue("Cases  per {PREV_PER_N}"), 
         x = glue("Reported Date"))  +
    scale_x_date(breaks = "weeks") #+
    #facet_wrap(~pt)


library(CANSIM2R)


library(jsonlite)

url <- "https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.json?"#lang=E&dguid=2016A00001001472&topic=0&notes=0&stat=0"
dat <- jsonlite::fromJSON(url) as_tibble()
dat$DATA %>% as_tibble() %>% distinct(V5)
dat$DATA %>% as_tibble()

colnames(dat$DATA) <- dat$COLUMNS

view(dat$DATA)

dat <- NULL

url <- "https://www12.statcan.gc.ca/rest/census-recensement/CR2016Geo.json?lang=E&geos=CSD&cpt=35"
dat <- jsonlite::fromJSON(url) 
colnames(dat$DATA) <- dat$COLUMNS
dat$DATA <- dat$DATA %>% as_tibble()
dat$DATA
geo_uid  <- dat$DATA %>% filter(GEO_NAME_NOM == "Toronto") %>% pull(GEO_UID) 


url <- glue("https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.json?lang=E&dguid={geo_uid}&topic=0&notes=0&stat=0")
dat <- jsonlite::fromJSON(url) 
colnames(dat$DATA) <- dat$COLUMNS
dat$DATA <- dat$DATA %>% as_tibble()
dat$DATA
