
covid_cases <- read_u_of_t_covid_cases()



covid_cases_n <- covid_cases %>% count(hr_uid , wt = n) %>% mutate(hr_uid = as.character(hr_uid))
covid_cases_prev <- covid_cases %>% distinct(hr_uid , prev_hr) %>% mutate(hr_uid = as.character(hr_uid))


statcan_census_profile(geo_uid = "hr")

dat <- 
  statcan_census_geographies(cpt = "00", geos = "HR") %>%
  #sample_n(55) %>% 
  pull(geo_uid) %>% 
  map_dfr(function(g){ statcan_census_profile(geo_uid = g, topic = 7, stat = 1)}) %>% 
  filter(str_starts(hier_id, "4.1.5.3.2")) %>% 
  mutate(per_income_10_20k = as.integer(t_data_donnee)) %>% 
  select(geo_id, per_income_10_20k) %>%
  rename(hr_uid := geo_id) 


dat %>% 
  left_join(covid_cases_prev) %>%
  ggplot(aes(x = per_income_10_20k, y = prev_hr)) + 
  geom_point()



statcan_map_cloropleth(geos = "hr", 
                       geos_id_col_nm = "hr_uid", #"geo_id",
                       dat = covid_cases_prev, val_nm = "prev_hr") +
  scale_fill_viridis_c() + facet_wrap(~pt)




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
