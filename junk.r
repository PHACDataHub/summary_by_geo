



###############################################################
######################### JUNK ################################

library(parallel)
dat %>% filter(str_starts(hier_id, "4.1.5.3.2")) %>% select(text_name_nom)

statcan_map_cloropleth(geos = "hr", 
                       geos_id_col_nm = "geo_id",
                       dat = dat %>% filter(str_starts(hier_id, "4.1.5.3.2")) %>% 
                         mutate(t_data_donnee = as.integer(t_data_donnee))) +
  scale_fill_viridis_c("Income 10-20k", labels = scales::percent) 
geos_id_col_nm = 'geo_id'











############################################
#'
#' Holds a cache of the data from statscan api
#'
STATCAN_CACHE <- 
  tibble(
    hash = as.character(NA),
    url =  as.character(NA),
    resp =  list(),
    dt = as.Date(NA)
  )



set.seed(1)

tic <- Sys.time()
dat = NULL
dat <- 
  statcan_census_geographies(cpt = "00", geos = "HR") %>%
  sample_n(1) %>% 
  pull(geo_uid) %>% 
  mclapply(function(g){ statcan_census_profile(geo_uid = g, topic = 7, stat = 1)}, mc.cores = 1) %>% 
  bind_rows() %>% view()

toc<- Sys.time()

print(toc-tic)





filter(str_starts(hier_id, "4.1.5.3.2")) %>% 
  mutate(t_data_donnee = as.integer(t_data_donnee))

dat %>% 
  filter(str_starts(hier_id, "4.1.5.3.2"))

shp_df <- statcan_geo_polygon(geos = "hr")


library(cansim)
cansim::get_cansim_table_notes("14-10-0293")
cansim::get_cansim_table_notes("14-10-0293")
data <- get_cansim("14-10-0293")
births <- get_cansim("051-0013")
births <- get_cansim("17-10-0016-01")

get_cansim_table_overview("36-10-040")

births %>% statcan_parse_geo_uid_extract(colnm = )
tmp_tbl <- 
  list_cansim_tables() %>%
  sample_n(1) %>%
  pull(cansim_table_number) %>% 
  get_cansim() %>% 
  
  
  
  statcan_parse_geo_uid_extract() %>% 
  select(starts_with("DGUID")) %>% 
  distinct()

data %>% count(GEO)
data %>% count(DGUID) %>% view()
data %>% count(GeoUID, DGUID , GEO  )

statcan_census_profile(geo_uid = "2016A000212")
statcan_census_profile(geo_uid = "2016A000011124")
statcan_census_profile(geo_uid = "2016A000259")
statcan_census_profile(geo_uid = "2011S05004830", topic = 0)

statcan_name_map("topic", ret_vec = F)


data %>% 
  filter(!is.na(DGUID)) %>%
  filter(str_starts(DGUID, "2016")) %>%
  sample_n(1) %>% pull(DGUID) %>% 
  statcan_census_profile(geo_uid = ., topic = 0)# %>% pull(text_name_nom) %>% unique()




CANSIM::add_provincial_abbreviations()

statcan_census_profile(geo_uid = "2016A000212")

statcan_census_geographies(cpt = "10", geos = "hr") %>% pull(geo_id_code) # Matches with HR HR_UID
statcan_census_geographies(cpt = "Ontario", geos = "csd") %>% view()  # matches with CSDUID
statcan_census_geographies(cpt = "Ontario", geos = "CMACA")








dat %>% statcan_parse_geo_uid_extract("geo_uid") %>%
  count()

dat$text_name_nom %>% unique()
dat %>% filter(str_detect(text_name_nom,"Percentage with total income"))




statcan_name_map("geos") %>%
  map_dfr(~ {
    statcan_census_geographies(geos = .x) %>% 
      mutate(geos = .x) %>%
      mutate(geo_uid_vintage = str_sub(geo_uid, 1,4)) %>%
      mutate(geo_uid_type = str_sub(geo_uid, 5,5)) %>%
      mutate(geo_uid_schema = str_sub(geo_uid, 6,9)) %>%
      mutate(geo_uid_geo_id = str_sub(geo_uid, 10))
  }) #%>% 
#count(geo_id_code, sort = T) %>%

a %>% filter(geo_id_code == "1011")
a %>% count(geo_uid, sort = T)

statcan_census_geographies(cpt = "Ontario", geos = "CMACA") %>% 
  #sample_n(1) %>% 
  pull(geo_uid) %>%
  map_dfr(statcan_census_profile)


statcan_census_geographies() %>% 
  
  pull(geo_uid) %>%
  statcan_census_profile() %>% view()

