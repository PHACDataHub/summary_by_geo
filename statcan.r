

library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(ggthemes)
library(stringr)



geos_name_map <- 
"num = nm
CD = Census divisions
CMACA = Census metropolitan areas and census agglomerations
CSD = Census subdivisions (municipalities)
CT = Census tracts
DA = Dissemination areas
DPL = Designated places
ER = Economic regions
FED = Federal electoral districts (2013 Representation Order)
FSA = Forward sortation areas
HR = Health regions, including LHINs and PHUs (December 2017)
POPCNTR = Population centres
PR = Canada, provinces and territories"



cpt_name_map <- 
"num = nm
00 = All provinces and territories
10 = Newfoundland and Labrador
11 = Prince Edward Island
12 = Nova Scotia
13 = New Brunswick
24 = Quebec
35 = Ontario
46 = Manitoba
47 = Saskatchewan
48 = Alberta
59 = British Columbia
60 = Yukon
61 = Northwest Territories
62 = Nunavut"



pt_name_map <- 
  "num = nm
00 = ca
10 = nl
11 = pe
12 = ns
13 = nb
24 = qc
35 = on
46 = mb
47 = sk
48 = ab
59 = bc
60 = yt
61 = nt
62 = nu"



topic_name_map <-
"num = nm
0 = All topics
1 = Aboriginal peoples
2 = Education
3 = Ethnic origin
4 = Families, households and marital status
5 = Housing
6 = Immigration and citizenship
7 = Income
8 = Journey to work
9 = Labour
10 = Language
11 = Language of work
12 = Mobility
13 = Population
14 = Visible minority"

lang_name_map <- 
"num = nm
E = English
F = French"

notes_name_map <- 
"num = nm
0 = do not include footnotes
1 = include footnotes"


stat_name_map <- 
"num = nm
0 = counts
1 = rates"

########################################
#'
#' Reads in a mapping from a number to a name that is coppied from the statscan website
#'
#'
statcan_read_nm_map <- function(text){
  read.csv(sep = "=", 
           colClasses = c("character", "character"), 
           stringsAsFactors = F,
           text = text) %>%  
    as_tibble() %>% 
    mutate_all(str_trim)
}





########################################
#'
#' reads mapping into one place
#'
#'
STATCAN_NAME_MAPS <- 
  bind_rows(
    statcan_read_nm_map(geos_name_map) %>% mutate(type = "geos"),
    statcan_read_nm_map(cpt_name_map) %>% mutate(type = "cpt"),
    statcan_read_nm_map(topic_name_map) %>% mutate(type = "topic"),
    statcan_read_nm_map(lang_name_map) %>% mutate(type = "lang"),
    statcan_read_nm_map(notes_name_map) %>% mutate(type = "notes"),
    statcan_read_nm_map(stat_name_map) %>% mutate(type = "stat"),
    statcan_read_nm_map(pt_name_map) %>% mutate(type = "pt")
  )


############################################
#'
#' Reruns 2 characters for the province name , if
#'
statcan_name_map_province_from_geoid <-function(geo_id){
  geo_id_df <- 
    geo_id %>% 
    as.character() %>%
    str_sub(1,2) %>%
    tibble(num = .)
  
  
  STATCAN_NAME_MAPS %>%
    filter(type == 'cpt') %>%
    distinct() %>%
    right_join(geo_id_df, by = "num") %>%
    pull(nm)
}
############################################
#'
#' Reruns 2 characters for the province name , if
#'
statcan_name_map_pt_from_geoid <-function(geo_id){
  geo_id_df <- 
    geo_id %>% 
    as.character() %>%
    str_sub(1,2) %>%
    tibble(num = .)
  
  
  STATCAN_NAME_MAPS %>%
    filter(type == 'pt') %>%
    distinct() %>%
    right_join(geo_id_df, by = "num") %>%
    pull(nm)
}



############################################
#'
#' returns the mapping for a given type with each possible value for that time and the nm or description of it.
#'
statcan_name_map <- function(a_type = "geos", ret_vec = TRUE){
  STATCAN_NAME_MAPS %>% 
    filter(type == a_type) %>%
    select(-type) %>%
    {if(ret_vec) pull(., num) else .}
}


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




############################################
#'
#'
#' gets a url from statscan and optionally caches it
#'
statcan_url <- function(url){
  a_hash <- digest::digest(url)
  
  cach_data <- STATCAN_CACHE %>% 
    filter(hash == a_hash) %>%
    pull(resp)
  
  
  if (length(cach_data) != 1){
    dat <- 
      tryCatch({
        jsonlite::fromJSON(url)
        },error=function(cond) {
          message(paste("URL does not seem to exist:", url))
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        },
        warning=function(cond) {
          message(paste("URL caused a warning:", url))
          message(cond)
          # Choose a return value in case of warning
          return(NULL)
        },
        finally={
          # NOTE:
          # Here goes everything that should be executed at the end,
          # regardless of success or error.
          # If you want more than one expression to be executed, then you 
          # need to wrap them in curly brackets ({...}); otherwise you could
          # just have written 'finally=<expression>' 
          message(paste("Processed URL:", url))
        }
      )
    if(is.null(dat)  ){
      return(NULL)
    }
    if (! is(dat$DATA, "matrix")){
      err_msg <- glue("Data returned is not a 'matrix' url = '{url}'") 
      stop(err_msg)      
    }
    if (length(dat$COLUMNS) != ncol(dat$DATA)){
      err_msg <- glue("Number of column headers is {length(dat$COLUMNS)} while data width is {length(dat$DATA)} at url = '{url}'") 
      stop(err_msg)
    }
    colnames(dat$DATA) <- dat$COLUMNS
    ret_val <- 
      dat$DATA %>% 
      as_tibble() %>% 
      clean_names()
    
    STATCAN_CACHE <<-
      STATCAN_CACHE %>% 
      add_row(hash = a_hash, 
              url = url,
              resp = list(ret_val),
              dt = Sys.Date())
    
  }
  ret_val <- 
    STATCAN_CACHE %>% 
    filter(hash == a_hash) %>%
    pull(resp) 
  
  return(ret_val[[1]])
}





############################################
#'
#'
#' Maps a name to a code
#'
statcan_nm_2_code <- function(val){
  a_type <- deparse(substitute(val))
  #print(a_type)
  #print(val)
  ret_val <-
    STATCAN_NAME_MAPS %>% 
    filter(type == a_type) %>%
    filter(nm == val) %>% 
    pull(num)
  
  if (length(ret_val) == 1){
    return(ret_val)
  }else{
    return(val)    
  }
}



############################################
#'
#'
#' Maps a codes to names
#'
statcan_code_2_nm <- function(val){
  a_type <- deparse(substitute(val))
  #print(a_type)
  ret_val <-
    STATCAN_NAME_MAPS %>% 
    filter(type == a_type) %>%
    filter(num == val) %>% 
    pull(nm)
  if (length(ret_val) == 1){
    return(ret_val)
  }else{
    return(val)    
  }
  
}






############################################
#'
#'
#'https://www12.statcan.gc.ca/wds-sdw/cpr2016-eng.cfm
#'
#'
statcan_census_profile <- function(geo_uid, topic=14, notes=1, stat=0, type = "json", lang = "E" ){
  topic <- statcan_nm_2_code(topic)
  notes <- statcan_nm_2_code(notes)
  stat <- statcan_nm_2_code(stat)
  lang <- statcan_nm_2_code(lang)
  
  url_template = "https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.{type}?lang={lang}&dguid={geo_uid}&topic={topic}&notes{notes}0&stat={stat}"
  
  url_template %>% 
    glue() %>% 
    statcan_url()
}



############################################
#'
#'
#' 
#' https://www12.statcan.gc.ca/wds-sdw/cr2016geo-eng.cfm
#'
statcan_census_geographies <- function(cpt="00", geos = "hr", type = "json", lang = "E" ){
  cpt <- statcan_nm_2_code(cpt)
  geos <- statcan_nm_2_code(geos)
  lang <- statcan_nm_2_code(lang)
  url_template <- "https://www12.statcan.gc.ca/rest/census-recensement/CR2016Geo.{type}?lang={lang}&geos={geos}&cpt={cpt}"
  url_template %>% 
    glue() %>% 
    statcan_url()
}


############################################
#'
#'
#' takes something like 'csd' or 'ct' and returns the name of the column that is standard in the shp file
#' 
#' 
statcan_geos_id_col_nm <- function(geos){
  if(str_starts(geos, "hr")){
    return("hr_uid")
  }
  geos %>%
    str_replace("_", "") %>%
    str_trim() %>%
    paste0("uid")
}



#####################################
#'
#' reutrns file name 
#' 
#'
statcan_geo_polygon_fn <- function(geos, 
                                dir = file.path(getwd(), "data"),
                                geos_id_col_nm = statcan_geos_id_col_nm(geos)){
  
  #https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lct_000a16a_e.zip
  geos <- 
    geos %>% 
    paste0("__") %>%
    str_sub(1, 3)
  
  fn_base = glue("l{geos}000a16a_e")
  
  fn_base <- 
  if (str_starts(geos, "hr")){
      "HR_000a18a_e"
  }else{
    fn_base
  } 
  full_fn <- file.path(dir, fn_base, glue("{fn_base}.shp"))  
  return(full_fn)
}

#####################################
#'
#' takes something like 'csd' or 'ct' and returns an st object for making maps
#' @example 
#'  statcan_geo_polygon('csd')
#'  statcan_geo_polygon('hr')
statcan_geo_polygon <- function(geos, 
                                dir = file.path(getwd(), "data"),
                                geos_id_col_nm = statcan_geos_id_col_nm(geos)){
  
  
  full_fn <- statcan_geo_polygon_fn(geos, dir, geos_id_col_nm)
  #https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lct_000a16a_e.zip

  
  id_col = statcan_geos_id_col_nm(geos)
  #statcan_name_map_pt_from_geoid
  shp_df <- 
    read_sf(full_fn) %>% 
    clean_names() %>%
    rename(!!sym(geos_id_col_nm) := id_col) %>%
    mutate(pt = statcan_name_map_pt_from_geoid(!!sym(geos_id_col_nm))) %>%
    mutate(prov = statcan_name_map_province_from_geoid(!!sym(geos_id_col_nm)))
  shp_df
}





#####################################
#'
#' returns ggplot object of cloropleth map
#'
#'
statcan_map_cloropleth <- function(geos, dat, geos_id_col_nm = statcan_geos_id_col_nm(geos), val_nm = "t_data_donnee"){
  shp_df <- statcan_geo_polygon(geos = geos, geos_id_col_nm = geos_id_col_nm)
  id_col = statcan_geos_id_col_nm(geos)
    
  
  
  
  
  
  shp_df %>% 
    inner_join(dat, by = geos_id_col_nm) %>% 
    ggplot(aes(fill = !!sym(val_nm) )) +
    geom_sf() +
    theme_map()
}






###################################
#' given a dataframe appends one column for for each name in extracts, like for example
#'  data[["DGUID_vintage"]] would be a thing now
#' 
statcan_parse_geo_uid_extract <- function(data, 
                                          colnm = "DGUID", 
                                          extracts = c("vintage", "type", "schema", "geo_id"),
                                          parse_func = statcan_parse_geo_uid){
  
  
  walk(extracts, function(ex){
    data[[glue("{colnm}_{ex}")]] <<- data[[colnm]] %>% parse_func(nm = ex)  
  })
  return(data)
}


###################################
#'
#' parse vector of geo_uid as seen here
#' https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm
#' 
#' format is  VVVVTSSSSGGGGGGGGGG
#' 
#'
statcan_parse_geo_uid <- function(geo_uid, nm = "vintage"){
  nm <- nm %>% 
    str_to_lower() %>%
    str_trim()
  if (nm == "vintage" | nm == "v"){
    return (str_sub(geo_uid, 1,4))
  }
  if (nm == "type" | nm == "t"){
    return (str_sub(geo_uid, 5,5))
  }
  if (nm == "schema" | nm == "s"){
    return (str_sub(geo_uid, 6,9))
  }  
  if (nm == "geo_id" | nm == "g"){
    return (str_sub(geo_uid, 10))
  }    
}


