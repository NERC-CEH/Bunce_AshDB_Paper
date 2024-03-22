# Combine ground flora data and calculate indices
library(dplyr)
library(tidyr)

# read in data ####

# Metadata
AMALG_SPCODES <- read.csv("Metadata/AMALG_SPCODES.csv")

# Plot descriptors
PLDATA22 <- read.csv("EIDC/2022/WOODLANDS_SURVEY_SITE_INFORMATION_2022.csv")
PLDATA7101 <- read.csv("EIDC/1971/Woodlands_Survey_Site_Information_1971_2001.csv")

# Plot locations
SITES22 <- read.csv("EIDC/2022/WOODLANDS_SURVEY_SITES_2022.csv")

# Ground flora data
GRFLORA22 <- read.csv("EIDC/2022/WOODLANDS_SURVEY_GROUND_FLORA_2022.csv")
GRFLORA7101 <- read.csv("EIDC/1971/Woodlands_Survey_Flora_Data_1971_2001.csv")

# Tree data
DBH22 <- read.csv("EIDC/2022/WOODLANDS_SURVEY_TREE_DATA_2022.csv")
DBH7101 <- read.csv("EIDC/1971/Woodlands_Survey_Tree_Diameter_Data_1971_2001.csv")


# Ground flora data
GRFLORA_ALL <- GRFLORA22 %>% 
  select(SITE_NO, PLOT_NO, BRC_NAMES, BRC_NUMBER, TOTAL_COVER) %>%
  distinct() %>%
  mutate(YEAR = 3) %>%
  full_join(select(GRFLORA7101, SITE_NO = SITE, PLOT_NO = PLOT,
                   BRC_NUMBER = Amalgams,
                   COV, YEAR = Yr_2, Bryo) %>% 
              filter(!is.na(BRC_NUMBER) & Bryo != "b" & !is.na(COV)) %>%
              distinct() %>%
              group_by(SITE_NO, PLOT_NO, BRC_NUMBER, YEAR) %>%
              summarise(TOTAL_COVER = sum(COV)) %>%
              mutate(TOTAL_COVER = ifelse(is.na(TOTAL_COVER) | TOTAL_COVER < 1 | is.infinite(TOTAL_COVER), 1, TOTAL_COVER))) %>%
  filter(!is.na(BRC_NUMBER)) %>%
  select(-BRC_NAMES) %>%
  mutate(BRC_NUMBER = round(BRC_NUMBER, 1)) %>%
  inner_join(select(AMALG_SPCODES, contains("BRC_")) %>%
               mutate(across(contains("NUMBER"), \(x) round(x, 1))) %>% 
               distinct(),
             relationship = "many-to-many") %>%
  group_by(YEAR, SITE_NO, PLOT_NO, AMALG_BRC_NAME, AMALG_BRC_CODE, BRC_NUMBER) %>%
  summarise(TOTAL_COVER = max(TOTAL_COVER)) %>%
  summarise(TOTAL_COVER = sum(TOTAL_COVER), .groups = "drop")

# Summary statistics
AMALG_EBER <- AMALG_SPCODES %>% 
  rename_with(toupper) %>%
  select(AMALG_BRC_CODE, GROWTH_FORM) %>%
  distinct() %>%
  filter(!is.na(AMALG_BRC_CODE))


GRFLORA_SUMMARY <- GRFLORA_ALL %>%
  inner_join(AMALG_EBER,
            by = c("AMALG_BRC_CODE")) %>%
  filter(GROWTH_FORM %in% c("f","fe","g","m","s","w")) %>%
  mutate(TOTAL_COVER = ifelse(TOTAL_COVER < 5, 1, TOTAL_COVER)) %>%
  group_by(SITE_NO, PLOT_NO, YEAR) %>%
  mutate(PROP_COVER = TOTAL_COVER/sum(TOTAL_COVER)) %>%
  summarise(SPECIES_RICH = length(unique(AMALG_BRC_CODE)),
            SPECIES_SHANNON = -sum(PROP_COVER * log(PROP_COVER)),
            FORB_COVER = sum(TOTAL_COVER[GROWTH_FORM == "f"]), 
            FERN_COVER = sum(TOTAL_COVER[GROWTH_FORM == "fe"]), 
            GRASS_COVER = sum(TOTAL_COVER[GROWTH_FORM == "g"]), 
            WOOD_COVER = sum(TOTAL_COVER[GROWTH_FORM == "w"]), 
            .groups = "drop") %>%
  mutate(across(ends_with("COVER"), \(x) replace_na(x, 0)))


GRFLORA_SITE <- GRFLORA_ALL %>%
  inner_join(AMALG_EBER,
             by = c("AMALG_BRC_CODE")) %>%
  filter(GROWTH_FORM %in% c("f","fe","g","m","s","w")) %>%
  mutate(TOTAL_COVER = ifelse(TOTAL_COVER < 5, 1, TOTAL_COVER)) %>%
  group_by(SITE_NO, PLOT_NO, YEAR) %>%
  mutate(PROP_COVER = TOTAL_COVER/sum(TOTAL_COVER)) %>%
  ungroup() %>% group_by(SITE_NO, YEAR) %>%
  summarise(SPECIES_RICH = length(unique(AMALG_BRC_CODE)),
            .groups = "drop")

# Survey dates
plot_locdate <- PLDATA22 %>%
  filter(CODE_GROUP_DESCRIPTION == "Survey start date") %>%
  select(SITE_NO, PLOT_NO, SURVEY_START = DESCRIPTION) %>%
  mutate(Date = lubridate::parse_date_time(SURVEY_START, "d-b-y"),
         Survey = "2022") %>%
  select(SITE_NO, PLOT_NO, Survey, Date) %>%
  full_join(PLDATA7101 %>%
              select(SITE_NO = SITE, DATE1971, DATE2003) %>%
              mutate(across(starts_with("DATE"), lubridate::dmy)) %>%
              # replace missing values with our best guesses
              mutate(
                DATE1971 = if_else(
                  SITE_NO == 53, lubridate::dmy("08-09-1971"),
                  if_else(
                    SITE_NO == 68, lubridate::dmy("23-08-1971"),
                    if_else(
                      SITE_NO == 72, lubridate::dmy("20-09-1971"),
                      if_else(SITE_NO == 90, lubridate::dmy("15-07-1971"),
                              if_else(SITE_NO == 99, lubridate::dmy("13-09-1971"),
                                    DATE1971)))))) %>%
              tidyr::pivot_longer(starts_with("DATE"), values_to = "Date", 
                                names_to = "Survey", names_prefix = "DATE") %>%
              filter(!is.na(Date)) %>%
              left_join(select(PLDATA7101, SITE_NO = SITE, PLOT_NO = PLOT) %>% 
                          distinct(),
                        relationship = "many-to-many") %>%
              filter(!is.na(PLOT_NO)) %>%
              distinct()) %>%
  group_by(SITE_NO, PLOT_NO, Survey) %>%
  filter(Date == min(Date)) %>% ungroup() %>%
  mutate(Year = lubridate::year(Date),
         DayOfYear = lubridate::yday(Date),
         YEAR = recode(Survey, "1971" = 1L, "2003" = 2L, "2022" = 3L)) %>%
  select(SITE_NO, PLOT_NO, Survey, Date, Year, DayOfYear, YEAR) 

# Ash dieback disease presence - all dieback plots have "Chala" in a description
ALLPLOTS_DISEASE <- PLDATA22 %>% 
  filter(grepl("Chala", DESCRIPTION)) %>%
  mutate(ASHDIEBACK = "Present") %>%
  select(SITE_NO, PLOT_NO, ASHDIEBACK) %>%
  full_join(select(PLDATA22, SITE_NO, PLOT_NO) %>% distinct()) %>%
  mutate(ASHDIEBACK = tidyr::replace_na(ASHDIEBACK, "Not present")) %>%
  distinct()

# Combine date and disease data
plot_meta <- full_join(plot_locdate, ALLPLOTS_DISEASE, 
                       by = c("SITE_NO","PLOT_NO"),
                       multiple = "all")
BLEAF_META <- plot_meta %>%
  select(SITE_NO, PLOT_NO, YEAR, DAYOFYEAR = DayOfYear, ASHDIEBACK) %>%
  bind_rows(data.frame(SITE_NO = 71, PLOT_NO = 6, YEAR = 3L, DAYOFYEAR = 182, 
                       ASHDIEBACK = "Not present"))

BRLEAF_SUMMARY <- GRFLORA_SUMMARY %>% 
  left_join(BLEAF_META)

# Site-level data
BRLEAF_SUMM_SITE <- BLEAF_META %>% 
  mutate(ASHDIEBACK = ifelse(ASHDIEBACK == "Present", 1, 0)) %>%
  group_by(SITE_NO, YEAR) %>%
  summarise(NPLOT = length(unique(PLOT_NO)),
            DAYOFYEAR = mean(DAYOFYEAR),
            ASHDIEBACK = sum(ASHDIEBACK, na.rm = TRUE), .groups = "drop") %>%
  mutate(ASHDIEBACK = ASHDIEBACK/NPLOT) %>%
  inner_join(GRFLORA_SITE)

# Cover of specific species ####
# Bramble cover
SP_COVER <- GRFLORA_ALL %>%
  filter(AMALG_BRC_NAME == "Rubus fruticosus agg.") %>%
  mutate(TOTAL_COVER = ifelse(TOTAL_COVER < 5, 1, TOTAL_COVER)) %>%
  group_by(SITE_NO, PLOT_NO, YEAR) %>%
  summarise(RUBUS = sum(TOTAL_COVER)) %>%
  full_join(select(GRFLORA_SUMMARY, SITE_NO, PLOT_NO, YEAR)) %>%
  mutate(RUBUS = replace_na(RUBUS, 0))

BRLEAF_SUMMARY <- left_join(BRLEAF_SUMMARY, SP_COVER)

# DBH data ####
# codes and metadata
DBH_CLASS_KEY <- data.frame(MID_POINT_CM = seq(2.5,192.5,5),
                            LOWER_BOUND = c(1,seq(5,190,5)),
                            DBH_CLASS = 1:39)
TREE_AMALGAMS <- read.csv("Metadata/TREE_AMALGAMS.csv")

DBH_ALL <- DBH22 %>%
  mutate(DEAD_LIVE = ifelse(DEAD == "Yes", "D", "L"),
         ROWID = 1:n()) %>%
  filter(!is.na(DBH)) %>%
  mutate(DBH_CLASS = as.numeric(cut(DBH, seq(0,195,5),labels = 1:39, right = FALSE)),
         MULTI_ID = ifelse(MULTI == "Yes",MULTI_ID,as.character(ROWID))) %>%
  # group_by(SITE_NO, PLOT_NO, SPECIES, DEAD_LIVE, MULTI_ID) %>%
  # summarise(DBH_CLASS = max(DBH_CLASS), .groups = "drop") %>%
  select(SITE_NO, PLOT_NO, DBH_CLASS, AMALGAM_NAMES = SPECIES, DEAD_LIVE) %>%
  filter(AMALGAM_NAMES != "None") %>%
  # fix typo
  mutate(AMALGAM_NAMES = ifelse(AMALGAM_NAMES == "Aesculus hippocastanus","Aesculus hippocastanum", AMALGAM_NAMES),
         YR = 3) %>%
  left_join(TREE_AMALGAMS, by = "AMALGAM_NAMES") %>%
  select(-AMALG_BRC_CODE) %>% rename(AMALG_BRC_CODE = ACTUAL_AMALG_BRC_CODE) %>%
  count(SITE_NO, PLOT_NO, DBH_CLASS, AMALG_BRC_CODE, AMALGAM_NAMES, YR, DEAD_LIVE) %>%
  rename(COUNT = n) %>%
  bind_rows(select(DBH7101, SITE_NO = Site, PLOT_NO = Plot, DBH_CLASS = DBHclass,
                   AMALG_BRC_CODE = Amalgams, AMALGAM_NAMES = Amalgam_names,
                   YR = Yr, DEAD_LIVE = Status, COUNT = Count) %>%
              mutate(DEAD_LIVE = substring(DEAD_LIVE, 1, 1),
                     COUNT = replace_na(COUNT, 1)) %>%
              left_join(TREE_AMALGAMS) %>%
              select(-AMALG_BRC_CODE, -DBH_TREE_SHRUB) %>% 
              rename(AMALG_BRC_CODE = ACTUAL_AMALG_BRC_CODE)) %>%
  inner_join(select(TREE_AMALGAMS, AMALG_BRC_CODE = ACTUAL_AMALG_BRC_CODE,
                    DBH_TREE_SHRUB) %>% distinct(),
             relationship = "many-to-one") %>%
  filter(DBH_TREE_SHRUB %in% c("T","S")) %>%
  select(SITE_NO, PLOT_NO, DBH_CLASS, AMALG_BRC_CODE,
         YEAR = YR, DEAD_LIVE, COUNT) %>%
  left_join(select(DBH_CLASS_KEY, MID_POINT_CM, DBH_CLASS),
            by = "DBH_CLASS") %>%
  mutate(BASAL_AREA = pi*((0.5*MID_POINT_CM)^2)) %>%
  filter(DEAD_LIVE == "L") %>%
  group_by(SITE_NO, PLOT_NO, YEAR) %>%
  summarise(STEM_COUNT = sum(COUNT),
            MEAN_BASAL_AREA = weighted.mean(BASAL_AREA, COUNT),
            SUM_BASAL_AREA = sum(COUNT*BASAL_AREA),
            TREE_RICHNESS = length(unique(AMALG_BRC_CODE)),
            .groups = "drop") %>%
  full_join(select(GRFLORA_SUMMARY, SITE_NO, PLOT_NO, YEAR)) %>%
  mutate(across(c(STEM_COUNT, SUM_BASAL_AREA, TREE_RICHNESS),
                \(x) replace_na(x, 0)))

BRLEAF_SUMMARY <- left_join(BRLEAF_SUMMARY, DBH_ALL)


write.csv(BRLEAF_SUMMARY, "Outputs/Broadleaf_summary_metrics.csv",
          row.names = FALSE)

# Climate data ####
# Get 2001 data
climdatfolder <- "INSERT NAME OF FOLDER HERE"
# 2001 rainfall
rain01 <- terra::rast(paste0(climdatfolder, "rainfall_hadukgrid_uk_1km_seas-20y_198101-200012.nc"))

pl_rain01 <- terra::extract(rain01, plot_locdate) %>%
  rename(Winter_rainfall_2001 = rainfall_1, Summer_rainfall_2001 = rainfall_3) %>%
  bind_cols(plot_locdate) %>%
  select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, 
         Winter_rainfall_2001, Summer_rainfall_2001)

pl_rain01_NAs <- terra::extract(rain01, 
                                plot_locdate[is.na(pl_rain01$Winter_rainfall_2001),],
                                method = "bilinear") %>%
  rename(Winter_rainfall_2001 = rainfall_1, Summer_rainfall_2001 = rainfall_3) %>%
  bind_cols(plot_locdate[is.na(pl_rain01$Winter_rainfall_2001),]) %>%
  select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, 
         Winter_rainfall_2001, Summer_rainfall_2001)

pl_rain01 <- pl_rain01 %>%
  filter(!is.na(Winter_rainfall_2001)) %>%
  bind_rows(pl_rain01_NAs)

# 2001 min temp
tasmin01 <- terra::rast(paste0(climdatfolder, "tasmin_hadukgrid_uk_1km_seas-20y_198101-200012.nc"))

pl_tmin01 <- terra::extract(tasmin01, plot_locdate) %>%
  rename(Winter_tasmin_2001 = tasmin_1) %>%
  bind_cols(plot_locdate) %>%
  select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, 
         Winter_tasmin_2001)

pl_tmin01_NAs <- terra::extract(tasmin01, 
                                plot_locdate[is.na(pl_tmin01$Winter_tasmin_2001),],
                                method = "bilinear") %>%
  rename(Winter_tasmin_2001 = tasmin_1) %>%
  bind_cols(plot_locdate[is.na(pl_tmin01$Winter_tasmin_2001),]) %>%
  select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, 
         Winter_tasmin_2001)

pl_tmin01 <- pl_tmin01 %>%
  filter(!is.na(Winter_tasmin_2001)) %>%
  bind_rows(pl_tmin01_NAs)


# 2001 max temp
tasmax01 <- terra::rast(paste0(climdatfolder, "tasmax_hadukgrid_uk_1km_seas-20y_198101-200012.nc"))

pl_tmax01 <- terra::extract(tasmax01, plot_locdate) %>%
  rename(Summer_tasmax_2001 = tasmax_3) %>%
  bind_cols(plot_locdate) %>%
  select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, 
         Summer_tasmax_2001)

pl_tmax01_NAs <- terra::extract(tasmax01, 
                                plot_locdate[is.na(pl_tmax01$Summer_tasmax_2001),],
                                method = "bilinear") %>%
  rename(Summer_tasmax_2001 = tasmax_3) %>%
  bind_cols(plot_locdate[is.na(pl_tmax01$Summer_tasmax_2001),]) %>%
  select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, 
         Summer_tasmax_2001)

pl_tmax01 <- pl_tmax01 %>%
  filter(!is.na(Summer_tasmax_2001)) %>%
  bind_rows(pl_tmax01_NAs)

# Combine 2001 climate data
pl_clim01 <- full_join(pl_rain01, pl_tmin01) %>%
  full_join(pl_tmax01)


# 1971 climate data
rain71 <- lapply(1951:1970, function(x){
  rain_dat <- terra::rast(paste0(climdatfolder, "Annual_4170/",
                                 "rainfall_hadukgrid_uk_1km_seas_",
                                 x,"01-",x,"12.nc"))
  pl_rain <- terra::extract(rain_dat, plot_locdate) %>%
    bind_cols(plot_locdate)
  pl_rain_NAs <- terra::extract(rain_dat, plot_locdate[is.na(pl_rain$rainfall_1),],
                                method = "bilinear") %>%
    bind_cols(plot_locdate[is.na(pl_rain$rainfall_1),])
  pl_rain <- pl_rain %>%
    filter(!is.na(rainfall_1)) %>%
    bind_rows(pl_rain_NAs) %>%
    mutate(Year = x) %>%
    select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year,
           Winter_rainfall_1971 = rainfall_1, Summer_rainfall_1971 = rainfall_3)
})
rain71_df <- do.call(rbind, rain71) %>%
  group_by(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date) %>%
  summarise(Winter_rainfall_1971 = mean(Winter_rainfall_1971),
            Summer_rainfall_1971 = mean(Summer_rainfall_1971),
            .groups = "drop")

# tasmin
tmin71 <- lapply(1951:1970, function(x){
  tmin_dat <- terra::rast(paste0(climdatfolder, "Annual_4170/",
                                 "tasmin_hadukgrid_uk_1km_seas_",
                                 x,"01-",x,"12.nc"))
  pl_tmin <- terra::extract(tmin_dat, plot_locdate) %>%
    bind_cols(plot_locdate)
  pl_tmin_NAs <- terra::extract(tmin_dat, plot_locdate[is.na(pl_tmin$tasmin_1),],
                                method = "bilinear") %>%
    bind_cols(plot_locdate[is.na(pl_tmin$tasmin_1),])
  pl_tmin <- pl_tmin %>%
    filter(!is.na(tasmin_1)) %>%
    bind_rows(pl_tmin_NAs) %>%
    mutate(Year = x) %>%
    select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year,
           Winter_tasmin_1971 = tasmin_1)
})
tmin71_df <- do.call(rbind, tmin71) %>%
  group_by(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date) %>%
  summarise(Winter_tasmin_1971 = mean(Winter_tasmin_1971),
            .groups = "drop")

# tasmax
tmax71 <- lapply(1951:1970, function(x){
  tmax_dat <- terra::rast(paste0(climdatfolder, "Annual_4170/",
                                 "tasmax_hadukgrid_uk_1km_seas_",
                                 x,"01-",x,"12.nc"))
  pl_tmax <- terra::extract(tmax_dat, plot_locdate) %>%
    bind_cols(plot_locdate)
  pl_tmax_NAs <- terra::extract(tmax_dat, plot_locdate[is.na(pl_tmax$tasmax_1),],
                                method = "bilinear") %>%
    bind_cols(plot_locdate[is.na(pl_tmax$tasmax_1),])
  pl_tmax <- pl_tmax %>%
    filter(!is.na(tasmax_1)) %>%
    bind_rows(pl_tmax_NAs) %>%
    mutate(Year = x) %>%
    select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year,
           Summer_tasmax_1971 = tasmax_3)
})
tmax71_df <- do.call(rbind, tmax71) %>%
  group_by(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date) %>%
  summarise(Summer_tasmax_1971 = mean(Summer_tasmax_1971),
            .groups = "drop")

# All climate data
clim71 <- full_join(rain71_df, tmin71_df) %>%
  full_join(tmax71_df)

# Most recent survey
rain22 <- lapply(1998:2021, function(x){
  rain_dat <- terra::rast(paste0(climdatfolder, "Annual/",
                                 "rainfall_hadukgrid_uk_1km_seas_",
                                 x,"01-",x,"12.nc"))
  pl_rain <- terra::extract(rain_dat, plot_locdate) %>%
    bind_cols(plot_locdate)
  pl_rain_NAs <- terra::extract(rain_dat, plot_locdate[is.na(pl_rain$rainfall_1),],
                                method = "bilinear") %>%
    bind_cols(plot_locdate[is.na(pl_rain$rainfall_1),])
  pl_rain <- pl_rain %>%
    filter(!is.na(rainfall_1)) %>%
    bind_rows(pl_rain_NAs) %>%
    mutate(Clim_Year = x) %>%
    select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, Clim_Year,
           Winter_rainfall_2022 = rainfall_1, Summer_rainfall_2022 = rainfall_3)
})
rain22_df <- do.call(rbind, rain22) %>%
  filter(Clim_Year < Year & Clim_Year >= (Year-20)) %>%
  group_by(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year) %>%
  summarise(Winter_rainfall_2022 = mean(Winter_rainfall_2022),
            Summer_rainfall_2022 = mean(Summer_rainfall_2022),
            .groups = "drop")

# tasmin
tmin22 <- lapply(1998:2021, function(x){
  tmin_dat <- terra::rast(paste0(climdatfolder, "Annual/",
                                 "tasmin_hadukgrid_uk_1km_seas_",
                                 x,"01-",x,"12.nc"))
  pl_tmin <- terra::extract(tmin_dat, plot_locdate) %>%
    bind_cols(plot_locdate)
  pl_tmin_NAs <- terra::extract(tmin_dat, plot_locdate[is.na(pl_tmin$tasmin_1),],
                                method = "bilinear") %>%
    bind_cols(plot_locdate[is.na(pl_tmin$tasmin_1),])
  pl_tmin <- pl_tmin %>%
    filter(!is.na(tasmin_1)) %>%
    bind_rows(pl_tmin_NAs) %>%
    mutate(Clim_Year = x) %>%
    select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, Clim_Year,
           Winter_tasmin_2022 = tasmin_1)
})
tmin22_df <- do.call(rbind, tmin22) %>%
  filter(Clim_Year < Year & Clim_Year >= (Year-20)) %>%
  group_by(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year) %>%
  summarise(Winter_tasmin_2022 = mean(Winter_tasmin_2022),
            .groups = "drop")

# tasmax
tmax22 <- lapply(1998:2021, function(x){
  tmax_dat <- terra::rast(paste0(climdatfolder, "Annual/",
                                 "tasmax_hadukgrid_uk_1km_seas_",
                                 x,"01-",x,"12.nc"))
  pl_tmax <- terra::extract(tmax_dat, plot_locdate) %>%
    bind_cols(plot_locdate)
  pl_tmax_NAs <- terra::extract(tmax_dat, plot_locdate[is.na(pl_tmax$tasmax_1),],
                                method = "bilinear") %>%
    bind_cols(plot_locdate[is.na(pl_tmax$tasmax_1),])
  pl_tmax <- pl_tmax %>%
    filter(!is.na(tasmax_1)) %>%
    bind_rows(pl_tmax_NAs) %>%
    mutate(Clim_Year = x) %>%
    select(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year, Clim_Year,
           Summer_tasmax_2022 = tasmax_3)
})
tmax22_df <- do.call(rbind, tmax22) %>%
  filter(Clim_Year < Year & Clim_Year >= (Year-20)) %>%
  group_by(SITE_NO, PLOT_NO, POINT_X, POINT_Y, Date, Year) %>%
  summarise(Summer_tasmax_2022 = mean(Summer_tasmax_2022),
            .groups = "drop")

# All climate data
Clim_data <- full_join(rain22_df, tmin22_df) %>%
  full_join(tmax22_df) %>%
  full_join(clim71) %>% 
  full_join(pl_clim01) %>%
  select(SITE_NO:POINT_Y, starts_with("Winter"), starts_with("Summer"))

# Climate data 
Clim_data <- read.csv("Outputs/Climate_data.csv")
Climate_data <- pivot_longer(Clim_data, 
                             Winter_rainfall_2022:Summer_tasmax_2001,
                             names_to = c("Season","Variable","YEAR"),
                             names_sep = "_") %>%
  mutate(Variable = paste(Season, Variable, sep = "_")) %>%
  select(-Season) %>%
  distinct() %>%
  pivot_wider(names_from = Variable, values_from = value) %>%
  mutate(Summer_rainfall = (Summer_rainfall - 250)/100,
         Winter_rainfall = (Winter_rainfall - 300)/100,
         Summer_tasmax = (Summer_tasmax - 19),
         Winter_tasmin = (Winter_tasmin - 1))


#  ash presence
ash_plots <- filter(DBH22, SITE_NO < 200 & 
                      AMALG_BRC_NAME == "Fraxinus excelsior (g)") %>% 
  select(SITE_NO, PLOT_NO) %>% unique() %>% mutate(ASHPRESENT = TRUE)
ash_sites <- unique(ash_plots$SITE_NO)

# deer risk
deerrisk <- readxl::read_excel(paste0(config::get()$directories$deerdata,
                                      "Grid refs and deer risk_broadleaved.xlsx"),
                               range = "A1:F104") %>%
  select(SITE_NO = SITE_NUMBER, DEER = `Woodland Deer Risk`) %>%
  mutate(DEER = ifelse(DEER == "None", "Low", DEER))