# NLCD data for NEON AOP footprints
# devtools::install_github("ropensci/FedData")

#install.packages("devtools")
#devtools::install_github("ropensci/FedData")

library(sf)
library(FedData)
library(dplyr)
library(tidyr)
# library(ggplot2)
# library(data.table)
library(glue)
library(fs)
library(raster)
library(leaflet)

data_dir <- "~/Downloads"
aop_kmz_filepath <- "https://www.neonscience.org/aop-flight-boundaries-kmz"
aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kmz")
#download.file(aop_kmz_filepath, destfile = aop_kmz_localfile)

#read in updated NEON AOP Footprints from NEON Website 
aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kml") # Donal had to convert KMZ to KML in Google Earth because he was missing KMZ driver for st_read()
#aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kmz")
aop_sf <- st_read(aop_kmz_localfile) %>% st_zm() %>% 
  mutate(Site = substr(Name, 1, 8), domain= substr(Name, 1, 3)) %>% 
  group_by(Site) %>%
  summarise_all(first) %>%
  st_cast("MULTIPOLYGON") ## Included these last few lines to combine the polygons based on site
                          ## This makes things easier and accounts for co-incidemt terrestrial and aquatic sites polygons

aop_sf2 <- st_read(aop_kmz_localfile) %>% st_zm() %>% 
  mutate(Site = substr(Name, 1, 8), domain= substr(Name, 1, 3)) %>% 
  aggregate(list(.$Site),first) %>%
  st_cast("MULTIPOLYGON") 

#Create leaflet map 
#Use this to view that polygons have merged correctly
#And that all are accounted for
aop_sf %>%
  leaflet() %>%
  addTiles(group = "Open Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
  addPolygons(popup = ~Name) %>%
  addLayersControl(baseGroups = c("Esri World Imagery", "Open Street Map"))


#NLCD Data 
nlcd_codes <- readr::read_csv("/Users/olearyd/Git/SESYNC-NEON-DATA/NLCD/nlcd_legend_2011.csv")
# https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend

# download.file("https://www.neonscience.org/science-design/field-sites/export", 
#               destfile = "neon-field-sites.csv")


#Use NEON site specific data to relate to NLCD data files 
#Remake table for all of the invididual polygons (p1, p2, p3) - if want all of the invidual polygon 
neon_site_data <- readr::read_csv("neon-field-sites.csv") %>% 
  dplyr::select(`Domain Number`, `Site ID`, State) %>%
  mutate(Site = glue::glue("{`Domain Number`}_{`Site ID`}")) %>%
  mutate(landmass = dplyr::case_when(State == "PR" ~ "PR",
                                     State == "AK" ~ "AK",
                                     State == "HI" ~ "HI",
                                     !State %in% c("PR", "AK", "HI") ~ "L48")) %>%
  mutate(nlcd_year = dplyr::case_when(State == "PR" ~ 2001,
                                      State == "AK" ~ 2011,
                                      State == "HI" ~ 2001,
                                      !State %in% c("PR", "AK", "HI") ~ 2016)) %>%
  dplyr::select(Site, landmass, nlcd_year) %>% 
  mutate(Site = as.character(Site)) %>%
  add_row(Site = "D05_CHEQ", landmass = "L48", nlcd_year = 2016) %>%
  add_row(Site = "D18_BARO", landmass = "AK", nlcd_year = 2011)

#Joining the same two columns together 
aop_x_sitedata <- aop_sf %>% full_join(neon_site_data)

## Losing some locations here. Why are they not present?
## Missing 2 from Hawaii, what about the others?

aop_sites <- unique(aop_sf$Name) %>% as.character()
aop_site <- aop_sites[5]
# for one site
#defining the function that takes as an input one neon site name/row
get_nlcd_percents <- function(site_number, dataset_type = "Land_Cover"){
  
  ## There is a problem filtering 
  ## Some have multiple rows because of the way they are being called
  ## filtering by aop_site returns multiple polygons for sites with 
  ## multiple ploygons! This throws an error in get_nlcd
  ## Might be better to loop through integers 1:nrow(aop_x_sitedata)
  
  ## Will need to fix label, aop_site_sf, and maybe others
  
  #aop_site <- aop_sites[site_number]
  
  aop_site_sf <- aop_x_sitedata %>% dplyr::filter(Name == aop_site)
  site_landmass <- aop_site_sf %>% pull(landmass)
  nlcd_year <- aop_site_sf %>% pull(nlcd_year)
  
  #print(paste(aop_site,nrow(aop_site_sf)))
  

  
  nlcd_site <- FedData::get_nlcd(aop_site_sf,
                                 label = aop_site,
                                 dataset = 'Land_Cover', 
                                 landmass = site_landmass,
                                 year = nlcd_year,
                                 force.redo = FALSE)
  
  #changing the projection of aop sites to align with nlcd raster data 
  aop_site_sf_prj <- aop_site_sf %>% st_transform(proj4string(nlcd_site))
  #making a mask value of na for the space outside of the polygon that is being included in the raster image 
  nlcd_site_mask <- raster::mask(nlcd_site, as(aop_site_sf_prj, "Spatial"))
  
  if(dataset_type == "Land_Cover"){
    #saving screenshot image of each of the footprints   
    filename <- glue::glue("outputs/landcover-{aop_site}-{nlcd_year}.png")
    png(filename)
    # nlcd_agg <- raster::disaggregate(nlcd_site, fact = 3)
    plot(nlcd_site, maxpixels=1e8, mar = c(1,1,1,1), mfrow = c(1,1))
    plot(st_geometry(aop_site_sf_prj), add = TRUE, col = NA, border = "red")
    dev.off()    
    
    # tabulate number of cells in each type and 
    # Merge with legend to see land cover types
    # % of each landcover in footprint thats selected
    cover <- raster::freq(nlcd_site_mask) %>%
      as.data.frame() %>%
      dplyr::filter(!is.na(value)) %>% #removing NA values 
      dplyr::left_join(nlcd_codes, by = c("value" = "Class")) %>% #merging w/ nldc column labels 
      dplyr::mutate(percent_cover = count/sum(count)) %>% #calculating % cover from frequency 
      dplyr::select(class_name, count, percent_cover) %>% #picking certain columns 
      mutate(Site = aop_site) #merging column with site name 
    
  }
  #using nlcd to calculate impervious surface   
  if(dataset_type == "Impervious"){
    hist_data <- hist(nlcd_site_mask, plot = FALSE,
                      breaks = seq(0,100, 5))
    hist_df <- data.frame(site = aop_site,
                          imperv_class = hist_data$breaks[2:21],
                          imperv_freq = hist_data$counts,
                          stringsAsFactors = FALSE)
    
    hist_df %>% readr::write_csv(glue::glue("data/nlcd/imperv_hist_data-{dataset_type}-{aop_site}-{nlcd_year}.csv"))
    
    hist(nlcd_site_mask,
         main = glue("Distribution of impervious cover at {aop_site} {nlcd_year}"),
         xlab = "Impervious (%)", 
         ylab = "Number of Pixels",
         col = "springgreen")
    
    filename <- glue::glue("impervious-{aop_site}-{nlcd_year}.png")
    png(filename)
    plot(nlcd_site, maxpixels=1e8, mar = c(1,1,1,1), mfrow = c(1,1))
    plot(st_geometry(aop_site_sf_prj), add = TRUE, col = NA, border = "green")
    dev.off()    
    
    # area above 0%, 5%, 10%, 50% impervious
    
    filename <- glue::glue("impervious-hist-{aop_site}-{nlcd_year}.pdf")
    pdf(filename)
    hist(nlcd_site_mask,
         main = glue("Distribution of impervious cover at {aop_site} {nlcd_year}"),
         xlab = "Impervious (%)", 
         ylab = "Number of Pixels",
         col = "springgreen")
    dev.off()
    
    rcl <- matrix(c(-Inf, 5, 100,
                    5, 10, 200, 
                    10, 50, 300,
                    50, 100, 400), ncol = 3, byrow = TRUE)
    imp_rcl <- nlcd_site_mask %>%
      raster::reclassify(rcl, include.lowest = TRUE)
    
    imp_val_df <- data.frame(value = c(100, 200, 300, 400),
                             class = c("below 5 percent", "5-10%", "10-50%", "over 50%"),
                             stringsAsFactors = FALSE)
    # tabulate area in each class
    cover <- raster::freq(imp_rcl) %>%
      as.data.frame() %>%
      dplyr::left_join(imp_val_df, by = "value") %>%
      dplyr::mutate(area_m2 = count*(30*30)) %>%
      mutate(Site = aop_site)
  }
  
  
  cover %>% readr::write_csv(glue::glue("outputs/{dataset_type}-{aop_site}-{nlcd_year}.csv"))
  
}

# aop_noD18 <- aop_sites#[-c(1:57)]
# 
# get_nlcd_percents(aop_noD18[1], dataset_type = "Impervious")
# get_nlcd_percents(aop_sites[1], dataset_type = "Land_Cover")

#runs the function for all aop sites 

f2=purrr::safely(get_nlcd_percents, otherwise=print(aop_site))

purrr::walk(1:nrow(aop_x_sitedata), ~f2(.x, dataset_type = "Land_Cover"))
purrr::walk(aop_noD18, ~get_nlcd_percents(.x, dataset_type = "Impervious"))


# combine land cover data into one table and save as csv

all_aop_landcover <- fs::dir_ls("data/nlcd", regexp = "Land_Cover") %>%
  purrr::map_df(~readr::read_csv(.x)) %>%
  mutate(percent_cover = percent_cover*100) %>%
  tidyr::spread(key = class_name, value = percent_cover, fill = 0) %>%
  mutate(all_developed = `Developed High Intensity` + `Developed, Low Intensity` +
           `Developed, Medium Intensity` + `Developed, Open Space`) %>%
  arrange(-all_developed)

all_aop_landcover %>% readr::write_csv(file.path(data_dir, "NEON-AOP-LandCover.csv"))

all_aop_landcover_areas <- fs::dir_ls("data/nlcd", regexp = "Land_Cover") %>%
  purrr::map_df(~readr::read_csv(.x)) %>% 
  mutate(area_ha = (count*30)/1e4) %>%
  dplyr::select(-count, -percent_cover) %>%
  tidyr::spread(key = class_name, value = area_ha, fill = 0) %>%
  mutate(all_developed = `Developed High Intensity` + `Developed, Low Intensity` +
           `Developed, Medium Intensity` + `Developed, Open Space`) %>%
  arrange(-all_developed)

all_aop_landcover_areas %>% readr::write_csv(file.path(data_dir, "NEON-AOP-LandCover_areas.csv"))

# combine impervious data into one table and save as csv

all_aop_impervious <- fs::dir_ls("data/nlcd", regexp = "Impervious") %>%
  purrr::map_df(~readr::read_csv(.x)) %>%
  dplyr::select(Site, area_m2, class) %>%
  tidyr::spread(key = class, value = area_m2,2, fill = 0) %>%
  dplyr::select(Site, 4, 3, 2, 5, 6)

all_aop_impervious %>% readr::write_csv(file.path(data_dir, "NEON-AOP-Impervioius.csv"))
