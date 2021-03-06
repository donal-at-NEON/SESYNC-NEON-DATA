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

### Will need to read in Hawaii KMLs as well, merge into single sf object
aop_kmz_H1 <- glue::glue("{data_dir}/D20_PUUM_C1_P1_v3.kml")
aop_kmz_H2 <- glue::glue("{data_dir}/D20_PUUM_C1_P2_v3.kml")

H1 <- st_read(aop_kmz_H1)
H2 <- st_read(aop_kmz_H2)

H_comb <- rbind(H1,H2)
H_comb$Name=c("D20_PUUM_C1_P1_v3","D20_PUUM_C1_P2_v3")
H_comb <- H_comb %>%
  mutate(Site = substr(Name, 1, 8), domain= substr(Name, 1, 3)) %>% 
  group_by(Site) %>%
  summarise_all(first)

#read in updated NEON AOP Footprints from NEON Website 
aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kml") # Donal had to convert KMZ to KML in Google Earth because he was missing KMZ driver for st_read()
#aop_kmz_localfile <- glue::glue("{data_dir}/aop-flight-boundaries.kmz")
aop_sf <- st_read(aop_kmz_localfile) %>% st_zm() %>% 
  mutate(Site = substr(Name, 1, 8), domain= substr(Name, 1, 3)) %>% 
  group_by(Site) %>%
  summarise_all(first) %>%
  st_cast("MULTIPOLYGON") ## Included these last few lines to merge the polygons based on site
                          ## This makes things easier and accounts for co-incidemt terrestrial and aquatic sites polygons

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
# This set of lines is a complex 'dplyr' command that reads in the neon-field-sites.csv and
# defines two important attributes for each site- the 'landmass' and 'nlcd_year' attributes, which
# are later used in the 'FedData::get_nlcd()' function. 
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
# This line combines the list of NEON sites and their landmass/nlcd_year with the
# merged polygons from the KMZ files
aop_x_sitedata <- aop_sf %>% left_join(neon_site_data)
# some of the sites from the CSV will have no match with polygons from the KMZ
# this may be for several reasons, including: no AOP coverage at that site, or that site
# may be adjacent to a larger site (e.g., an aquatic site nested within a larger terrestrial site)
# and therefore the AOP footprint encompasses both and is named for the larger site.

## Missing 2 from Hawaii, what about the others?

# This line names a new vector of simple character stringsite names from the polygons, 
# which will then be used when looping through each site.
aop_sites <- unique(aop_sf$Name) %>% as.character()

aop_site <- aop_sites[5]
# for one site

#defining the function that takes as an input one neon site name/row
get_nlcd_percents <- function(aop_site, dataset_type = "Land_Cover"){
  
  #(optional) At the beginning of each loop, print out which site is being analyzed
  print(aop_site)
  
  # First, pick the polygon with name that matches aop_site
  aop_site_sf <- aop_x_sitedata %>% dplyr::filter(Name == aop_site)
  # Then, extract the 'landmass' and 'nlcd_year' variables associated with that site
  site_landmass <- aop_site_sf %>% pull(landmass)
  nlcd_year <- aop_site_sf %>% pull(nlcd_year)
  
  # Call the NLCD servers to retrieve the Landcover -or- Imperviousness data
  # This call requires the 'landmass' and 'nlcd_year' attributes to determine which 
  # database to pull from. It also uses the AOP flightbox polygon extent to clip the NLCD rasters.
  # To determine the dataset to select, this call will use the 'dataset_type' variable - which is 
  # set to 'Land_Cover' be default in the function definition (above), but can also be passed
  # 'Impervious' as shown in the purrr::walk() function below
  nlcd_site <- FedData::get_nlcd(aop_site_sf,
                                 label = aop_site,
                                 dataset = dataset_type, 
                                 landmass = site_landmass,
                                 year = nlcd_year,
                                 force.redo = FALSE)
  
  #changing the projection of aop sites to align with nlcd raster data 
  aop_site_sf_prj <- aop_site_sf %>% st_transform(proj4string(nlcd_site))
  #making a mask value of NA for the space outside of the polygon that is being included in the raster image 
  nlcd_site_mask <- raster::mask(nlcd_site, as(aop_site_sf_prj, "Spatial"))
  
  ## Next, we do two separate processes depending on the data type (Land_Cover or Impervious)
  if(dataset_type == "Land_Cover"){
    
    #saving screenshot image of each of the footprints   
    filename <- glue::glue("plots/nlcd/landcover-{aop_site}-{nlcd_year}.png")
    png(filename)
    # nlcd_agg <- raster::disaggregate(nlcd_site, fact = 3)
    plot(nlcd_site, maxpixels=1e8, mar = c(1,1,1,1), mfrow = c(1,1))
    plot(st_geometry(aop_site_sf_prj), add = TRUE, col = NA, border = "red", lwd=2)
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
    
    ## 'cover' data.frame produced in both 'Land_Cover' subloop (here) and 'Impervious' subloop (below)
    #  in both cases, 'cover' is eventually written as a CSV at the end of the 'get_nlcd_percents()' function
    
  }
  
  #using nlcd to calculate impervious surface   
  if(dataset_type == "Impervious"){
    
    # First, we define a histogram (but don't plot it) of the imperviousness data
    # to break the dataset into 20 evenly spaced bins
    hist_data <- hist(nlcd_site_mask, plot = FALSE,
                      breaks = seq(0,100, 5))
    hist_df <- data.frame(site = aop_site,
                          imperv_class = hist_data$breaks[2:21],
                          imperv_freq = hist_data$counts,
                          stringsAsFactors = FALSE)
    
    # We save the bins and their counts as a CSV, rather than plotting it.
    hist_df %>% readr::write_csv(glue::glue("data/nlcd/imperv_hist_data-{dataset_type}-{aop_site}-{nlcd_year}.csv"))
    
    # We also plot a histogram of impervious survace % values while we're processing
    hist(nlcd_site_mask,
         main = glue("Distribution of impervious cover at {aop_site} {nlcd_year}"),
         xlab = "Impervious (%)", 
         ylab = "Number of Pixels",
         col = "springgreen")
    
    # Let's save a PNG image of the impervious surface map too, with the AOP flight 
    # box(es) superimposed on top.
    filename <- glue::glue("plots/nlcd/impervious-{aop_site}-{nlcd_year}.png")
    
    png(filename) #png() (or its friends jpeg(), pdf(), etc.) opens a 'graphic device' to produce
                  #an image of that type. This is as if the computer has defined a new blank canvas
                  #with the name 'filename' that is a PNG, and whatever you plot/draw onto that
                  #canvas will be saved as the image.
    plot(nlcd_site, maxpixels=1e8, mar = c(1,1,1,1), mfrow = c(1,1))
    plot(st_geometry(aop_site_sf_prj), add = TRUE, col = NA, border = "green", lwd=2)
    dev.off() # dev.off() defines the end of the image making process, and closes the graphic device
    
    # We will also save a PDF image of the impervious surface histogram to view later
    filename <- glue::glue("plots/nlcd/impervious-hist-{aop_site}-{nlcd_year}.pdf")
    pdf(filename) #open a new PDF here
    hist(nlcd_site_mask,
         main = glue("Distribution of impervious cover at {aop_site} {nlcd_year}"),
         xlab = "Impervious (%)", 
         ylab = "Number of Pixels",
         col = "springgreen")
    dev.off() # dev.off() defines the end of the image making process, and closes the graphic device
    
    # Here we are defining a new matrix called 'rcl' (short for 'reclassification matrix')
    rcl <- matrix(c(-Inf, 5, 100, #Values between -Inf and 5 -> 100
                    5, 10, 200,   #Values between 5 and 10 -> 200
                    10, 50, 300,  #Values between 10 and 50 -> 300
                    50, 100, 400),#Values between 50 and 100 -> 400
                  ncol = 3, byrow = TRUE)
    
    # We use 'rcl' to reclassify the impervious surface raster into classes
    imp_rcl <- nlcd_site_mask %>%
      raster::reclassify(rcl, include.lowest = TRUE)
    
    # make a new data.frame of classification values and their corresponding labels.
    imp_val_df <- data.frame(value = c(100, 200, 300, 400),
                             class = c("below 5 percent", "5-10%", "10-50%", "over 50%"),
                             stringsAsFactors = FALSE) # This keeps the labels as strings, which makes things easier later
    
    # tabulate area in each class
    # and save to a new data.frame called 'cover'
    cover <- raster::freq(imp_rcl) %>% # raster::freq() makes a frequency table - a count of how many cells have each unique value in the raster (100, 200, 300, 400, NA)
      as.data.frame() %>%              # convert that frequency table into a data.frame
      dplyr::left_join(imp_val_df, by = "value") %>% # join the frequency data.fram with the 'labels' data.frame created above
      dplyr::mutate(area_m2 = count*(30*30)) %>%     # make a new column called 'area_m2' and multiply the number of cells (which are based on 30mX30m Landsat pixles) by 900 to convert to square meters
      mutate(Site = aop_site) # make a new column called 'Site' that uses the aop_site variable that was passed to the original call in the purrr:walk function below
  }
  
  # Finally, write out the 'cover' data.frame (from 'Land_Cover' or 'Impervious' as a CSV
  cover %>% readr::write_csv(glue::glue("data/nlcd/{dataset_type}-{aop_site}-{nlcd_year}.csv"))
  
}

## This is an old chunk of code that got rid of the domains in Alaska, because
#  there is no impveriousness data available for AK. This is no longer needed
#  now that we have wrapped our function in the 'safely' wrapper which can 
#  allow for this error without crashing the 'walk' loop.

# aop_noD18 <- aop_sites#[-c(1:57)]
# 
# get_nlcd_percents(aop_noD18[1], dataset_type = "Impervious")
# get_nlcd_percents(aop_sites[1], dataset_type = "Land_Cover")


## To handle any errors from the get_nlcd_percents() function, 
#  we will wrap it in the 'purrr::safely()' function, which can
#  allow an error to happen without crashing the loop.
f2_LC=purrr::safely(get_nlcd_percents, quiet=F)

## The 'walk()' function loops through each site in the aop_sites vector
#  and applies our 'safely()'-wrapped get_nlcd_percents function
purrr::walk(aop_sites, ~f2_LC(.x, dataset_type = "Land_Cover"))

# Do it again for imperviousness - will throw error with AK sites but that's ok, they just don't have imperviousness data
f2_IMP=purrr::safely(get_nlcd_percents, quiet=F)
purrr::walk(aop_sites, ~f2_IMP(.x, dataset_type = "Impervious"))
#purrr::walk(aop_noD18, ~f2_IMP(.x, dataset_type = "Impervious")) <-- old line using the list that omits AK


## After running the two 'walk()' functions above, you should have results and plots
#  for each site in the aop_sites vector. These next steps read all of those individual
#  results CSVs and combine them into a single table for comparison, and easy portability (as a single CSV)

# combine land cover data into one table and save as csv
data_dir <- "~/Git/SESYNC-NEON-DATA/data/results/"

all_aop_landcover <- fs::dir_ls("data/nlcd", regexp = "Land_Cover") %>%
  purrr::map_df(~readr::read_csv(.x)) %>%
  mutate(percent_cover = percent_cover*100) %>%
  tidyr::spread(key = class_name, value = percent_cover, fill = 0) %>%
  mutate(all_developed = `Developed High Intensity` + `Developed, Low Intensity` +
           `Developed, Medium Intensity` + `Developed, Open Space`) %>%
  arrange(-all_developed)

all_aop_landcover %>% readr::write_csv(file.path(data_dir, "NEON-AOP-LandCover.csv"))


# Convert landcover to area and save that too.
all_aop_landcover_areas <- fs::dir_ls("data/nlcd", regexp = "Land_Cover") %>%
  purrr::map_df(~readr::read_csv(.x)) %>% 
  mutate(area_ha = (count*30*30)/1e6) %>%
  #######################################################################################
  ### Should it be count*30*30?? and 1e6??
  #######################################################################################
  dplyr::select(-count, -percent_cover) %>%
  tidyr::spread(key = class_name, value = area_ha, fill = 0) %>%
  mutate(all_developed = `Developed High Intensity` + `Developed, Low Intensity` +
           `Developed, Medium Intensity` + `Developed, Open Space`) %>%
  arrange(-all_developed)

all_aop_landcover_areas %>% readr::write_csv(file.path(data_dir, "NEON-AOP-LandCover_areas.csv"))


##### ERROR
## Problem here, when using 'Impervious' as regex, it finds both the %cover and histogram CSVs.
## This is an issue because the two table formats are not compatable.
## Better way to choose only the %cover CSV? Maybe rename HIST output so that it doesn't contain 'dataset_type'
##they both don't get
## Picked up and manipulated? 

# combine impervious data into one table and save as csv
all_aop_impervious <- fs::dir_ls("data/nlcd", regexp = "Impervious") %>%
  purrr::map_df(~readr::read_csv(.x)) %>%
  dplyr::select(Site, area_m2, class) 

# this line handles all of the 'NA' values present in the imperviousness data
# Donal assumes that these 'NA's are no impverviousness, but may also just be undefined
# For example, many areas around MOAB are impervious slickrock, but undefined. Many
# undefined areas in forests are likely to be highly permiable to precipitation. Therefore,
# it may be difficult to determine an imperviousness classification in general for each area.
all_aop_impervious$class[is.na(all_aop_impervious$class)] <- "Undefined"

# Drop all of the rows with no 'Site' 
all_aop_impervious <- all_aop_impervious[complete.cases(all_aop_impervious),]

all_aop_impervious <- all_aop_impervious %>%
  tidyr::spread(key = class, value = area_m2,2, fill = 0) %>% ## What is the '2' doing in there? Typo?
  dplyr::select(Site, 4, 3, 2, 5, 6)

all_aop_impervious %>% readr::write_csv(file.path(data_dir, "NEON-AOP-Impervioius.csv"))

