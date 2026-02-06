library(tidyverse) 
library(readr)      
library(lubridate) 
library(vegan)   
library(data.table)
library(factoextra)
library(gridExtra) 
library(tidytext) 
library(scales)
library(ragg)
library(nlme)
library(performance)
library(emmeans)


dc_2011 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2011_complete.csv", header = T)
dc_2012 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2012_complete.csv", header = T)
dc_2013 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2013_complete.csv", header = T)
dc_2014 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2014_complete.csv", header = T)
dc_2015 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2015_complete.csv", header = T)
dc_2016 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2016_complete.csv", header = T)
dc_2017 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2017_complete.csv", header = T)
dc_2018 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2018_complete.csv", header = T)
dc_2019 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2019_complete.csv", header = T)
dc_2020 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2020_complete.csv", header = T)
dc_2021 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/EA_DC_2021_complete.csv", header = T)
dc_2022 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/DC-2022.csv", header = T)
dc_2023 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/DC-2023.csv", header = T)
dc_2024 <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Water Sampling/DC-2024.csv", header = T)

colnames(dc_2011) # check column names
colnames(dc_2024)

# Combine all years into one dataframe
bound <- rbind(dc_2011,dc_2012, dc_2013, dc_2014, dc_2015, dc_2016, dc_2017, dc_2018, dc_2019, dc_2020, dc_2021,dc_2022, dc_2023, dc_2024) 
write.csv(bound, file = "bound.csv", row.names = F) 

bound <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/bound.csv", header = T)

#####################################################################
# FILTER: Keep relevant sample type + select metal determinands
#####################################################################

RIVER <- bound[grep("RIVER / RUNNING SURFACE WATER", bound$sample.sampledMaterialType.label), ]

ALL_WATER <- bound[grep("WATER", bound$sample.sampledMaterialType.label), ]

metal_patterns <- c("Cadmium", "Copper", "Zinc", "Lead", "Nickel", "Arsenic", "Iron", "Aluminium") 

#Find text that contains the metals above
pattern <- paste(metal_patterns, collapse = "|") # "Cadmium|Copper|Zinc|..."

RIVER_metals <- RIVER[grepl(pattern, RIVER$determinand.label, ignore.case = T), ]

# Inspect what determinands survived the filter
sort(unique(RIVER_metals$determinand.label))
dim(RIVER_metals)

# Filter the full ALL_WATER 
ALL_WATER_metals <- ALL_WATER[grepl(pattern, ALL_WATER$determinand.label, ignore.case = T), ]
sort(unique(ALL_WATER_metals$determinand.label))
dim(ALL_WATER_metals)

# Filter the metal dataset to rows whose samplingPoint notation matches any of those IDs.
###1 HAYLE
Hayle <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/Hayle_sampling_points.csv", header=T)
HayleIDs <- Hayle$ID
RIVER_Hayle <- RIVER_metals[grepl(paste(HayleIDs, collapse="|"), RIVER_metals$sample.samplingPoint.notation), ]

###2 RED RIVER

RR <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/Red_river_sampling_points.csv", header=T)
RRIDs <- RR$ID
Red_RIVER <- RIVER_metals[grepl(paste(RRIDs, collapse="|"), RIVER_metals$sample.samplingPoint.notation), ]

####3 CARNON RIVER, one site is minewater

CR <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/Carnon_river_sampling_points.csv" , header=T)
CRIDs <- CR$ID
# Carnon_RIVER2 <- RIVER_metals[grepl(paste(CRIDs, collapse="|"), RIVER$sample.samplingPoint.notation), ]
Carnon_RIVER <- ALL_WATER_metals[grepl(paste(CRIDs, collapse="|"), ALL_WATER_metals$sample.samplingPoint.notation), ]

####4 RIVER COBER

CoR <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/Cober_river_sampling_points.csv" , header=T)
CoRIDs <- CoR$ID
Cober_RIVER <- RIVER_metals[grepl(paste(CoRIDs, collapse="|"), RIVER_metals$sample.samplingPoint.notation), ]

###############################################################################
# BASELINES: Compute minimum “baseline” value per metal per catchment
###############################################################################

# For each river, for each metal:
# Filter to that determinand label
# Convert results to numeric
# Take minimum value across all years/sites (ignoring NA)

#### Hayle ### 
min_zinc_H <- RIVER_Hayle %>%
  filter(determinand.label == "Zinc - as Zn") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_zinc_H = min(result, na.rm = TRUE)) %>%
  pull(min_zinc_H)

min_zinc_H
#  16

min_cadmium_H <- RIVER_Hayle %>%
  filter(determinand.label == "Cadmium - Cd") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_cadmium_H = min(result, na.rm = TRUE)) %>%
  pull(min_cadmium_H)

min_cadmium_H
#  0.1

min_copper_H <- RIVER_Hayle %>%
  filter(determinand.label == "Copper - Cu") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_copper_H = min(result, na.rm = TRUE)) %>%
  pull(min_copper_H)

min_copper_H
# 2.36

min_arsenic_H <- RIVER_Hayle %>%
  filter(determinand.label == "Arsenic - As") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_arsenic_H = min(result, na.rm = TRUE)) %>%
  pull(min_arsenic_H)

min_arsenic_H
# 1

min_lead_H <- RIVER_Hayle %>%
  filter(determinand.label == "Lead - as Pb") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_lead_H = min(result, na.rm = TRUE)) %>%
  pull(min_lead_H)

min_lead_H
# 0.1

min_iron_H <- RIVER_Hayle %>%
  filter(determinand.label == "Iron - as Fe") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_iron_H = min(result, na.rm = TRUE)) %>%
  pull(min_iron_H)

min_iron_H
# 33.6

min_nickel_H <- RIVER_Hayle %>%
  filter(determinand.label == "Nickel - Ni") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_nickel_H = min(result, na.rm = TRUE)) %>%
  pull(min_nickel_H)

min_nickel_H
#  1

min_aluminium_H <- RIVER_Hayle %>%
  filter(determinand.label == "Aluminium-Al") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_aluminium_H = min(result, na.rm = TRUE)) %>%
  pull(min_aluminium_H)

min_aluminium_H
# 10


#### Red ####
min_zinc_R <- Red_RIVER %>%
  filter(determinand.label == "Zinc - as Zn") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_zinc_R = min(result, na.rm = TRUE)) %>%
  pull(min_zinc_R)

min_zinc_R
#  9.4

min_cadmium_R <- Red_RIVER %>%
  filter(determinand.label == "Cadmium - Cd") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_cadmium_R = min(result, na.rm = TRUE)) %>%
  pull(min_cadmium_R)

min_cadmium_R
#  0.1

min_copper_R <- Red_RIVER %>%
  filter(determinand.label == "Copper - Cu") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_copper_R = min(result, na.rm = TRUE)) %>%
  pull(min_copper_R)

min_copper_R
# 5.9

min_arsenic_R <- Red_RIVER %>%
  filter(determinand.label == "Arsenic - As") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_arsenic_R = min(result, na.rm = TRUE)) %>%
  pull(min_arsenic_R)

min_arsenic_R
# 3.8

min_lead_R <- Red_RIVER %>%
  filter(determinand.label == "Lead - as Pb") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_lead_R = min(result, na.rm = TRUE)) %>%
  pull(min_lead_R)

min_lead_R
# 0.1

min_iron_R <- Red_RIVER %>%
  filter(determinand.label == "Iron - as Fe") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_iron_R = min(result, na.rm = TRUE)) %>%
  pull(min_iron_R)

min_iron_R
# 118

min_nickel_R <- Red_RIVER %>%
  filter(determinand.label == "Nickel - Ni") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_nickel_R = min(result, na.rm = TRUE)) %>%
  pull(min_nickel_R)

min_nickel_R
#  1

min_aluminium_R <- Red_RIVER %>%
  filter(determinand.label == "Aluminium-Al") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_aluminium_R = min(result, na.rm = TRUE)) %>%
  pull(min_aluminium_R)

min_aluminium_R
# 51


#### Carnon ###
min_zinc_C <- Carnon_RIVER %>%
  filter(determinand.label == "Zinc - as Zn") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_zinc_C = min(result, na.rm = TRUE)) %>%
  pull(min_zinc_C)

min_zinc_C
#  152

min_cadmium_C <- Carnon_RIVER %>%
  filter(determinand.label == "Cadmium - Cd") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_cadmium_C = min(result, na.rm = TRUE)) %>%
  pull(min_cadmium_C)

min_cadmium_C
#  0.565

min_copper_C <- Carnon_RIVER %>%
  filter(determinand.label == "Copper - Cu") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_copper_C = min(result, na.rm = TRUE)) %>%
  pull(min_copper_C)

min_copper_C
# 21.5

min_arsenic_C <- Carnon_RIVER %>%
  filter(determinand.label == "Arsenic - As") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_arsenic_C = min(result, na.rm = TRUE)) %>%
  pull(min_arsenic_C)

min_arsenic_C
# 3.42

min_lead_C <- Carnon_RIVER %>%
  filter(determinand.label == "Lead - as Pb") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_lead_C = min(result, na.rm = TRUE)) %>%
  pull(min_lead_C)

min_lead_C
# 2

min_iron_C <- Carnon_RIVER %>%
  filter(determinand.label == "Iron - as Fe") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_iron_C = min(result, na.rm = TRUE)) %>%
  pull(min_iron_C)

min_iron_C
# 30

min_nickel_C <- Carnon_RIVER %>%
  filter(determinand.label == "Nickel - Ni") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_nickel_C = min(result, na.rm = TRUE)) %>%
  pull(min_nickel_C)

min_nickel_C
#  1.47

min_aluminium_C <- Carnon_RIVER %>%
  filter(determinand.label == "Aluminium-Al") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_aluminium_C = min(result, na.rm = TRUE)) %>%
  pull(min_aluminium_C)

min_aluminium_C
# 10

### Cober ###
min_zinc <- Cober_RIVER %>%
  filter(determinand.label == "Zinc - as Zn") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_zinc = min(result, na.rm = TRUE)) %>%
  pull(min_zinc)

min_zinc
#  14.9

min_cadmium <- Cober_RIVER %>%
  filter(determinand.label == "Cadmium - Cd") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_cadmium = min(result, na.rm = TRUE)) %>%
  pull(min_cadmium)

min_cadmium
#  0.1

min_copper <- Cober_RIVER %>%
  filter(determinand.label == "Copper - Cu") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_copper = min(result, na.rm = TRUE)) %>%
  pull(min_copper)

min_copper
# 8.37

min_arsenic <- Cober_RIVER %>%
  filter(determinand.label == "Arsenic - As") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_arsenic = min(result, na.rm = TRUE)) %>%
  pull(min_arsenic)

min_arsenic
# 1

min_lead <- Cober_RIVER %>%
  filter(determinand.label == "Lead - as Pb") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_lead = min(result, na.rm = TRUE)) %>%
  pull(min_lead)

min_lead
# 2

min_iron <- Cober_RIVER %>%
  filter(determinand.label == "Iron - as Fe") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_iron = min(result, na.rm = TRUE)) %>%
  pull(min_iron)

min_iron
# 283

min_nickel <- Cober_RIVER %>%
  filter(determinand.label == "Nickel - Ni") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_nickel = min(result, na.rm = TRUE)) %>%
  pull(min_nickel)

min_nickel
#  1

min_aluminium <- Cober_RIVER %>%
  filter(determinand.label == "Aluminium-Al") %>%
  mutate(result = as.numeric(result)) %>%
  summarise(min_aluminium = min(result, na.rm = TRUE)) %>%
  pull(min_aluminium)

min_aluminium
# 59.6

## hardcode the baseline values into a tidy table per catchment
baseline_tbl_hayle <- tibble::tribble(
  ~determinand.label, ~baseline,
  "Aluminium-Al", 10,
  "Arsenic - As", 1,
  "Cadmium - Cd", 0.1,
  "Copper - Cu",  2.36,
  "Iron - as Fe", 33.6,
  "Lead - as Pb", 0.1,
  "Nickel - Ni",  1,
  "Zinc - as Zn", 16)

baseline_tbl_red <- tibble::tribble(
  ~determinand.label, ~baseline,
  "Aluminium-Al", 51,
  "Arsenic - As", 3.8,
  "Cadmium - Cd", 0.1,
  "Copper - Cu",  5.9,
  "Iron - as Fe", 118,
  "Lead - as Pb", 0.1,
  "Nickel - Ni",  1,
  "Zinc - as Zn", 9.4)

baseline_tbl_carnon <- tibble::tribble(
  ~determinand.label, ~baseline,
  "Aluminium-Al", 10,
  "Arsenic - As", 3.42,
  "Cadmium - Cd", 0.565,
  "Copper - Cu",  21.5,
  "Iron - as Fe", 30,
  "Lead - as Pb", 2,
  "Nickel - Ni",  1.47,
  "Zinc - as Zn", 152)

baseline_tbl_cober <- tibble::tribble(
  ~determinand.label, ~baseline,
  "Aluminium-Al", 59.6,
  "Arsenic - As", 1,
  "Cadmium - Cd", 0.1,
  "Copper - Cu",  8.37,
  "Iron - as Fe", 283,
  "Lead - as Pb", 2,
  "Nickel - Ni",  1,
  "Zinc - as Zn", 14.9)

###############################################################################
#PREP METALS: Parse results, bin Month into seasons, and recode Location names
###############################################################################

# Why: your 16S metadata uses your short site names + Month bins. EA data doesn’t.
# This step forces alignment so joins work cleanly later.

hayle_metals_prepped <- RIVER_Hayle %>%
  mutate(result = parse_number(as.character(result)),
         month_num = month(sample.sampleDateTime),
         Month = case_when(month_num %in% 1:3   ~ "January",
                           month_num %in% 4:6   ~ "April",
                           month_num %in% 7:9   ~ "July",
                           month_num %in% 10:12 ~ "October"), # turn month number into season-bin label
         Location = recode(sample.samplingPoint.label, # rename long EA labels to site names that align with metadata
                           "RIVER HAYLE ABOVE CROWAN" = "Above crowan",
                           "RIVER HAYLE AT B3303 BRIDGE  CROWAN" = "Clowance",
                           "RIVER HAYLE AT DRYM FARM" = "Drym",
                           "RIVER HAYLE AT GODOLPHIN BRIDGE" = "Godolphin",
                           "RIVER HAYLE AT RELUBBUS" = "Relubbas",
                           "RIVER HAYLE AT ST ERTH GAUGING STATION" = "St Erth")) %>%
  select(Location, Month, determinand.label, result, everything())

red_metals_prepped <- Red_RIVER %>%
  mutate(result = parse_number(as.character(result)),
         month_num = month(sample.sampleDateTime),
         Month = case_when(month_num %in% 1:3   ~ "January",
                           month_num %in% 4:6   ~ "April",
                           month_num %in% 7:9   ~ "July",
                           month_num %in% 10:12 ~ "October"),
         Location = recode(sample.samplingPoint.label,
                           "RED RIVER ABOVE BREA TIN WORKS" = "Brea addit",
                           "RED RIVER AT GWITHIAN TOWANS" = "Gwithian",
                           "RED RIVER AT KIEVE BRIDGE" = "Kieve",
                           "RED RIVER AT ROSCROGGAN BRIDGE" = "Tolvaddon",
                           "RED RIVER AT ROSEWARNE FARM" = "Roscroggan",
                           "RED RIVER D/S TRESKILLARD STREAM CONF" = "Brea village",
                           "RED RIVER U/S SOUTH CROFTY PLANT/MILL" = "Tuckingmill")) %>%
  select(Location, Month, determinand.label, result, everything())

carnon_metals_prepped <- Carnon_RIVER %>%
  mutate(result = parse_number(as.character(result)),
         month_num = month(sample.sampleDateTime),
         Month = case_when( month_num %in% 1:3   ~ "January",
                            month_num %in% 4:6   ~ "April",
                            month_num %in% 7:9   ~ "July",
                            month_num %in% 10:12 ~ "October"),
         Location = recode(sample.samplingPoint.label,
                           "CARNON RIVER AT BISSOE BR GAUGING STN" = "Bissoe",
                           "HICKS MILL STREAM AT COMFORD" = "Gwennap road",
                           "HICKS MILL STREAM AT GWENNAP CHURCH" = "Gwennap road",
                           "HICKS MILL STREAM AT PENPONS MILL" = "Lanner",
                           "HICKS MILL STREAM D/S LANNER ST DAY STW" = "Cusgarne junction",
                           "RIVER CARNON U/S GRENNA BRIDGE" = "Grenna",
                           "TRESAVEAN LEAT" = "Lanner")) %>%
  select(Location, Month, determinand.label, result, everything())

cober_metals_prepped <- Cober_RIVER %>%
  mutate(result = parse_number(as.character(result)),
         month_num = month(sample.sampleDateTime),
         Month = case_when(month_num %in% 1:3   ~ "January",
                           month_num %in% 4:6   ~ "April",
                           month_num %in% 7:9   ~ "July",
                           month_num %in% 10:12 ~ "October"),
         Location = recode(sample.samplingPoint.label,
                           "MEDLYN STREAM AT CHY BRIDGE" = "Medlyn farm",
                           "RIVER COBER AT COVERACK BRIDGE" = "Coverack bridges upstream",
                           "RIVER COBER AT HELSTON PARK G STN" = "Helston/Penrose",
                           "RIVER COBER AT LOWERTOWN BRIDGE" = "Lowertown",
                           "RIVER COBER AT ST JOHNS BRIDGE" = "Helston/Penrose",
                           "RIVER COBER AT TRENEAR BRIDGE" = "Trenear bridge (Poldark mine)")) %>%
  select(Location, Month, determinand.label, result, everything())


#################################################
# Read alpha diversity + sample metadata + merge
#################################################

# Read in alpha diversity results table
alpha <- read.table("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/16S_project_data_June25/alpha_results.text", 
                    header = T, sep = "", fill = T, strip.white = T, check.names = F)

# Read in sample metadata
samples_metadata <- read_csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/16S_project_data_June25/samples_metadata.csv")

# Identify SampleIDs that occur in BOTH datasets
common_samples <- intersect(alpha$SampleID, samples_metadata$SampleID)

# Count how many SampleIDs match between the two datasets
length(common_samples) 

# List SampleIDs that are in alpha results but NOT in metadata
setdiff(alpha$SampleID, samples_metadata$SampleID) 

# List SampleIDs that are in metadata but NOT in alpha results
setdiff(samples_metadata$SampleID, alpha$SampleID)

# Merge alpha and  metadata into one combined dataframe. Joined by SampleID (only matching SampleIDs will be included by default)
alphadf <- merge(alpha, samples_metadata, by = "SampleID") # Make one big dataset side by side

# Quick check of the first few rows of the merged dataset
head(alphadf) 


###############################################################################
# BUILD METALS WIDE TABLES: Location × Month grid + fill missing with baseline
###############################################################################

# Get the Location × Month combinations that exist in data for Hayle
hayle_grid_LM <- alphadf %>% filter(Catchment == "Hayle") %>% distinct(Location, Month)

# Expand that grid to include every metal (so each Location x Month has all metals listed)
hayle_metals_filled_long <- hayle_grid_LM %>%
  crossing(determinand.label = baseline_tbl_hayle$determinand.label) %>%
  left_join(hayle_metals_prepped %>% select(Location, Month, determinand.label, result),
            by = c("Location", "Month", "determinand.label")) %>%
  left_join(baseline_tbl_hayle, by = "determinand.label") %>%
  mutate(result = coalesce(result, baseline)) %>%   # fill missing measurements with baseline
  select(Location, Month, determinand.label, result)

# Average if multiple measurements exist for same Location × Month × Metal
hayle_metals_mean_long <- hayle_metals_filled_long %>%
  group_by(Location, Month, determinand.label) %>%
  summarise(metal_mean = mean(result, na.rm = TRUE), .groups = "drop")

# Long to wide (one row per Location × Month, one column per metal)
hayle_metals_wide <- hayle_metals_mean_long %>% pivot_wider(names_from = determinand.label, values_from = metal_mean)

## RED ##
red_grid_LM <- alphadf %>% filter(Catchment == "Red") %>% distinct(Location, Month)

red_metals_filled_long <- red_grid_LM %>%
  crossing(determinand.label = baseline_tbl_red$determinand.label) %>%
  left_join(red_metals_prepped %>% select(Location, Month, determinand.label, result),
            by = c("Location", "Month", "determinand.label")) %>%
  left_join(baseline_tbl_red, by = "determinand.label") %>%
  mutate(result = coalesce(result, baseline)) %>%
  select(Location, Month, determinand.label, result)

red_metals_mean_long <- red_metals_filled_long %>%
  group_by(Location, Month, determinand.label) %>%
  summarise(metal_mean = mean(result, na.rm = TRUE), .groups = "drop")

red_metals_wide <- red_metals_mean_long %>% pivot_wider(names_from = determinand.label, values_from = metal_mean)

## CARNON ##
carnon_grid_LM <- alphadf %>% filter(Catchment == "Carnon") %>% distinct(Location, Month)

carnon_metals_filled_long <- carnon_grid_LM %>%
  crossing(determinand.label = baseline_tbl_carnon$determinand.label) %>%
  left_join(carnon_metals_prepped %>% select(Location, Month, determinand.label, result),
            by = c("Location", "Month", "determinand.label")) %>%
  left_join(baseline_tbl_carnon, by = "determinand.label") %>%
  mutate(result = coalesce(result, baseline)) %>%
  select(Location, Month, determinand.label, result)

carnon_metals_mean_long <- carnon_metals_filled_long %>%
  group_by(Location, Month, determinand.label) %>%
  summarise(metal_mean = mean(result, na.rm = TRUE), .groups = "drop")

carnon_metals_wide <- carnon_metals_mean_long %>% pivot_wider(names_from = determinand.label, values_from = metal_mean)

## COBER ## 
cober_grid_LM <- alphadf %>% filter(Catchment == "Cober") %>% distinct(Location, Month)

cober_metals_filled_long <- cober_grid_LM %>%
  crossing(determinand.label = baseline_tbl_cober$determinand.label) %>%
  left_join(cober_metals_prepped %>% select(Location, Month, determinand.label, result),
            by = c("Location", "Month", "determinand.label")) %>%
  left_join(baseline_tbl_cober, by = "determinand.label") %>%
  mutate(result = coalesce(result, baseline)) %>%
  select(Location, Month, determinand.label, result)

cober_metals_mean_long <- cober_metals_filled_long %>%
  group_by(Location, Month, determinand.label) %>%
  summarise(metal_mean = mean(result, na.rm = TRUE), .groups = "drop")

cober_metals_wide <- cober_metals_mean_long %>% pivot_wider(names_from = determinand.label, values_from = metal_mean)

# rename long determinand labels to short metal codes
rename_metals <- function(df){
  df %>% rename(
    Al = `Aluminium-Al`,
    As = `Arsenic - As`,
    Cd = `Cadmium - Cd`,
    Cu = `Copper - Cu`,
    Fe = `Iron - as Fe`,
    Pb = `Lead - as Pb`,
    Ni = `Nickel - Ni`,
    Zn = `Zinc - as Zn`)
}

hayle_metals_wide <- rename_metals(hayle_metals_wide)
red_metals_wide <- rename_metals(red_metals_wide)
carnon_metals_wide <- rename_metals(carnon_metals_wide)
cober_metals_wide <- rename_metals(cober_metals_wide)

###############################################################################
# JOIN METALS INTO METADATA: Split by catchment, join by Location × Month
###############################################################################

alphadf_Hayle <- alphadf %>%
  filter(Catchment == "Hayle") %>%
  left_join(hayle_metals_wide, by = c("Location","Month"))

alphadf_Red <- alphadf %>%
  filter(Catchment == "Red") %>%
  left_join(red_metals_wide, by = c("Location","Month"))

alphadf_Carnon <- alphadf %>%
  filter(Catchment == "Carnon") %>%
  left_join(carnon_metals_wide, by = c("Location","Month"))

alphadf_Cober <- alphadf %>%
  filter(Catchment == "Cober") %>%
  left_join(cober_metals_wide, by = c("Location","Month"))

#######################################################################
# AGGREGATE REPLICATES: Average alpha metrics to Location × Month level
#######################################################################

alphadf_Hayle2 <- alphadf_Hayle %>% group_by(Location, Month, Altitude, Temp, pH, Al, As, Cd, Cu, Fe, Pb, Ni, Zn, Poll_class) %>%
  summarise(avg_chao = mean(chao1), avg_shan= mean(shannon_entropy), .groups = "drop") 

alphadf_Red2 <- alphadf_Red %>% group_by(Location, Month, Altitude, Temp, pH, Al, As, Cd, Cu, Fe, Pb, Ni, Zn, Poll_class) %>%
  summarise(avg_chao = mean(chao1), avg_shan= mean(shannon_entropy), .groups = "drop") 

alphadf_Carnon2 <- alphadf_Carnon %>% group_by(Location, Month, Altitude, Temp, pH, Al, As, Cd, Cu, Fe, Pb, Ni, Zn, Poll_class) %>%
  summarise(avg_chao = mean(chao1), avg_shan= mean(shannon_entropy), .groups = "drop") 

alphadf_Cober2 <- alphadf_Cober %>% group_by(Location, Month, Altitude, Temp, pH, Al, As, Cd, Cu, Fe, Pb, Ni, Zn, Poll_class) %>%
  summarise(avg_chao = mean(chao1), avg_shan= mean(shannon_entropy), .groups = "drop") 

#################################################################
# METAL CONCENTRATION PROFILEs: Altitude gradients for each metal 
#################################################################

hayle_long <- alphadf_Hayle2 %>%
  pivot_longer(cols = c(Al, As, Cd, Cu, Fe, Pb, Ni, Zn),
               names_to  = "Metal",
               values_to = "conc") %>%
  mutate(Month = factor(Month, levels = c("January", "April", "July", "October")))

red_long <- alphadf_Red2 %>%
  pivot_longer(cols = c(Al, As, Cd, Cu, Fe, Pb, Ni, Zn),
               names_to  = "Metal",
               values_to = "conc") %>%
  mutate(Month = factor(Month, levels = c("January", "April", "July", "October")))

carnon_long <- alphadf_Carnon2 %>%
  pivot_longer(cols = c(Al, As, Cd, Cu, Fe, Pb, Ni, Zn),
               names_to  = "Metal",
               values_to = "conc") %>%
  mutate(Month = factor(Month, levels = c("January", "April", "July", "October")))

cober_long <- alphadf_Cober2 %>%
  pivot_longer(cols = c(Al, As, Cd, Cu, Fe, Pb, Ni, Zn),
               names_to  = "Metal",
               values_to = "conc") %>%
  mutate(Month = factor(Month, levels = c("January", "April", "July", "October")))


hayle_plot <- hayle_long %>%
  arrange(Metal, Month, desc(Altitude))


p_hayle_alt <- ggplot(hayle_plot, aes(Altitude, conc,
                                      colour = Month,
                                      group = interaction(Month, Metal))) +
  geom_path(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.9) +
  scale_x_reverse(name = "Altitude (upstream → downstream)") +
  labs(y = "Metal concentration (µg/L)",
       title = "Metal concentration profiles along the Hayle",
       colour = "Month") +
  facet_wrap(~Metal, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "bottom")

p_hayle_alt

red_plot <- red_long %>%
  arrange(Metal, Month, desc(Altitude))

p_red_alt <- ggplot(red_plot, aes(Altitude, conc,
                                  colour = Month,
                                  group = interaction(Month, Metal))) +
  geom_path(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.9) +
  scale_x_reverse(name = "Altitude (upstream → downstream)") +
  labs(y = "Metal concentration (µg/L)",
       title = "Metal concentration profiles along Red",
       colour = "Month") +
  facet_wrap(~Metal, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "bottom")

p_red_alt

carnon_plot <- carnon_long %>%
  arrange(Metal, Month, desc(Altitude))   

p_carnon_alt <- ggplot(carnon_plot, aes(Altitude, conc,
                                        colour = Month,
                                        group = interaction(Month, Metal))) +
  geom_path(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.9) +
  scale_x_reverse(name = "Altitude (upstream → downstream)") +
  labs(y = "Metal concentration (µg/L)",
       title = "Metal concentration profiles along Carnon",
       colour = "Month") +
  facet_wrap(~Metal, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "bottom")


p_carnon_alt

cober_plot <- cober_long %>%
  arrange(Metal, Month, desc(Altitude))

p_cober_alt <- ggplot(cober_plot, aes(Altitude, conc,
                                      colour = Month,
                                      group = interaction(Month, Metal))) +
  geom_path(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.8, alpha = 0.9) +
  scale_x_reverse(name = "Altitude (upstream → downstream)") +
  labs(y = "Metal concentration (µg/L)",
       title = "Metal concentration profiles along Cober",
       colour = "Month") +
  facet_wrap(~Metal, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"),
        legend.position = "bottom")


p_cober_alt


######################################################################
# PCA ON METALS PER CATCHMENT: reduce correlated metals into gradients
######################################################################
# Reduce 8 correlated metal variables into a few composite axes
# Can use PC scores as “indices” in later biological analyses.

# Vector of the metal columns included in the PCA
metal_cols <- c("Al","As","Cd","Cu","Fe","Pb","Ni","Zn")

# Build the PCA input table
hayle_for_PCA <- hayle_metals_wide[, metal_cols] %>% as.data.frame()

# Create readable row names like "Godolphin_January".
rownames(hayle_for_PCA) <- paste(hayle_metals_wide$Location, hayle_metals_wide$Month, sep = "_")

hayle_metal_pca <- prcomp(na.omit(hayle_for_PCA), center = T, scale. = T)
summary(hayle_metal_pca)

# Scree plot (eigenvalues/variance explained)
fviz_eig(hayle_metal_pca)

# Extract PCA scores
hayle_metal_pca_results <- as.data.frame(hayle_metal_pca$x)

# Recover Location and Month back from the rownames
hayle_pca_split <- str_split_fixed(rownames(hayle_metal_pca_results), "_", 2)

# Adds metadata columns back so can color/shape points in plots.
hayle_metal_pca_results$Location <- hayle_pca_split[,1]
hayle_metal_pca_results$Month <- hayle_pca_split[,2]

hayle_pca_plot <- ggplot(hayle_metal_pca_results, aes(PC1, PC2, colour = Location, shape = Month)) +
  geom_point(size = 2) + theme_bw(base_size = 10) + labs(title = "Hayle PCA scores")

hayle_biplot <- fviz_pca_var(hayle_metal_pca, col.var = "contrib",
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE) +
  ggtitle("Hayle PCA loadings (metals)")

print(hayle_pca_plot)
print(hayle_biplot)


# Extract “results tables” for reporting
# Eigenvalues
hayle_eig.val <- get_eigenvalue(hayle_metal_pca)
hayle_res.var <- get_pca_var(hayle_metal_pca)

# Which metals drive PC1 and PC2 most
hayle_res.var$contrib[, 1] %>% sort(decreasing = T) %>% head(5) 
hayle_res.var$contrib[, 2] %>% sort(decreasing = T) %>% head(5) 

hayle_metal_pca$rotation[,1]
hayle_metal_pca$rotation[,2]


####### RED ########
red_for_PCA <- red_metals_wide[, metal_cols] %>% as.data.frame()
rownames(red_for_PCA) <- paste(red_metals_wide$Location, red_metals_wide$Month, sep = "_")

red_metal_pca <- prcomp(na.omit(red_for_PCA), center = T, scale. = T)
summary(red_metal_pca)

fviz_eig(red_metal_pca)

red_metal_pca_results <- as.data.frame(red_metal_pca$x)

red_pca_split <- str_split_fixed(rownames(red_metal_pca_results), "_", 2)
red_metal_pca_results$Location <- red_pca_split[,1]
red_metal_pca_results$Month <- red_pca_split[,2]

red_pca_plot <- ggplot(red_metal_pca_results, aes(PC1, PC2, colour = Location, shape = Month)) +
  geom_point(size = 2) + theme_bw(base_size = 10) + labs(title = "Red PCA scores")

red_biplot <- fviz_pca_var(red_metal_pca, col.var = "contrib",
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE) +
  ggtitle("Red PCA loadings (metals)")

print(red_pca_plot)
print(red_biplot)

red_eig.val <- get_eigenvalue(red_metal_pca)
red_res.var <- get_pca_var(red_metal_pca)

red_res.var$contrib[, 1] %>% sort(decreasing = T) %>% head(5)  
red_res.var$contrib[, 2] %>% sort(decreasing = T) %>% head(5)  


red_metal_pca$rotation[,1]  
red_metal_pca$rotation[,2]  


##### CARNON #####
carnon_for_PCA <- carnon_metals_wide[, metal_cols] %>% as.data.frame()
rownames(carnon_for_PCA) <- paste(carnon_metals_wide$Location, carnon_metals_wide$Month, sep = "_")

carnon_metal_pca <- prcomp(na.omit(carnon_for_PCA), center = T, scale. = T)
summary(carnon_metal_pca)

fviz_eig(carnon_metal_pca)

carnon_metal_pca_results <- as.data.frame(carnon_metal_pca$x)

carnon_pca_split <- str_split_fixed(rownames(carnon_metal_pca_results), "_", 2)
carnon_metal_pca_results$Location <- carnon_pca_split[,1]
carnon_metal_pca_results$Month <- carnon_pca_split[,2]

carnon_pca_plot <- ggplot(carnon_metal_pca_results, aes(PC1, PC2, colour = Location, shape = Month)) +
  geom_point(size = 2) + theme_bw(base_size = 10) + labs(title = "Carnon PCA scores")

carnon_biplot <- fviz_pca_var(carnon_metal_pca, col.var = "contrib",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                              repel = TRUE) +
  ggtitle("Carnon PCA loadings (metals)")

print(carnon_pca_plot)
print(carnon_biplot)

carnon_eig.val <- get_eigenvalue(carnon_metal_pca)
carnon_res.var <- get_pca_var(carnon_metal_pca)

carnon_res.var$contrib[, 1] %>% sort(decreasing = T) %>% head(5)  
carnon_res.var$contrib[, 2] %>% sort(decreasing = T) %>% head(5) 

carnon_metal_pca$rotation[,1] 
carnon_metal_pca$rotation[,2]

###### COBER ######
cober_for_PCA <- cober_metals_wide[, metal_cols] %>% as.data.frame()
rownames(cober_for_PCA) <- paste(cober_metals_wide$Location, cober_metals_wide$Month, sep = "_")

cober_metal_pca <- prcomp(na.omit(cober_for_PCA), center = T, scale. = T)
summary(cober_metal_pca)

fviz_eig(cober_metal_pca)

cober_metal_pca_results <- as.data.frame(cober_metal_pca$x)

cober_pca_split <- str_split_fixed(rownames(cober_metal_pca_results), "_", 2)
cober_metal_pca_results$Location <- cober_pca_split[,1]
cober_metal_pca_results$Month <- cober_pca_split[,2]

cober_pca_plot <- ggplot(cober_metal_pca_results, aes(PC1, PC2, colour = Location, shape = Month)) +
  geom_point(size = 2) + theme_bw(base_size = 10) + labs(title = "Cober PCA scores")

cober_biplot <- fviz_pca_var(cober_metal_pca, col.var = "contrib",
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE) +
  ggtitle("Cober PCA loadings (metals)")

print(cober_pca_plot)
print(cober_biplot)

cober_eig.val <- get_eigenvalue(cober_metal_pca)
cober_res.var <- get_pca_var(cober_metal_pca)

cober_res.var$contrib[, 1] %>% sort(decreasing = T) %>% head(5)  
cober_res.var$contrib[, 2] %>% sort(decreasing = T) %>% head(5)

cober_metal_pca$rotation[,1]  
cober_metal_pca$rotation[,2]


hayle_pca_scores <- hayle_metal_pca_results %>% dplyr::select(Location, Month, PC1, PC2)
red_pca_scores   <- red_metal_pca_results   %>% dplyr::select(Location, Month, PC1, PC2)
carnon_pca_scores<- carnon_metal_pca_results%>% dplyr::select(Location, Month, PC1, PC2)
cober_pca_scores <- cober_metal_pca_results %>% dplyr::select(Location, Month, PC1, PC2)

#########################################
# JOIN PCA SCORES BACK TO ALPHA SUMMARIES
#########################################

alphadf_Hayle3 <- alphadf_Hayle2 %>%
  left_join(hayle_pca_scores %>% select(Location, Month, PC1, PC2),
            by = c("Location","Month"))

alphadf_Red3 <- alphadf_Red2 %>%
  left_join(red_pca_scores %>% select(Location, Month, PC1, PC2),
            by = c("Location", "Month"))

alphadf_Carnon3 <- alphadf_Carnon2 %>%
  left_join(carnon_pca_scores %>% select(Location, Month, PC1, PC2),
            by = c("Location", "Month"))

alphadf_Cober3 <- alphadf_Cober2 %>%
  left_join(cober_pca_scores %>% select(Location, Month, PC1, PC2),
            by = c("Location", "Month"))

alphadf_Hayle3$Month <- factor(alphadf_Hayle3$Month)
alphadf_Red3$Month <- factor(alphadf_Red3$Month)
alphadf_Carnon3$Month <- factor(alphadf_Carnon3$Month)
alphadf_Cober3$Month <- factor(alphadf_Cober3$Month)

###############################################################################
# MIXED MODELS: Shannon + Chao vs environment + metals PCs + random Location
###############################################################################
# Because the same sites was measured more than once (for different months), the results from one site are naturally related. 
# A mixed model accounts for that by letting each site have its own baseline, instead of treating every sample as totally separate.

m_shan_hayle <- lme(avg_shan ~ pH + Altitude + Temp + Month + PC1 + PC2 + Poll_class, random = ~ 1 | Location,
                    data = alphadf_Hayle3, na.action = na.omit)

check_model(m_shan_hayle)

summary(m_shan_hayle)

anova(m_shan_hayle)

# Estimated marginal means for Month 
emm_month_h <- emmeans(m_shan_hayle, ~ Month)

# Tukey-adjusted pairwise comparisons between months
pairs(emm_month_h, adjust = "tukey")

m_full_ML <- update(m_shan_hayle, method = "ML")

m_noTemp_ML  <- update(m_full_ML, . ~ . - Temp)
m_noMonth_ML <- update(m_full_ML, . ~ . - Month)

anova(m_full_ML, m_noTemp_ML)

anova(m_full_ML, m_noMonth_ML)


## Red Shannon ##

m_shan_red <- lme(avg_shan ~ pH + Altitude + Temp + Month + PC1 + PC2 + Poll_class, random = ~ 1 | Location,
                  data = alphadf_Red3, na.action = na.omit)

check_model(m_shan_red) 

summary(m_shan_red)

anova(m_shan_red)

## Carnon Shannon ##

m_shan_carnon <- lme(avg_shan ~ pH + Altitude + Temp + Month + PC1 + PC2 + Poll_class, random = ~ 1 | Location,
                     data = alphadf_Carnon3, na.action = na.omit)

check_model(m_shan_carnon) 

summary(m_shan_carnon)

anova(m_shan_carnon)

## Cober Shannon ##

m_shan_cober <- lme(avg_shan ~ pH + Altitude + Temp + Month + PC1 + PC2, random = ~ 1 | Location,
                    data = alphadf_Cober3, na.action = na.omit) #Poll_class is just one level of 1

check_model(m_shan_cober) 

summary(m_shan_cober)

anova(m_shan_cober)

## Hayle Chao ##

m_chao_hayle <- lme(avg_chao ~ pH + Altitude + Temp + Month + PC1 + PC2 + Poll_class, random = ~ 1 | Location,
                    data = alphadf_Hayle3, na.action = na.omit)

check_model(m_chao_hayle) 

summary(m_chao_hayle)

anova(m_chao_hayle)

# Estimated marginal means for Month 
emm_month_h_chao <- emmeans(m_chao_hayle, ~ Month)

# Tukey-adjusted pairwise comparisons between months
pairs(emm_month_h_chao, adjust = "tukey")

m_full_ML_chao <- update(m_chao_hayle, method = "ML")

m_noTemp_ML_chao  <- update(m_full_ML_chao, . ~ . - Temp)
m_noMonth_ML_chao <- update(m_full_ML_chao, . ~ . - Month)

anova(m_full_ML_chao, m_noTemp_ML_chao)

anova(m_full_ML_chao, m_noMonth_ML_chao)

## Red Chao ##

m_chao_red <- lme(avg_chao ~ pH + Altitude + Temp + Month + PC1 + PC2 + Poll_class, random = ~ 1 | Location,
                  data = alphadf_Red3, na.action = na.omit)

check_model(m_chao_red) 

summary(m_chao_red)

anova(m_chao_red)

## Carnon Chao ##

m_chao_carnon <- lme(avg_chao ~ pH + Altitude + Temp + Month + PC1 + PC2 + Poll_class, random = ~ 1 | Location,
                     data = alphadf_Carnon3, na.action = na.omit)

check_model(m_chao_carnon) 

summary(m_chao_carnon)

anova(m_chao_carnon)

emm_month_c_chao <- emmeans(m_chao_carnon, ~ Month)

# Tukey-adjusted pairwise comparisons between months
pairs(emm_month_c_chao, adjust = "tukey")

m_full_ML_chao_c <- update(m_chao_carnon, method = "ML")

m_noTemp_ML_chao_c  <- update(m_full_ML_chao_c, . ~ . - Temp)
m_noMonth_ML_chao_c <- update(m_full_ML_chao_c, . ~ . - Month)

anova(m_full_ML_chao_c, m_noTemp_ML_chao_c)

anova(m_full_ML_chao_c, m_noMonth_ML_chao_c)

## Cober Chao ##

m_chao_cober <- lme(avg_chao ~ pH + Altitude + Temp + Month + PC1 + PC2, random = ~ 1 | Location,
                    data = alphadf_Cober3, na.action = na.omit)

check_model(m_chao_cober)

summary(m_chao_cober)

anova(m_chao_cober)

###############################################################################
# CATCHMENT FIGURES: Alpha vs Altitude with Month, pH, Pollution class
###############################################################################

alphadf_plot <- alphadf %>%
  group_by(Location, Month, Altitude, Catchment, pH, Temp) %>%
  summarise(avg_shan   = mean(shannon_entropy, na.rm = TRUE),
            avg_chao   = mean(chao1, na.rm = TRUE),
            Poll_class = names(sort(table(Poll_class), decreasing = TRUE))[1], 
            .groups = "drop")


alphadf_plot_shannon <- alphadf_plot %>%
  mutate(pH_range = factor(ntile(pH, 3),
                           levels = 1:3, 
                           labels = c("Low pH", "Medium pH", "High pH")),
         Month = factor(Month, levels = c("January", "April", "July", "October")),
         Catchment = factor(Catchment, levels = c("Hayle", "Red", "Carnon", "Cober")),
         Poll_class = factor(Poll_class, levels = c("1","2","3"),
                             labels = c("Low", "Moderate", "High")))

p <- ggplot(alphadf_plot_shannon, aes(x = Altitude, y = avg_shan)) +
  geom_point(aes(fill = pH_range, shape = Month, size = Poll_class),
             colour = "black",
             stroke = 0.6,
             alpha = 0.95) +
  facet_wrap(~ Catchment, nrow = 2) +
  scale_fill_manual(values = c("Low pH" = "forestgreen",
                               "Medium pH" = "firebrick",
                               "High pH" = "purple4"),
                    name = "pH range") +
  scale_shape_manual(values = c("January" = 21,
                                "April"   = 22,
                                "July"    = 23,
                                "October" = 24),
                     name = "Month") +
  scale_size_manual(values = c("Low" = 2.6, "Moderate" = 3.6, "High" = 4.8),
                    name = "Pollution class") +
  theme_bw(base_size = 12) +
  labs(x = "Altitude (m)", y = "Shannon diversity") +
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        axis.text  = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.box = "vertical") +
  guides(fill  = guide_legend(override.aes = list(shape = 21, size = 4, colour = "black")),
         size  = guide_legend(override.aes = list(shape = 21, fill = "grey80", colour = "black")),
         shape = guide_legend(override.aes = list(size = 4, fill = "grey80", colour = "black")))

p

ggsave("Shannon_pH_pollclass.png", p, width = 10, height = 6, dpi = 600, device = ragg::agg_png)


alphadf_plot_chao <- alphadf_plot %>%
  mutate(pH_range = factor(ntile(pH, 3),
                           levels = 1:3,
                           labels = c("Low pH", "Medium pH", "High pH")),
         Month = factor(Month, levels = c("January", "April", "July", "October")),
         Catchment = factor(Catchment, levels = c("Hayle", "Red", "Carnon", "Cober")),
         Poll_class = factor(Poll_class, levels = c("1","2","3"),
                             labels = c("Low", "Moderate", "High")))



p2 <- ggplot(alphadf_plot_chao, aes(x = Altitude, y = avg_chao)) +
  geom_point(aes(fill = pH_range, shape = Month, size = Poll_class),
             colour = "black",
             stroke = 0.6,
             alpha = 0.95) +
  facet_wrap(~ Catchment, nrow = 2) +
  scale_fill_manual(values = c("Low pH" = "forestgreen",
                               "Medium pH" = "firebrick",
                               "High pH" = "purple4"),
                    name = "pH range") +
  scale_shape_manual(values = c("January" = 21,
                                "April"   = 22,
                                "July"    = 23,
                                "October" = 24),
                     name = "Month") +
  scale_size_manual(values = c("Low" = 2.6, "Moderate" = 3.6, "High" = 4.8),
                    name = "Pollution class") +
  theme_bw(base_size = 12) +
  labs(x = "Altitude (m)", y = "Chao richness") +
  theme(strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(face = "bold"),
        axis.text  = element_text(size = 10),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.box = "vertical") +
  guides(fill  = guide_legend(override.aes = list(shape = 21, size = 4, colour = "black")),
         size  = guide_legend(override.aes = list(shape = 21, fill = "grey80", colour = "black")),
         shape = guide_legend(override.aes = list(size = 4, fill = "grey80", colour = "black")))

p2

ggsave("Chao_pH_pollclass.png", p2, width = 10, height = 6, dpi = 600, device = ragg::agg_png)

################################################################
# BRAY–CURTIS / NMDS / PERMANOVA: Community composition patterns
################################################################

pc_scores_all <- dplyr::bind_rows(hayle_pca_scores  %>% mutate(Catchment = "Hayle"),
                                  red_pca_scores    %>% mutate(Catchment = "Red"),
                                  carnon_pca_scores %>% mutate(Catchment = "Carnon"),
                                  cober_pca_scores  %>% mutate(Catchment = "Cober")) %>%
  mutate(Month = factor(Month, levels = c("January","April","July","October")))

samples_metadata2 <- samples_metadata %>%
  mutate(Month = factor(Month, levels = c("January","April","July","October")),
         Location = as.character(Location),
         Catchment = as.character(Catchment)) %>%
  left_join(pc_scores_all, by = c("Catchment","Location","Month"))

bray_curtis <- read.table("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/16S_project_data_June25/braycurtis_distance.tsv",
                          header = T, sep = "\t", row.names = 1, check.names = F)


# Turns the square Bray–Curtis matrix into a 'dist' object (the format vegan functions expect).
bray_dist <- as.dist(as.matrix(bray_curtis))
ids_dist <- labels(bray_dist)

# Prepare metadata so sample IDs match the Bray–Curtis labels
metadata <- as.data.frame(samples_metadata2)

# Cleans sample IDs
metadata$SampleID <- trimws(metadata$SampleID)

# Sets the rownames of the metadata to be the SampleID (e.g., "BB-1", "BB-100" etc)
rownames(metadata) <- metadata$SampleID


stopifnot(all(ids_dist %in% rownames(metadata)))  
meta <- metadata[ids_dist, , drop = F]
stopifnot(all(rownames(meta) == ids_dist))


meta2 <- meta %>%
  mutate(Altitude = as.numeric(Altitude),
         pH = as.numeric(pH),
         Temp = as.numeric(Temp),
         Poll_class = factor(Poll_class, levels = c(1,2,3), labels = c("Low","Medium","High"), ordered = TRUE))

meta2 <- meta2 %>%
  mutate(SampleID = as.character(SampleID),
         Location = factor(Location),
         Catchment = factor(Catchment, levels = c("Hayle","Red","Carnon","Cober")),
         Month = factor(Month, levels = c("January","April","July","October")),
         Altitude = as.numeric(Altitude),
         pH = as.numeric(pH),
         Temp = as.numeric(Temp),
         PC1 = as.numeric(PC1),
         PC2 = as.numeric(PC2),
         Poll_class = factor(Poll_class, levels = c("Low","Medium","High"), ordered = TRUE)) %>%
  as.data.frame()

set.seed(1)
nmds <- metaMDS(bray_dist, k = 2, trymax = 200, autotransform = FALSE, trace = FALSE)
nmds$stress

stressplot(nmds)

# Build a plotting dataframe
nmds_sites <- as.data.frame(scores(nmds, display = "sites"))
nmds_sites$SampleID <- rownames(nmds_sites)

plot_df <- meta2 %>%left_join(nmds_sites, by = "SampleID")

# ggplot NMDSs

# by Month
month_cols <- c(
  "January" = "forestgreen",
  "April"   = "firebrick",
  "July"    = "purple4",
  "October" = "darkgoldenrod3")

p_nmds_month <- ggplot(plot_df, aes(NMDS1, NMDS2, colour = Month)) +
  geom_point(alpha = 0.75, size = 2) +
  stat_ellipse(type = "t", linetype = 2, size = 0.8) +
  scale_colour_manual(values = month_cols, name = "Month") +
  theme_bw() + labs(title = "A")

p_nmds_month

# by Catchment
catch_cols <- c(
  "Hayle"  = "forestgreen",
  "Red"    = "firebrick",
  "Carnon" = "purple4",
  "Cober"  = "darkgoldenrod3")

p_nmds_catch <- ggplot(plot_df, aes(NMDS1, NMDS2, colour = Catchment)) +
  geom_point(alpha = 0.75, size = 2) +
  stat_ellipse(type = "t", linetype = 2, size = 0.8) +
  scale_colour_manual(values = catch_cols, name = "Catchment") +
  theme_bw() + labs(title = "B")

p_nmds_catch

# by Pollution class
poll_cols <- c(
  "Low"    = "forestgreen",
  "Medium" = "firebrick",
  "High"   = "purple4")

p_nmds_poll <- ggplot(plot_df, aes(NMDS1, NMDS2, colour = Poll_class)) +
  geom_point(alpha = 0.75, size = 2) +
  stat_ellipse(type = "t", linetype = 2, size = 0.8) +
  scale_colour_manual(values = poll_cols, name = "Poll Class") +
  theme_bw() + labs(title = "C")

p_nmds_poll

#continuous gradients (pH, Temp, Altitude)
cols_3 <- c("Low" = "forestgreen",
            "Medium" = "firebrick",
            "High" = "purple4")

plot_df3 <- plot_df %>%
  mutate(pH_bin = factor(ntile(pH, 3),
                         levels = 1:3,
                         labels = c("Low", "Medium", "High")),
         Temp_bin = factor(ntile(Temp, 3), 
                           levels = 1:3,
                           labels = c("Low", "Medium", "High")), 
         Alt_bin = factor(ntile(Altitude, 3),
                          levels = 1:3,
                          labels = c("Low", "Medium", "High")))

p_nmds_ph <- ggplot(plot_df3, aes(NMDS1, NMDS2, colour = pH_bin)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(type = "t", linetype = 2, size = 0.8) + 
  scale_colour_manual(values = cols_3, name = "pH") +
  theme_bw() +
  labs(title = "D")

p_nmds_temp <- ggplot(plot_df3, aes(NMDS1, NMDS2, colour = Temp_bin)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(type = "t", linetype = 2, size = 0.8) + 
  scale_colour_manual(values = cols_3, name = "Temperature") +
  theme_bw() +
  labs(title = "E")

p_nmds_alt <- ggplot(plot_df3, aes(NMDS1, NMDS2, colour = Alt_bin)) +
  geom_point(alpha = 0.8, size = 2) +
  stat_ellipse(type = "t", linetype = 2, size = 0.8) +
  scale_colour_manual(values = cols_3, name = "Altitude") +
  theme_bw() +
  labs(title = "F")

p_nmds_ph
p_nmds_temp
p_nmds_alt


##### PERMANOVA #####
# between catchment and month model (no strata)
set.seed(1)
adon_between <- adonis2(bray_dist ~ Catchment + Month,
                        data = meta2,
                        permutations = 999,
                        by = "margin")

adon_between

set.seed(1)
adon_between_all <- adonis2(bray_dist ~ Catchment + Month + Altitude + pH + Temp + Poll_class + PC1 + PC2,
                            data = meta2,
                            permutations = 999,
                            by = "margin")

adon_between_all

set.seed(1)
adon_within_site <- adonis2(bray_dist ~ Month + Altitude + pH + Temp + Poll_class + PC1 + PC2,
                            data = meta2,
                            permutations = 999,
                            by = "margin",
                            strata = meta2$Location)

adon_within_site

#########################################################
### DISPERSION CHECK (for categorical variables only) ###
#########################################################

# CATCHMENT #
dis_catch <- betadisper(bray_dist, meta2$Catchment)
anova(dis_catch)

permutest(dis_catch, permutations = 999)

TukeyHSD(dis_catch)

# MONTH #
dis_month <- betadisper(bray_dist, meta2$Month)
anova(dis_month)

permutest(dis_month, permutations = 999)

TukeyHSD(dis_month)

# POLLUTION CLASS #
dis_poll <- betadisper(bray_dist, meta2$Poll_class)
anova(dis_poll)

permutest(dis_poll, permutations = 999)

TukeyHSD(dis_poll)

#####################################
# TAXONOMY: Phylum relative abundance
#####################################

phylum <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/16S_project_data_June25/Phylum.csv", header = TRUE)

# Convert from wide to long format
phylum_long <- pivot_longer(phylum, cols = -index, names_to = "Original_name", values_to = "Count")

# Extract only the phylum names (the part after "p__")
phylum_long <- mutate(phylum_long, Phylum = str_extract(Original_name, "(?<=p__)[^.]*"))

# Replace missing or blank phylum names with "Unclassified"
phylum_long <- mutate(phylum_long, Phylum = ifelse(is.na(Phylum) | Phylum == "", "Unclassified", Phylum))

phylum_clean <- phylum_long %>% rename(SampleID = index)

head(phylum_clean)

# Join the metadata to clean phylum

data <- left_join(phylum_clean, samples_metadata, by = "SampleID")

head(data)

data_Hayle <- data %>% filter(Catchment == "Hayle")
data_Red <- data %>% filter(Catchment == "Red")
data_Carnon <- data %>% filter(Catchment == "Carnon")
data_Cober <- data %>% filter(Catchment == "Cober")

data_Hayle2 <- data_Hayle %>% group_by(Location, Phylum, Month, Altitude) %>% 
  summarise(count_mean = mean(Count))

data_Red2 <- data_Red %>% group_by(Location, Phylum, Month, Altitude) %>% 
  summarise(count_mean = mean(Count))

data_Carnon2 <- data_Carnon %>% group_by(Location, Phylum, Month, Altitude) %>% 
  summarise(count_mean = mean(Count))

data_Cober2 <- data_Cober %>% group_by(Location, Phylum, Month, Altitude) %>% 
  summarise(count_mean = mean(Count))


## Relative abundance per sample
rel_abund_hayle <- data_Hayle2  %>%
  group_by(Location, Month) %>%
  mutate(rel_abund = count_mean / sum(count_mean)) %>%
  ungroup() %>%
  mutate(Location = fct_reorder(Location, Altitude, .desc = T))

## Top 15 phyla in HAYLE 
top15_hayle <- data_Hayle2 %>%
  group_by(Phylum) %>%
  summarise(total = sum(count_mean, na.rm = T), .groups = "drop") %>%
  slice_max(total, n = 15) %>%
  pull(Phylum)

## Recode others as "Other"
rel_abund_hayle_top <- rel_abund_hayle %>%
  mutate(Phylum = ifelse(Phylum %in% top15_hayle, Phylum, "Other"))

rel_abund_hayle_top <- rel_abund_hayle_top %>%
  group_by(Location, Month) %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund, .desc = T)) %>%
  ungroup() 

colors <- c("Acidobacteriota"   = "sienna4",
            "Actinobacteriota"  = "sandybrown",
            "Bacteroidota"      = "firebrick",
            "Bdellovibrionota"  = "indianred1",
            "Campilobacterota"  = "purple4",
            "Chloroflexi"       = "plum",
            "Crenarchaeota"     = "royalblue3",
            "Cyanobacteria"     = "lightgreen",
            "Desulfobacterota"  = "maroon4",
            "Firmicutes"        = "forestgreen",
            "Myxococcota"       = "mediumpurple",
            "Patescibacteria"   = "khaki1",
            "Planctomycetota"   = "darkgoldenrod3",
            "Proteobacteria"    = "lightseagreen",
            "Verrucomicrobiota" = "magenta4",
            "Other"             = "grey")


ggplot(rel_abund_hayle_top, aes(x = Location, y = rel_abund, fill = Phylum)) +
  geom_col() +
  facet_wrap(~ Month, scales = "free_x") +
  labs(x = "Sample (high → low altitude)", y = "Relative abundance", fill = "Phylum") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = colors)


## Relative abundance per sample
rel_abund_red <- data_Red2 %>%
  group_by(Month, Location) %>%
  mutate(rel_abund = count_mean / sum(count_mean)) %>%
  ungroup() %>%
  mutate(Location = fct_reorder(Location, Altitude, .desc = T))

## Top 15 phyla in Red
top15_red <- data_Red2 %>%
  group_by(Phylum) %>%
  summarise(total = sum(count_mean, na.rm = T), .groups = "drop") %>%
  slice_max(total, n = 15) %>%
  pull(Phylum)

## Recode others as "Other"
rel_abund_red_top <- rel_abund_red %>%
  mutate(Phylum = ifelse(Phylum %in% top15_red, Phylum, "Other"))

rel_abund_red_top <- rel_abund_red_top %>%
  group_by(Location, Month) %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund)) %>%
  ungroup() %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund, .desc = T))

## Plot
ggplot(rel_abund_red_top, aes(x = Location, y = rel_abund, fill = Phylum)) +
  geom_col() +
  facet_wrap(~ Month, scales = "free_x") +
  labs(x = "Sample (high → low altitude)", y = "Relative abundance", fill = "Phylum") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = colors)


## Relative abundance per sample
rel_abund_carnon <- data_Carnon2 %>%
  group_by(Month, Location) %>%
  mutate(rel_abund = count_mean / sum(count_mean)) %>%
  ungroup() %>%
  mutate(Location = fct_reorder(Location, Altitude, .desc = T))

## Top 15 phyla in Carnon
top15_carnon <- data_Carnon2 %>%
  group_by(Phylum) %>%
  summarise(total = sum(count_mean, na.rm = T), .groups = "drop") %>%
  slice_max(total, n = 15) %>%
  pull(Phylum)

## Recode others as "Other"
rel_abund_carnon_top <- rel_abund_carnon %>%
  mutate(Phylum = ifelse(Phylum %in% top15_red, Phylum, "Other"))

rel_abund_carnon_top <- rel_abund_carnon_top %>%
  group_by(Location, Month) %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund)) %>%
  ungroup() %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund, .desc = T))

## Plot
ggplot(rel_abund_carnon_top, aes(x = Location, y = rel_abund, fill = Phylum)) +
  geom_col() +
  facet_wrap(~ Month, scales = "free_x") +
  labs(x = "Sample (high → low altitude)", y = "Relative abundance", fill = "Phylum") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = colors)


## Relative abundance per sample
rel_abund_cober <- data_Cober2 %>%
  group_by(Month, Location) %>%
  mutate(rel_abund = count_mean / sum(count_mean)) %>%
  ungroup() %>%
  mutate(Location = fct_reorder(Location, Altitude, .desc = T))

## Top 15 phyla in Cober
top15_cober <- data_Cober2 %>%
  group_by(Phylum) %>%
  summarise(total = sum(count_mean, na.rm = T), .groups = "drop") %>%
  slice_max(total, n = 15) %>%
  pull(Phylum)

## Recode others as "Other"
rel_abund_cober_top <- rel_abund_cober %>%
  mutate(Phylum = ifelse(Phylum %in% top15_red, Phylum, "Other"))

rel_abund_cober_top <- rel_abund_cober_top %>%
  group_by(Location, Month) %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund)) %>%
  ungroup() %>%
  mutate(Phylum = fct_reorder(Phylum, rel_abund, .desc = T))

## Plot
ggplot(rel_abund_cober_top, aes(x = Location, y = rel_abund, fill = Phylum)) +
  geom_col() +
  facet_wrap(~ Month, scales = "free_x") +
  labs(x = "Sample (high → low altitude)", y = "Relative abundance", fill = "Phylum") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = colors)

phylum_rel <- data %>%
  group_by(SampleID) %>%
  mutate(rel_abund = Count / sum(Count, na.rm = T)) %>%
  ungroup()

topP <- 3

topP_phylum_per_location <- phylum_rel %>%
  group_by(Catchment, Location, Phylum) %>%
  summarise(mean_rel_abund = mean(rel_abund, na.rm = T),
            .groups = "drop") %>%
  group_by(Catchment, Location) %>%
  slice_max(mean_rel_abund, n = topP, with_ties = F) %>%
  arrange(Catchment, Location, desc(mean_rel_abund))

topP_phylum_per_location

hayle_plotP <- topP_phylum_per_location %>%
  filter(Catchment == "Hayle") %>%
  mutate(phylum_plot = reorder_within(Phylum, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, phylum_plot, fill = Phylum)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Hayle", x = "Mean relative abundance", y = "Phylum")

hayle_plotP

red_plotP <- topP_phylum_per_location %>%
  filter(Catchment == "Red") %>%
  mutate(phylum_plot = reorder_within(Phylum, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, phylum_plot, fill = Phylum)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Red", x = "Mean relative abundance", y = "Phylum")

red_plotP

carnon_plotP <- topP_phylum_per_location %>%
  filter(Catchment == "Carnon") %>%
  mutate(phylum_plot = reorder_within(Phylum, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, phylum_plot, fill = Phylum)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Carnon", x = "Mean relative abundance", y = "Phylum")

carnon_plotP

cober_plotP <- topP_phylum_per_location %>%
  filter(Catchment == "Cober") %>%
  mutate(phylum_plot = reorder_within(Phylum, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, phylum_plot, fill = Phylum)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Cober", x = "Mean relative abundance", y = "Phylum")

cober_plotP

###################################################
######## Genus Top 10 bar chart by location #######
###################################################

genus <- read.csv("C:/Users/chayl/OneDrive - Swansea University/Documents/Biology Yr3/Dissertation/Data/Project Data/16S_project_data_June25/level-6.csv", header = TRUE)

genus_long <- pivot_longer(genus, cols = -index, names_to = "Original_name", values_to = "Count")

genus_long <- genus_long %>%
  mutate(Genus = str_extract(Original_name, "(?<=g__)[^.]*"),
         Genus = ifelse(is.na(Genus) | Genus == "", "Unclassified", Genus)) %>%
  rename(SampleID = index)

genus_data <- left_join(genus_long, samples_metadata, by = "SampleID")

genus_rel <- genus_data %>%
  group_by(SampleID) %>%
  mutate(rel_abund = Count / sum(Count, na.rm = T)) %>%
  ungroup()

topN <- 10

topN_genus_per_location <- genus_rel %>%
  group_by(Catchment, Location, Genus) %>%
  summarise(mean_rel_abund = mean(rel_abund, na.rm = T),
            .groups = "drop") %>%
  group_by(Catchment, Location) %>%
  slice_max(mean_rel_abund, n = topN, with_ties = F) %>%
  arrange(Catchment, Location, desc(mean_rel_abund))

topN_genus_per_location

topN_genus_per_location <- topN_genus_per_location %>%
  mutate(Genus = factor(Genus, levels = unique(Genus)))

hayle_plot <- topN_genus_per_location %>%
  filter(Catchment == "Hayle") %>%
  mutate(Genus_plot = reorder_within(Genus, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, Genus_plot, fill = Genus)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Hayle", x = "Mean relative abundance", y = "Genus")

hayle_plot

red_plot <- topN_genus_per_location %>%
  filter(Catchment == "Red") %>%
  mutate(Genus_plot = reorder_within(Genus, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, Genus_plot, fill = Genus)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Red", x = "Mean relative abundance", y = "Genus")

red_plot

carnon_plot <- topN_genus_per_location %>%
  filter(Catchment == "Carnon") %>%
  mutate(Genus_plot = reorder_within(Genus, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, Genus_plot, fill = Genus)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Carnon", x = "Mean relative abundance", y = "Genus")

carnon_plot

cober_plot <- topN_genus_per_location %>%
  filter(Catchment == "Cober") %>%
  mutate(Genus_plot = reorder_within(Genus, mean_rel_abund, Location, .desc = T)) %>%
  ggplot(aes(mean_rel_abund, Genus_plot, fill = Genus)) +
  geom_col(show.legend = F) +
  facet_wrap(~Location, scales = "free_y") +
  scale_y_reordered() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  theme_bw() +
  labs(title = "Cober", x = "Mean relative abundance", y = "Genus")

cober_plot
