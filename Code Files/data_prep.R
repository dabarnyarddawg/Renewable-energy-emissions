require(dplyer)
require(lubridate)
require(sf)

############
# DATA PREP
############

# TEMPERATURE DATA
# load temp data
temp <- read.csv2(Sys.glob('Data/NOAA/*.csv'), sep = ',', 
                  colClasses = c(rep('NULL',5),'character',rep('NULL',4),'character',rep('NULL',79)), na.strings = '')

# rename columns
colnames(temp) <- c('Datetime_UTC', 'Temp_F')

# convert numeric & date data
temp$Temp_F <- as.numeric(temp$Temp_F)
temp$Datetime_UTC <- as.POSIXct(temp$Datetime_UTC, tz = 'UTC')

# offset eastern standard time to UTC
temp$Datetime_UTC <- temp$Datetime_UTC + hours(5)

# calculate hourly average temperatures
temp <- temp %>% group_by(date(Datetime_UTC), hour(Datetime_UTC)) %>% 
    summarise(Temp_F = mean(Temp_F, na.rm = TRUE))

# re-build datetime column
temp$Datetime_UTC <- paste(temp$`date(Datetime_UTC)`,' ',temp$`hour(Datetime_UTC)`,':00:00',sep = '')
temp$Datetime_UTC <- as.POSIXct(temp$Datetime_UTC, tz = 'UTC')
temp <- temp[,c(4,3)]


# EMISSIONS DATA
# load facilities data for each state into lists
facilities_colN <- c('State','Facilty_Name','Facility_ID','Unit_ID','Latitude','Longitude','Unit_Type',
                     'Primary_Fuel','Operating_Status')
facilities_colC <- c('factor','character','character','character','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','NULL','character',
                     'character','NULL','NULL','NULL','NULL','NULL','NULL','factor','factor','NULL','NULL','NULL','NULL','NULL','NULL',
                     'factor','NULL')
facilities_list <- lapply(Sys.glob('Data/EPA/*/*facility_*'), read.csv2, sep = ',', colClasses = facilities_colC, row.names = NULL, na.strings = '')

# merge each facilty list item into one data frame
facilities <- Reduce(rbind, facilities_list)

# rename columns and drop extra column at right
colnames(facilities) <- facilities_colN
facilities <- facilities[,-ncol(facilities)]
rm(facilities_colC, facilities_colN, facilities_list)

# convert numeric columns to numeric data type
facilities$Latitude <- as.numeric(facilities$Latitude)
facilities$Longitude <- as.numeric(facilities$Longitude)

# load PJM shapefile for subsetting of facilities
pjm <- read_sf('Data/PJM/PJM_Zones_2016_shapefile')

# change map projection long-lat with WGS-84 datum
pjm <- st_transform(pjm, crs = '+proj=longlat +datum=WGS84')

# create facilities spatial data frame for the partial states
facilities_sf <- facilities[facilities$State %in% c('IL','MI','IN','KY','TN','NC'),]
facilities_sf$Long <- facilities_sf$Longitude
facilities_sf$Lat <- facilities_sf$Latitude
facilities_sf <- st_as_sf(facilities_sf, coords = c('Long', 'Lat'), crs = '+proj=longlat +datum=WGS84')

# subset facilities to pjm
facilities_sf <- st_intersection(facilities_sf, pjm)
facilities_sf <- as.data.frame(facilities_sf)[,1:9]

# re-combine with facilities from complete states
facilities <- rbind(facilities_sf, facilities[!(facilities$State %in% c('IL','MI','IN','KY','TN','NC')),])
rm(pjm, facilities_sf)

# save facilities table for import into QGIS
write.table(as.data.frame(facilities), file = 'Data/Facilities/pjm_facilities.csv', quote = FALSE, sep = ';', row.names = FALSE)

# load emissions data
emissions_colN <- c('State','Facilty_Name','Facility_ID','Unit_ID','Date','Hour','Operating_Time','Gross_Load_MW','Steam_Load_1klbshr',
                    'SO2_lbs','NOx_lbsMMBtu','NOx_lbs','CO2_tons','HI_MMBtu')
emissions_colC <- c('factor','character','character','character','NULL','NULL','Date','character','NULL','character','character','character',
                    'character','character','character','character','character')
emissions_list <- lapply(Sys.glob('Data/EPA/*/*emission_*'), read.csv2, sep = ',', row.names = NULL, colClasses = emissions_colC, na.strings = '')

# merge each emissions list item into one data frame
emissions <- Reduce(rbind, emissions_list)

# rename columns and drop extra column at right
colnames(emissions) <- emissions_colN
emissions <- emissions[,-ncol(emissions)]
rm(emissions_colC, emissions_colN, emissions_list)

# convert numeric columns to numeric data type
emissions[,6:14] <- sapply(emissions[,6:14], FUN = as.numeric)

# merge emissions and facility dataframes
emissions <- merge(emissions[,c(3,4,5,6,10,12,13)], facilities[,c(3,4,7,8,9)], by = c('Facility_ID','Unit_ID'))
rm(facilities)

# create new column of fuel type to separate out types of natural gas
emissions$Fuel_Type <- as.character(emissions$Primary_Fuel)

# subset to only plants in operation for the full year
emissions <- emissions[emissions$Operating_Status=='Operating',]

# reduce fuel types
emissions[emissions$Fuel_Type=='Coal, Pipeline Natural Gas','Fuel_Type'] = 'Coal'
emissions[emissions$Fuel_Type=='Pipeline Natural Gas','Fuel_Type'] = 'Natural Gas'
emissions[emissions$Unit_Type=='Combined cycle','Fuel_Type'] = 'Natural Gas - Combined Cycle'
emissions[emissions$Unit_Type=='Combustion turbine','Fuel_Type'] = 'Natural Gas - Combustion Turbine'
emissions[emissions$Fuel_Type=='Natural Gas','Fuel_Type'] = 'Natural Gas - Boiler'
emissions[emissions$Fuel_Type=='Other Gas','Fuel_Type'] = 'Natural Gas - Boiler'

# convert fuel type to factor column
emissions$Fuel_Type <- as.factor(emissions$Fuel_Type)

# drop unit type, primary fuel, operating status
emissions <- emissions[,-c(8,9,10)]

# sum by fuel type, by hour and day
emissions <- emissions %>% group_by(Date,Hour,Fuel_Type) %>% 
    summarise(SO2_lbs = sum(SO2_lbs, na.rm=TRUE), NOx_lbs = sum(NOx_lbs, na.rm=TRUE), CO2_tons = sum(CO2_tons, na.rm=TRUE))

# create POSIXct datetime column in emissions emissions and convert to UTC
emissions$Datetime_UTC <- paste(emissions$Date,' ',emissions$Hour,':00:00',sep = '')
emissions$Datetime_UTC <- as.POSIXct(emissions$Datetime_UTC, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
emissions$Datetime_UTC <- emissions$Datetime_UTC + hours(5)

# drop and re-arrange columns
emissions <- emissions[,c(7,3,4,5,6)]

# add columns for SO2 and NOx in tons
emissions$SO2_tons <- emissions$SO2_lbs / 2000
emissions$NOx_tons <- emissions$NOx_lbs / 2000
emissions <- emissions[,-c(3,4)]



# GENERATION DATA
# load generation data
gen_ColC <- c('character','NULL','factor','character','character','logical')
gen <- read.csv2(Sys.glob('Data/PJM/gen*'), sep = ',', colClasses = gen_ColC)
rm(gen_ColC)

# rename columns
colnames(gen) <- c('Datetime_UTC','Fuel_Type','Gen_MW','Fuel_Pct','Is_Renew')

# convert numeric data
gen$Gen_MW <- as.numeric(gen$Gen_MW)
gen$Fuel_Pct <- as.numeric(gen$Fuel_Pct)
gen$Datetime_UTC <- as.POSIXct(gen$Datetime_UTC, format = '%m/%d/%Y %I:%M:%S %p', tz = 'UTC')

# keep only solar and wind generation
gen <- gen[gen$Fuel_Type=='Wind'| gen$Fuel_Type=='Solar',]
gen <- gen[,-5]

# sum by hour
gen <- gen %>% group_by(Datetime_UTC) %>% summarise(Gen_MW = sum(Gen_MW, na.rm=TRUE))

# add delta MW column for change in generation from prior hour
gen$dGen_MW <- gen$Gen_MW - lag(gen$Gen_MW, n = 1)

# drop first row 
gen <- gen[-1,]



# ELECTRICITY LOAD DATA
# load electricity system load data
load <- read.csv2(Sys.glob('Data/PJM/*load*'), sep = ',', colClasses = 'character')

# drop unneeded columns
load <- load[,c(1,6)]

# rename columns
colnames(load) <- c('Datetime_UTC', 'Load_MW')

# convert numeric data
load$Load_MW <- as.numeric(load$Load_MW)
load$Datetime_UTC <- as.POSIXct(load$Datetime_UTC, format = '%m/%d/%Y %I:%M:%S %p', tz = 'UTC')

# sum by hour
load <- load %>% group_by(Datetime_UTC) %>% summarise(Load_MW = sum(Load_MW, na.rm = TRUE))

# merge tables
df <- merge(emissions, gen, by = 'Datetime_UTC')
df <- merge(df, load, by = 'Datetime_UTC')
df <- merge(df, temp, by = 'Datetime_UTC')
rm(emissions, gen, load, temp)

# drop rows with NAs
df <- df[complete.cases(df),]

# export combined data set
write.table(df, file = 'Data/cleaned_dataset.csv', quote = FALSE, sep = ',', row.names = FALSE)
