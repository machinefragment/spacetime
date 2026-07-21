# LAB 2.1 - DATA WRANGLING
library("dplyr")
library("STRbook")
library("tidyr")
library("spacetime")
library("sp")
# Working with spatio-temporal data in long format
# Loading locations, time stamps, max temps

locs <- read.table(system.file("extdata", "Stationinfo.dat", package ="STRbook"),
                   col.names = c("id", "lat", "lon"))
Times <- read.table(system.file("extdata", "Times_1990.dat", package ="STRbook"),
                    col.names = c("julian", "year", "month", "day"))
Tmax <- read.table(system.file("extdata", "Tmax_1990.dat", package ="STRbook"))

# now, we want to make Tmax sensible
names(Tmax) <- locs$id

# each row is associated with a time point in Tmax, attach using cbind
Tmax <- cbind(Times, Tmax)
head(names(Tmax),10)

# now tmax contains the time info in the first four columns and temp data in the other columns
# must put tmax into long format, so we need to identify a key-value pair.
# in our case, keys are station IDs and values are max temp, in a field named z

Tmax_long <- gather(Tmax, id, z, -julian, -year, -month, -day) # exclude certain columns values
head(Tmax_long)

# note how gather helped us achieve the goal - we now have a single row per measurement and multiple rows may be associated w same time point
# since station id is currently a character but is actually an integer, fix that
Tmax_long$id <- as.integer(Tmax_long$id)

# make sure to filter out missing values, the missing values are -9999 in this case, but 
# we set the filter as an inequality to make sure we catch everything
Tmax_long <- filter(Tmax_long, !(z <= -9998))
#now we associate each z with a process, assuming we want to include other variables
Tmax_long <- mutate(Tmax_long, proc = "Tmax")
# now we do the same to the others, but to save time we just load them from the prepackage
data(Tmin_long, package ="STRbook")
data(TDP_long, package = "STRbook")
data(Precip_long, package = "STRbook")

# now we construct our final dataframe
NOAA_df_1990 <- rbind(Tmax_long, Tmin_long, TDP_long, Precip_long)

# data in long format is easy to group and summarize
# lets find mean value for each variable in a year
summ <- group_by(NOAA_df_1990, year, proc) %>%
  summarise(mean_proc = mean(z))

# what about finding out the number of days when it did not rain at each station in June? 
# first filter then summarize

NOAA_precip <- filter(NOAA_df_1990, proc == "Precip" & month == 6)
summ <- group_by(NOAA_precip, year, id) %>%
  summarise(days_no_precip = sum (z == 0))
head(summ)
# the median number of dats with no recorded precipitation... 
median(summ$days_no_precip)

# whole lot of things you can do otherwise, but lets attach some spatial information now
NOAA_df_1990 <- left_join(NOAA_df_1990, locs, by = "id")
# what if we want it wide instead of long again? the opposite function of gather is spread

# Constructing an STIDF object <- page 57
# step 1 is to take maxtemp and define a formal time stamp
NOAA_df_1990$date <- with(NOAA_df_1990,
                          paste(year, month, day, sep = "-"))
# the field date now needs to be converted into a Date object
NOAA_df_1990$date <- as.Date(NOAA_df_1990$date)
# now we constructed STIDF for max temp
Tmax_long2 <- filter(NOAA_df_1990, proc == "Tmax")
STObj <- stConstruct( x = Tmax_long2, #dataset
                      space = c("lon", "lat"), #spatial fields
                      time = "date") # time field
# the function STIDF is slightly different from STconstruct, so you can do it differently
# the difference being we need shit from the sp package
spat_part <- SpatialPoints( coords = Tmax_long2[, c("lon", "lat")])
temp_part <- Tmax_long2$date
STObj2 <- STIDF (sp = spat_part,
                 time = temp_part,
                 data = select(Tmax_long2, -date, -lon, -lat))

# ok now construct an STFDF object
# when spatial points are fixed in time, we only need to provide as many coords as time, in this case station location
spat_part <- SpatialPoints(coords = locs[, c("lon", "lat")])
temp_part <- with(Times,
                  paste(year, month, day, sep = "-"))
temp_part <- as.Date(temp_part)

# now we need to contain all the missing favlues, and get data into long format
Tmax_long3 <- gather(Tmax, id, z, -julian, -year, -month, -day)

Tmax_long3$id <- as.integer(Tmax_long3$id)
Tmax_long3 <- arrange(Tmax_long3, julian, id) # so sorting happens in the right order

# confirm ordering is correct for construction of object
all(unique(Tmax_long3$id) == locs$id)

STObj3 <- STFDF(sp = spat_part,
                time = temp_part,
                data = Tmax_long3)
# and lab one is done! yay
# you can also add a CRS if you want