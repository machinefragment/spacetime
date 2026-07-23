# Lab 2.2 Data Visualization

library(animation)
library(gstat)
library(dplyr)
library(maps)
library(STRbook)
library(ggplot2)

#consistent results, set seed 1
set.seed(1)

#we now load the data
data("NOAA_df_1990", package = "STRbook")
Tmax <- filter(NOAA_df_1990, # subset the data
               proc == "Tmax" & # only max temperature
                 month %in% 5:9 & # May to September
                 year == 1993) # year of 1993

#the first record has a Julian date of 728050 corresponding to 01 May 1993
#to ease operations, we create a new variable t that is equal to 1 when julian
#is equal to 728050 and increases by 1 for each day in the record

Tmax$t <- Tmax$julian - 728049

#Spatial Plots
#visualization for data that is fixed in space, lets start with maps.
#since we have a lot of time points, lets just pick three

Tmax_1 <- subset(Tmax, t %in% c(1, 15, 30)) # extract data
NOAA_plot <- ggplot(Tmax_1) + # plot points
  geom_point(aes(x = lon,y = lat, # lon and lat
                 colour = z), # attribute color
             size = 2) + # make all points larger
  col_scale(name = "degF") + # attach color scale
  xlab("Longitude (deg)") + # x-axis label
  ylab("Latitude (deg)") + # y-axis label
  geom_path(data = map_data("state"), # add US states map
            aes(x = long, y = lat, group = group)) +
  facet_grid(~date) + # facet by time
  coord_fixed(xlim = c(-105, -75),
              ylim = c(25, 50)) + # zoom in
  theme_bw() # B&W theme

#in this example, point-referenced data was used.so, a regular lattice.
#what if polygon? then an irregular lattice. consider BEA income data set
data("BEA", package = "STRbook")
data("MOcounties", package = "STRbook")
County1 <- filter(MOcounties, NAME10 == "Clark, MO")
plot(County1$long, County1$lat)
MOcounties <- left_join(MOcounties, BEA, by = "NAME10")
#in the above, we just did the basic loading a shapefile with relevant data operation
g1 <- ggplot(MOcounties) +
  geom_polygon(aes(x = long, y = lat, # county boundary
                   group = NAME10, # county group
                   fill = log(X1970))) + # log of income
  geom_path(aes(x = long, y = lat, # county boundary
                group = NAME10)) + # county group
  fill_scale(limits = c(7.5,10.2),
             name = "log($)") +
  coord_fixed() + ggtitle("1970") + # annotations
  xlab("x (m)") + ylab("y (m)") + theme_bw()

#next, time series plots 
UIDs <- unique(Tmax$id) # extract IDs
UIDs_sub <- sample(UIDs, 10) # sample 10 IDs
Tmax_sub <- filter(Tmax, id %in% UIDs_sub) # subset data
TmaxTS <- ggplot(Tmax_sub) +
  geom_line(aes(x = t, y = z)) + # line plot of z against t
  facet_wrap(~id, ncol = 5) + # facet by station
  xlab("Day number (days)") + # x label
  ylab("Tmax (degF)") + # y label
  theme_bw() + # BW theme
  theme(panel.spacing = unit(1, "lines")) # facet spacing

#hovmoller plots. 2d space-time viz where space is one D and time is D2
#data on space time grid, generate regular of 25 sp and 100 temp
lim_lat <- range(Tmax$lat) # latitude range
lim_t <- range(Tmax$t) # time range
lat_axis <- seq(lim_lat[1], # latitude axis
                lim_lat[2],
                length=25)
t_axis <- seq(lim_t[1], # time axis
              lim_t[2],
              length=100)
lat_t_grid <- expand.grid(lat = lat_axis,
                          t = t_axis)
#next, associate each station's latitudinal coordinate with the closest on grid
Tmax_grid <- Tmax
dists <- abs(outer(Tmax$lat, lat_axis, "-"))
Tmax_grid$lat <- lat_axis[apply(dists, 1, which.min)]

#group by lat and time, then average all station values in latitude time bands
Tmax_lat_Hov <- group_by(Tmax_grid, lat, t) %>%
  summarise(z = mean(z))

Hovmoller_lat <- ggplot(Tmax_lat_Hov) + # take data
  geom_tile(aes(x = lat, y = t, fill = z)) + # plot
  fill_scale(name = "degF") + # add color scale
  scale_y_reverse() + # rev y scale
  ylab("Day number (days)") + # add y label
  xlab("Latitude (degrees)") + # add x label
  theme_bw() 

#now some animations
Tmax_t <- function(tau) {
  Tmax_sub <- filter(Tmax, t == tau) # subset data
  ggplot(Tmax_sub) +
    geom_point(aes(x = lon,y = lat, colour = z), # plot
               size = 4) + # pt. size
    col_scale(name = "z", limits = c(40, 110)) +
    theme_bw() # B&W theme
}
#max temp as a function of time
#post and save
outdir <- file.path(getwd(), "output")

gen_anim <- function() {
  for(t in lim_t[1]:lim_t[2]){ # for each time point
    plot(Tmax_t(t)) # plot data at this time point
  }
}
ani.options(interval = 0.2) # 0.2s interval between frames
saveHTML(gen_anim(), # run the main function
         autoplay = FALSE, # do not play on load
         loop = FALSE, # do not loop
         verbose = FALSE, # no verbose
         outdir = "outdir", # save to outdir
         single.opts = "'controls': ['first', 'previous',
'play', 'next', 'last',
'loop', 'speed'],
'delayMin': 0",
         htmlfile = "NOAA_anim.html") # save filename
#trouble saving right - figure out later