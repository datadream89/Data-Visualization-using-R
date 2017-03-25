#Data Visualization in R 
#ggplot2

#titanic dataset
library(titanic)
data("titanic_train", package="titanic")
titanic<-titanic_train
titanic

#Worldcup dataset
library(faraway)
data("worldcup")
library(tidyverse)
library(ggplot2)

#Generic Code
object<-ggplot(dataframe,aes(x=column_1,y=column_2))
#without object 
ggplot(dataframe,aes(x=column_1,y=column_2))

#Histogram A for the titanic dataset
ggplot(data=titanic,aes(x=Fare))+geom_histogram()

#Alternative ways to create histogram A for the titanic dataset
ggplot()+geom_histogram(data=titanic,aes(x=Fare))
titanic%>%ggplot()+geom_histogram(aes(x=Fare))
titanic%>%ggplot(aes(x=Fare))+geom_histogram()
ggplot(titanic,aes(x=Fare))+geom_histogram(bins=15)

#Scatterplot for the worldcup dataset
ggplot(worldcup,aes(x=Time,y=Passes))+geom_point()

#You may add optional aesthetics such as alpha(transparency),color, fill, group, size,shape and stroke aesthetics
ggplot(worldcup,aes(x=Time,y=Passes,color=Position,size=Shots))+geom_point()

#using multiple geoms- geom_textfor marking noteworthy players
library(dplyr)
noteworthy_players <- worldcup %>% filter(Shots == max(Shots) |
                                            Passes == max(Passes)) %>%
mutate(point_label = paste(Team, Position, sep = ", "))
noteworthy_players
ggplot(worldcup, aes(x = Passes, y = Shots)) +
  geom_point() +
  geom_text(data = noteworthy_players, aes(label = point_label),
            vjust = "inward", hjust = "inward")

#Vertical Reference Lines
ggplot(worldcup, aes(x = Time)) +
  geom_histogram(binwidth = 10) +
  geom_vline(xintercept = 90 * 0:6,
             color = "blue", alpha = 0.5)

#Constant Aesthetics
ggplot(worldcup, aes(x = Time, y = Passes)) +
  geom_point(color = "darkgreen")

#Example plots Nepali dataset

library(faraway)
data(nepali)
nepali <- nepali %>%
  select(id, sex, wt, ht, age) %>%
  mutate(id = factor(id),
         sex = factor(sex, levels = c(1, 2),
                      labels = c("Male", "Female"))) %>%
  distinct(id, .keep_all = TRUE)
head(nepali)

#Histogram of the nepali dataset
ggplot(nepali, aes(x = ht)) +
  geom_histogram(fill = "lightblue", color = "black") +
  ggtitle("Height of children") +
  xlab("Height (cm)") + xlim(c(0, 120))

#Scatter plot of the nepali dataset
ggplot(nepali, aes(x = ht, y = wt, color = sex)) +
  geom_point(size = 1.7) +
  ggtitle("Weight versus Height") +
  xlab("Height (cm)") + ylab("Weight (kg)")

#Boxplots
ggplot(nepali, aes(x = sex, y = ht)) +
  geom_boxplot() +
  xlab("Sex")+ ylab("Height (cm)")

#All pairs of scatterplots for several variables
library(GGally)
ggpairs(nepali %>% select(sex, wt, ht, age))

#Customizing ggplot2 packages- Aim for high data density, clear labels, useful references, highlight interesting aspects 
#of the data, use small multiples, make order meaningful

#The data
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
data(nepali)
data(worldcup)
library(dlnm)
data(chicagoNMMAPS)
chic <- chicagoNMMAPS
chic_july <- chic %>%
  filter(month == 7 & year == 1995)
chic_july

#data density- reduce data to ink ratio, use different themes

ggplot(worldcup, aes(x = Time, y = Shots)) +
  geom_point() +
  theme_classic()

chicago_plot <- ggplot(chic_july, aes(x = date, y = death)) +
  xlab("Day in July 1995") +
  ylab("All-cause deaths") +
  ylim(0, 450)
chicago_plot

chicago_plot +
  geom_area(fill = "black") +
  theme_excel()

chicago_plot +
  geom_line() +
  theme_tufte()

#Meaningful labels

library(forcats)
# Create a messier example version of the data
wc_example_data <- worldcup %>%
  dplyr::rename(Pos = Position) %>%
  mutate(Pos = fct_recode(Pos,
                          "DC" = "Defender",
                          "FW" = "Forward",
                          "GK" = "Goalkeeper",
                          "MF" = "Midfielder"))
wc_example_data
wc_example_data %>%
  ggplot(aes(x = Pos)) +
  geom_bar()

#Cleaner version of the data 
wc_example_data %>%
  mutate(Pos = fct_recode(Pos,
                          "Defender" = "DC",
                          "Forward" = "FW",
                          "Goalkeeper" = "GK",
                          "Midfielder" = "MF")) %>%
  ggplot(aes(x = Pos)) +
  geom_bar(fill = "lightgray") +
  xlab("") +
  ylab("Number of players") +
  coord_flip() +
  theme_tufte()

#References

ggplot(filter(worldcup, Position == "Forward"), aes(x = Passes, y = Shots)) +
  geom_point(size = 1.5) +
  theme_few() +
  geom_smooth()

#Highlighting

noteworthy_players <- worldcup %>%
  filter(Shots == max(Shots) | Passes == max(Passes)) %>%
  mutate(point_label = paste0(Team, Position, sep = ", "))
  ggplot(worldcup, aes(x = Passes, y = Shots)) +
  geom_point(alpha = 0.5) +
  geom_text(data = noteworthy_players, aes(label = point_label),
            vjust = "inward", hjust = "inward", color = "blue") +
  theme_few()
  
#Small Multiples
  
#Without faceting 
  data(worldcup)
  worldcup %>%
    ggplot(aes(x = Time, y = Shots, color = Position)) +
    geom_point()
  
#With faceting- facet grid
  
  worldcup %>%
    filter(Team %in% c("Spain", "Netherlands")) %>%
    ggplot(aes(x = Time, y = Shots)) +
    geom_point() +
    facet_grid(Team ~ Position)
  
#With faceting- facet wrap
  
  worldcup %>%
    ggplot(aes(x = Time, y = Shots)) +
    geom_point(alpha = 0.25) +
    facet_wrap(~ Team, ncol = 6)
  
#Ordering
   #example 1
  worldcup %>%
    select(Position, Time, Shots) %>%
    group_by(Position) %>%
    mutate(ave_shots = mean(Shots),
           most_shots = Shots == max(Shots)) %>%
    ungroup() %>%
    arrange(ave_shots) %>%
    mutate(Position = factor(Position, levels = unique(Position))) %>%
    ggplot(aes(x = Time, y = Shots, color = most_shots)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                       guide = FALSE) +
    facet_grid(. ~ Position) +
    theme_few()
  
  #Example2
   
  worldcup %>%
    dplyr::select(Team, Time) %>%
    dplyr::group_by(Team) %>%
    dplyr::mutate(ave_time = mean(Time),
                  min_time = min(Time),
                  max_time = max(Time)) %>%
    dplyr::arrange(ave_time) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Team = factor(Team, levels = unique(Team))) %>%
    ggplot(aes(x = Time, y = Team)) +
    geom_segment(aes(x = min_time, xend = max_time, yend = Team),
                 alpha = 0.5, color = "gray") +
    geom_point(alpha = 0.5) +
    geom_point(aes(x = ave_time), size = 2, color = "red", alpha = 0.5) +
    theme_minimal() +
    ylab("")
  
#Scales and Color
  
#default scale
  
  ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) +
    geom_point(alpha = 0.5)
  
#custom scale- example 1 
  
  ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(name = "Time played (minutes)",
                       breaks = 90 * c(2, 4, 6),
                       minor_breaks = 90 * c(1, 3, 5))
  
#custom scale - example 2
  
  ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shots)) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(name = "Time played (minutes)",
                       breaks = 90 * c(2, 4, 6),
                       minor_breaks = 90 * c(1, 3, 5)) +
    scale_size_continuous(name = "Shots on goal",
                          breaks = c(0, 10, 20))
  
  
#default date
  
  ggplot(chic_july, aes(x = date, y = death)) +
    geom_line()
  
#Custom date
  ggplot(chic_july, aes(x = date, y = death)) +
    geom_line() +
    scale_x_date(name = "Date in July 1995",
                 date_labels = "%m-%d")
  
#Scale transformation
  ggplot(chic_july, aes(x = date, y = death)) +
    geom_line() +
    scale_y_log10(breaks = c(1:4 * 100))
  
#Brewer Palletes-Sequential or divergent(continuous)and qualitative(categorical data)
  library(RColorBrewer)
  display.brewer.pal(name = "Set1", n = 8)
  display.brewer.pal(name = "PRGn", n = 8)
  display.brewer.pal(name = "PuBuGn", n = 8)
  
#example 
  wc_example <- ggplot(worldcup, aes(x = Time, y = Passes,
                                     color = Position, size = Shots)) +
    geom_point(alpha = 0.5)
  a <- wc_example +
    scale_color_brewer(palette = "Set1") +
    ggtitle("Set1")
  
   b <- wc_example +
    scale_color_brewer(palette = "Dark2") +
    ggtitle("Dark2")
  
  c <- wc_example +
    scale_color_brewer(palette = "Pastel2") +
    ggtitle("Pastel2") +
    theme_dark()
 
 d <- wc_example +
    scale_color_brewer(palette = "Accent") +
    ggtitle("Accent")
 grid.arrange(a, b, c, d, ncol = 2)
  
  
#discrete colors manually
ggplot(worldcup, aes(x = Time, y = Passes,
                     color = Position, size = Shots)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("blue", "red",
                                "darkgreen", "darkgray"))
#Viridis color map
library(viridis)
worldcup %>%
  ggplot(aes(x = Time, y = Shots, color = Passes)) +
  geom_point(size = 0.9) +
  facet_wrap(~ Position) +
  scale_color_viridis()

#viridis discrete
worldcup %>%
  ggplot(aes(x = Time, y = Shots, color = Position)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(discrete = TRUE)

#options four palletes - Magma,Inferno, Plasma, and Viridis

library(gridExtra)
worldcup_ex <- worldcup %>%
  ggplot(aes(x = Time, y = Shots, color = Passes)) +
  geom_point(size = 0.9)
magma_plot <- worldcup_ex +
  scale_color_viridis(option = "A") +
  ggtitle("magma")
inferno_plot <- worldcup_ex +
  scale_color_viridis(option = "B") +
  ggtitle("inferno")
plasma_plot <- worldcup_ex +
  scale_color_viridis(option = "C") +
  ggtitle("plasma")

viridis_plot <- worldcup_ex +
  scale_color_viridis(option = "D") +
  ggtitle("viridis")

grid.arrange(magma_plot, inferno_plot, plasma_plot, viridis_plot, ncol = 2)

#Mapping

library(ggplot2)
us_map <- map_data("state")
head(us_map, 3)

#Map of Carolinas 
library(tidyverse)
us_map %>%
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point()

#Set interior state colors 
us_map %>%
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black")

#Remove background
us_map %>%
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  theme_void()

#Complete US map 

us_map %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  theme_void()

#Chloropleth map

library(dplyr)
library(viridis)
votes.repub %>%
  tbl_df() %>%
  mutate(state = rownames(votes.repub),
         state = tolower(state)) %>%
  right_join(us_map, by = c("state" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = `1976`)) +
  geom_polygon(color = "black") +
  theme_void() +
  scale_fill_viridis(name = "Republican\nvotes (%)")

#Serial mapping dataset 

library(readr)
serial <- read_csv(paste0("https://raw.githubusercontent.com/",
                          "dgrtwo/serial-ggvis/master/input_data/",
                          "serial_podcast_data/serial_map_data.csv"))
head(serial, 3)
serial <- serial %>%
  mutate(long = -76.8854 + 0.00017022 * x,
         lat = 39.23822 + 1.371014e-04 * y,
         tower = Type == "cell-site")
serial %>%
  slice(c(1:3, (n() - 3):(n())))

#Maryland dataset
maryland <- map_data('county', region = 'maryland')
head(maryland)

#Baltimore city data
baltimore <- maryland %>%
  filter(subregion %in% c("baltimore city", "baltimore"))
head(baltimore, 3)
ggplot(baltimore, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  theme_void()

#Adding Serial data 

ggplot(baltimore, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  geom_point(data = serial, aes(group = NULL, color = tower)) +
  theme_void() +
  scale_color_manual(name = "Cell tower", values = c("black", "red"))

#ggmap and googlemaps API 

library(ggmap)
beijing <- get_map("Beijing", zoom = 12)
ggmap(beijing) +
  theme_void() +
  ggtitle("Beijing, China")

#Tourist attractions in the USA

map_1 <- get_map("Estes Park", zoom = 12,
                 source = "google", maptype = "terrain") %>%
  ggmap(extent = "device")
map_2 <- get_map("Estes Park", zoom = 12,
                 source = "stamen", maptype = "watercolor") %>%
  ggmap(extent = "device")
map_3 <- get_map("Estes Park", zoom = 12,
                 source = "google", maptype = "hybrid") %>%
  ggmap(extent = "device")
library(gridExtra)
grid.arrange(map_1, map_2, map_3, nrow = 1)

get_map(c(2.35, 48.86), zoom = 10) %>%
  ggmap(extent = "device")

#Baltimore with serial 

get_map("Baltimore County", zoom = 10,
        source = "stamen", maptype = "toner") %>%
  ggmap() +
  geom_polygon(data = baltimore, aes(x = long, y = lat, group = group),
               color = "navy", fill = "lightblue", alpha = 0.2) +
  geom_point(data = serial, aes(x = long, y = lat, color = tower)) +
  theme_void() +
  scale_color_manual(name = "Cell tower", values = c("black", "red"))

mapdist("Baltimore, MD",
        "1 First St NE, Washington, DC") %>%
  select(from, to, miles)

geocode("Supreme Court of the United States")

#County data 

library(choroplethr)
library(choroplethrMaps)
data(df_pop_county)
df_pop_county %>% slice(1:3)
county_choropleth(df_pop_county)

county_choropleth(df_pop_county, state_zoom = c("colorado", "wyoming"))

county_choropleth(df_pop_county, state_zoom = c("north carolina"),
                  reference_map = TRUE)

#Hurricane Floyd's data 
library(readr)
floyd_events <- read_csv("data/floyd_events.csv")
floyd_events %>% slice(1:3)

#The number of events listed for each US county
floyd_events %>%
  group_by(fips) %>%
  dplyr::summarize(n_events = n()) %>%
  mutate(fips = as.numeric(fips)) %>%
  dplyr::rename(region = fips,
                value = n_events) %>%
  county_choropleth(state_zoom = c("north carolina", "virginia"),
                    reference_map = TRUE)

#To create a map of flood events that includes the track of Hurricane Floyd 
#on the map


floyd_track <- read_csv("data/floyd_track.csv")
floyd_events %>%
  dplyr::group_by(fips) %>%
  dplyr::summarize(flood = sum(grepl("Flood", type))) %>%
  dplyr::mutate(fips = as.numeric(fips)) %>%
  dplyr::rename(region = fips,
                value = flood) %>%
  county_choropleth(state_zoom = c("north carolina", "maryland",
                                   "delaware", "new jersey",
                                   "virginia", "south carolina",
                                   "pennsylvania", "new york",
                                   "connecticut", "massachusetts",
                                   "new hampshire", "vermont",
                                   "maine", "rhode island"),
                    reference_map = TRUE) +
  geom_path(data = floyd_track, aes(x = -longitude, y = latitude,
                                    group = NA),
            color = "red")

#Customization of county plot

floyd_map$add_state_outline <- TRUE
floyd_map$ggplot_scale <- scale_fill_manual(values = c("yellow"),
                                            guide = FALSE)
floyd_xlim <- floyd_map$get_bounding_box()[c(1, 3)]
floyd_ylim <- floyd_map$get_bounding_box()[c(2, 4)]
a <- floyd_map$render() +
  geom_path(data = floyd_track, aes(x = -longitude, y = latitude,
                                    group = NA),
            color = "red", size = 2, alpha = 0.6) +
  xlim(floyd_map$get_bounding_box()[c(1, 3)]) +
  ylim(floyd_map$get_bounding_box()[c(2, 4)])
b <- floyd_map$render_with_reference_map() +
  geom_path(data = floyd_track, aes(x = -longitude, y = latitude,
                                    group = NA),
            color = "red", size = 2, alpha = 0.6) +
  xlim(floyd_xlim) +
  ylim(floyd_ylim)
library(gridExtra)
grid.arrange(a, b, ncol = 2)

#Advanced spatial objects 

library(tigris)
library(sp)
denver_tracts <- tracts(state = "CO", county = 31, cb = TRUE)
plot(denver_tracts)
bbox(denver_tracts)
is.projected(denver_tracts)
proj4string(denver_tracts)
head(denver_tracts@data)

roads <- primary_roads()
plot(denver_tracts, col = "lightblue")
plot(roads, add = TRUE, col = "darkred")

#spatial object to dataframe conversion

denver_tracts_df <- fortify(denver_tracts)
denver_tracts_df %>%
  dplyr::select(1:4) %>% dplyr::slice(1:5)

#use ggplot2 functions

denver_tracts_df %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  theme_void()

#Coordinate Reference System 

proj4string(denver_tracts)

## Generic code
proj4string(my_spatial_object) <- "+proj=longlat +datum=NAD83"


#plotting maps with different projections

usamap <- map_data("state") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
map_1 <- usamap + coord_map() + ggtitle("default")
map_2 <- usamap + coord_map("gilbert") + ggtitle("+ coord_map('gilbert')")
map_3 <- usamap + coord_map("conic", lat0 = 30) +
  ggtitle("+ coord_map('conic', lat0 = 30)")
grid.arrange(map_1, map_2, map_3, ncol = 1)

#GIS style tasks 

load("data/fars_colorado.RData")
driver_data %>%
dplyr::select(1:5) %>% dplyr::slice(1:5)

#Dataframe mapped using ggplot 2

map_data("county", region = "Colorado") %>%
  ggplot(aes(x = long, y = lat, group = subregion)) +
  geom_polygon(color = "gray", fill = NA) +
  theme_void() +
  geom_point(data = driver_data,
             aes(x = longitud, y = latitude, group = NULL),
             alpha = 0.5, size = 0.7)

#county specific chloropleth 

library(stringr)
county_accidents <- driver_data %>%
  dplyr::mutate(county = str_pad(county, width = 3,
                                 side = "left", pad = "0")) %>%
  tidyr::unite(region, state, county, sep = "") %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(value = n()) %>%
  dplyr::mutate(region = as.numeric(region))
county_accidents %>% slice(1:4)
county_choropleth(county_accidents, state_zoom = "colorado")

#Denver accidents 

#we'll need to link each accident(point) to a census tract (polygon), and then we can count up the number of
#points linked to each polygon

denver_fars <- driver_data %>%
  filter(county == 31)

library(sp)
denver_fars_sp <- denver_fars
coordinates(denver_fars_sp) <- c("longitud", "latitude")
proj4string(denver_fars_sp) <- CRS("+init=epsg:4326")

denver_tracts_proj <- spTransform(denver_tracts,
                                  CRS("+init=epsg:26954"))
denver_fars_proj <- spTransform(denver_fars_sp,
                                CRS(proj4string(denver_tracts_proj)))

plot(denver_tracts_proj)
plot(denver_fars_proj, add = TRUE, col = "red", pch = 1)

library(GISTools)
tract_counts <- poly.counts(denver_fars_proj, denver_tracts_proj)
head(tract_counts)
choropleth(denver_tracts, tract_counts)

head(poly.areas(denver_tracts_proj))
choropleth(denver_tracts,
           tract_counts / poly.areas(denver_tracts_proj))

#Raster Data

library(raster)
bbox(denver_fars_sp)
denver_raster <- raster(xmn = -105.09, ymn = 39.60,
                        xmx = -104.71, ymx = 39.86,
                        res = 0.02)
den_acc_raster <- rasterize(geometry(denver_fars_sp),
                            denver_raster,
                            fun = "count")
image(den_acc_raster, col = terrain.colors(5))
plot(denver_tracts)
plot(den_acc_raster, add = TRUE, alpha = 0.5)

#HTML widgets
#plotly 
library(faraway)
data(worldcup)
library(plotly)
plot_ly(worldcup, type = "scatter",
        x = ~ Time, y = ~ Shots, color = ~ Position)

plot_ly(worldcup, type = "scatter",
        x = ~ Time, y = ~ Shots, color = I("blue"))

worldcup %>%
  mutate(Name = rownames(worldcup)) %>%
  plot_ly(x = ~ Time, y = ~ Shots, color = ~ Position) %>%
  add_markers(text = ~ paste("<b>Name:</b> ", Name, "<br />",
                             "<b>Team:</b> ", Team),
              hoverinfo = "text")

#3d scatterplot

worldcup %>%
  plot_ly(x = ~ Time, y = ~ Shots, z = ~ Passes,
          color = ~ Position, size = I(3)) %>%
  add_markers()

worldcup_scatter <- worldcup %>%
  ggplot(aes(x = Time, y = Shots, color = Position)) +
geom_point()
ggplotly(worldcup_scatter)

#Leaflet 

library(tigris)
denver_tracts <- tracts(state = "CO", county = 31, cb = TRUE)
load("data/fars_colorado.RData")
denver_fars <- driver_data %>%
filter(county == 31 & longitud < -104.5)

library(leaflet)
leaflet()

#maptiles
leaflet() %>%
  addTiles()

#pinmarkers
leaflet() %>%
  addTiles() %>%
  addMarkers(data = denver_fars, lng = ~ longitud, lat = ~ latitude)

#circlemarkers
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = denver_fars, radius = 2,
                   lng = ~ longitud, lat = ~ latitude)

#cluster options
leaflet() %>%
  addTiles() %>%
  addMarkers(data = denver_fars,
             lng = ~ longitud, lat = ~ latitude,
             clusterOptions = markerClusterOptions())
#change titles
leaflet() %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addCircleMarkers(data = denver_fars, radius = 2,
                   lng = ~ longitud, lat = ~ latitude)

#popup- show info about a point when a user clicks on it
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = denver_fars, radius = 2,
                   lng = ~ longitud, lat = ~ latitude,
                   popup = ~ paste(age))
#HTML elements to popup 
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = denver_fars, radius = 2,
                   lng = ~ longitud, lat = ~ latitude,
                   popup = ~ paste("<b>Driver age:</b>", age))

#create a column with popup strings before passing the data into leaflet
denver_fars <- denver_fars %>%
  mutate(popup_info = paste("<b>Driver age:</b>", age, "<br />",
                            "<b>Date:</b>", format(date, "%Y-%m-%d"), "<br />",
                            "<b>Time:</b>", format(date, "%H:%M"), "<br />"),
         popup_info = ifelse(!is.na(alc_res),
                             paste(popup_info,
                                   "<b>Blood alcohol</b>", alc_res, "<br />"),
                             popup_info))
denver_fars %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = 2, lng = ~ longitud, lat = ~ latitude,
                   popup = ~ popup_info)

library(viridis)
pal <- colorFactor(viridis(5), denver_fars$drunk_dr)
leaflet() %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addCircleMarkers(data = denver_fars, radius = 2,
                   lng = ~ longitud, lat = ~ latitude,
                   popup = ~ popup_info,
                   color = ~ pal(drunk_dr))%>%
  addLegend(pal = pal, values = denver_fars$drunk_dr)

#census tract boundaries for Denver to a leaflet object
leaflet() %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(data = denver_tracts)

#popups to polygons

leaflet() %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(data = denver_tracts,
              popup = paste0("Tract ID: ", denver_tracts@data$NAME))

#overlay multiple elements ~ polygons and markers

leaflet() %>%
  addProviderTiles("Thunderforest.Transport") %>%
  addPolygons(data = denver_tracts,
              popup = paste0("Tract ID: ", denver_tracts@data$NAME),
              color = "#000000", fillColor = "969696",
              weight = 2) %>%
  addCircleMarkers(data = denver_fars, lat = ~ latitude,
                   lng = ~ longitud, radius = 2,
                   popup = ~ popup_info, opacity = 0.9,
                   color = ~ pal(drunk_dr)) %>%
  addLegend(pal = pal, values = denver_fars$drunk_dr, opacity = 0.9)

#allow users to pick which layers to shpw on the graph 

leaflet() %>%
  addProviderTiles("Thunderforest.Transport") %>%
  addPolygons(data = denver_tracts,
              popup = paste0("Tract ID: ", denver_tracts@data$NAME),
              color = "#000000", fillColor = "969696",
              weight = 2, group = "tracts") %>%
  addCircleMarkers(data = denver_fars, lat = ~ latitude,
                   lng = ~ longitud, radius = 2,
                   popup = ~ popup_info, opacity = 0.9,
                   color = ~ pal(drunk_dr),
                   group = "accidents") %>%
  addLegend(pal = pal, values = denver_fars$drunk_dr, opacity = 0.9) %>%
  addLayersControl(baseGroups = c("base map"),
                   overlayGroups = c("tracts", "accidents"))

#The grid package 
#grey circle grob 

library(grid)
my_circle <- circleGrob(x = 0.5, y = 0.5, r = 0.5,
                        gp = gpar(col = "gray", lty = 3))

grid.draw(my_circle)

my_rect <- rectGrob(x = 0.5, y = 0.5, width = 0.8, height = 0.3)
grid.draw(my_rect)


#write a ggplot object to current graphics device
wc_plot <- ggplot(worldcup, aes(x = Time, y = Passes)) +
  geom_point()
wc_plot
grid.draw(wc_plot)

grid.draw(wc_plot)
grid.draw(my_circle)

#lollipop grob

candy <- circleGrob(r = 0.1, x = 0.5, y = 0.6)
stick <- segmentsGrob(x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 0.5)
lollipop <- gTree(children = gList(candy, stick))
grid.draw(lollipop)

#viewport
grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5,
                      width = 0.5, height = 0.5,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#viewport at the center 

grid.draw(rectGrob())
sample_vp <- viewport(x = 0.5, y = 0.5,
                      width = 0.5, height = 0.5,
                      just = c("center", "center"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#make the viewport smaller 
grid.draw(rectGrob())
sample_vp <- viewport(x = 0.75, y = 0.75,
                      width = 0.25, height = 0.25,
                      just = c("left", "bottom"))
pushViewport(sample_vp)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

#multiple viewports
grid.draw(rectGrob())

sample_vp_1 <- viewport(x = 0.75, y = 0.75,
                        width = 0.25, height = 0.25,
                        just = c("left", "bottom"))
pushViewport(sample_vp_1)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()

sample_vp_2 <- viewport(x = 0, y = 0,
                        width = 0.5, height = 0.5,
                        just = c("left", "bottom"))
pushViewport(sample_vp_2)
grid.draw(roundrectGrob())
grid.draw(lollipop)
popViewport()


#Adding an inset map 

balt_counties <- map_data("county", region = "maryland") %>%
  mutate(our_counties = subregion %in% c("baltimore", "baltimore city"))
balt_map <- get_map("Baltimore County", zoom = 10) %>%
  ggmap(extent = "device") +
  geom_polygon(data = filter(balt_counties, our_counties == TRUE),
               aes(x = long, y = lat, group = group),
               fill = "red", color = "darkred", alpha = 0.2)

maryland_map <- balt_counties %>%
  ggplot(aes(x = long, y = lat, group = group, fill = our_counties)) +
  geom_polygon(color = "black") +
  scale_fill_manual(values = c("white", "darkred"), guide = FALSE) +
  theme_void() +
  coord_map()
grid.draw(ggplotGrob(balt_map))
md_inset <- viewport(x = 0, y = 0,
                     just = c("left", "bottom"),
                     width = 0.35, height = 0.35)
pushViewport(md_inset)
grid.draw(rectGrob(gp = gpar(alpha = 0.5, col = "white")))
grid.draw(rectGrob(gp = gpar(fill = NA, col = "black")))
grid.draw(ggplotGrob(maryland_map))
popViewport()

#Grid Graphics Co-ordinate systems

ex_vp <- viewport(x = 0.5, y = 0.5,
                  just = c("center", "center"),
                  height = 0.8, width = 0.8,
                  xscale = c(0, 100), yscale = c(0, 10))
pushViewport(ex_vp)
grid.draw(rectGrob())
grid.draw(circleGrob(x = unit(20, "native"), y = unit(5, "native"),
                     r = 0.1, gp = gpar(fill = "lightblue")))
grid.draw(circleGrob(x = unit(85, "native"), y = unit(8, "native"),
                     r = 0.1, gp = gpar(fill = "darkred")))
popViewport()

#gridarrange
library(gridExtra)
grid.arrange(lollipop, circleGrob(),
             rectGrob(), lollipop,
             ncol = 2)

time_vs_shots <- ggplot(worldcup, aes(x = Time, y = Shots)) +
  geom_point()
player_positions <- ggplot(worldcup, aes(x = Position)) +
  geom_bar()
grid.arrange(time_vs_shots, player_positions, ncol = 2)

#adding tables to grid graphic objects
library(tidyverse)
worldcup_table <- worldcup %>%
  filter(Team %in% c("Germany", "Spain", "Netherlands", "Uruguay")) %>%
  group_by(Team) %>%
  dplyr::summarize(`Average time` = round(mean(Time), 1),
                   `Average shots` = round(mean(Shots), 1)) %>%
  tableGrob()
grid.draw(ggplotGrob(time_vs_shots))
wc_table_vp <- viewport(x = 0.22, y = 0.85,
                        just = c("left", "top"),
                        height = 0.1, width = 0.2)
pushViewport(wc_table_vp)
grid.draw(worldcup_table)
popViewport()

#Theme

library(ggplot2)
ggplot(data = mtcars, aes(x = disp, y = mpg)) +
  geom_point() +
  theme_classic()

#changing the default theme theme_gray()
new_theme <- theme_minimal()
theme_set(new_theme)

ggplot(data = mtcars, aes(disp, mpg)) +
  geom_point() +
  facet_grid( . ~ gear)

#theme modification 

newtheme <- theme_bw() + theme(plot.title = element_text(color = "darkred"))
library(faraway)
ggplot(data = worldcup, aes(Time, Shots)) +
  geom_point() +
  facet_wrap(facets = ~ Position, ncol = 2) +
  ggtitle("World Cup Data") +
  newtheme

#Building new graphical elements

#Generic Geom code

GeomNEW <- ggproto("GeomNEW", Geom,
                   required_aes = <a character vector of required aesthetics>,
                   default_aes = aes(<default values for certain aesthetics>),
                   draw_key = <a function used to draw the key in the legend>,
                   draw_panel = function(data, panel_scales, coord) {
                     ## Function that returns a grid grob that will
                     ## be plotted (this is where the real work occurs)
                   }
)

#point Geom 

library(grid)
GeomMyPoint <- ggproto("GeomMyPoint", Geom,
                       required_aes = c("x", "y"),
                       default_aes = aes(shape = 1),
                       draw_key = draw_key_point,
                       draw_panel = function(data, panel_scales, coord) {
                         ## Transform the data first
                         coords <- coord$transform(data, panel_scales)
            ## (optional)Let's print out the structure of the 'coords' object
                         str(coords)
                         ## Construct a grid grob
                         pointsGrob(
                           x = coords$x,
                           y = coords$y,
                           pch = coords$shape
                         )
                       })

geom_mypoint <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomMyPoint, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
ggplot(data = worldcup, aes(Time, Shots)) + geom_mypoint()

#Auto transparent Geom

GeomAutoTransparent <- ggproto("GeomAutoTransparent", Geom,
                               required_aes = c("x", "y"),
                               default_aes = aes(shape = 19),
                               draw_key = draw_key_point,
                               draw_panel = function(data, panel_scales, coord) {
                                 ## Transform the data first
                                 coords <- coord$transform(data, panel_scales)
                                 ## Compute the alpha transparency factor based on the
                                 ## number of data points being plotted
                                 n <- nrow(data)
                                 if(n > 100 && n <= 200)
                                   coords$alpha <- 0.3
                                 else if(n > 200)
                                   coords$alpha <- 0.15
                                 else
                                   coords$alpha <- 1
                                 ## Construct a grid grob
                                 grid::pointsGrob(
                                   x = coords$x,
                                   y = coords$y,
                                   pch = coords$shape,
                                   gp = grid::gpar(alpha = coords$alpha)
                                 )
                               })


geom_transparent <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomAutoTransparent, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(data = worldcup, aes(Time, Shots)) + geom_transparent()

#lower number of observations

library(dplyr)
ggplot(data = sample_n(worldcup, 150), aes(Time, Shots)) +
  geom_transparent()

ggplot(data = sample_n(worldcup, 50), aes(Time, Shots)) +
  geom_transparent()

ggplot(data = worldcup, aes(Time, Shots)) +
  geom_transparent() +
  facet_wrap(~ Position, ncol = 2) +
  newtheme

......................................................................
  
