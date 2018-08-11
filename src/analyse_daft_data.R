library(jsonlite)
library(magrittr)

daft.data <- getwd() %>%
  paste0("/data/daftPropertyData.json") %>%
  jsonlite::fromJSON()

daft.data <- daft.data[complete.cases(daft.data), ]


counties <- daft.data$county %>% unique() %>% paste0(" County")
number <- c()
for (c in counties) {
  n <- daft.data %>% 
    subset(county == c) %>% 
    nrow() %>% 
    as.integer()
  number %<>% append(n)
}

df <- data.frame(
  county = counties,
  number = number,
  stringsAsFactors = FALSE)



# load required libraries
library(rgeos)
library(maptools)

# read the shape file
spdf <- getwd() %>%
  paste0("/data/map/ireland-admin-counties.shp") %>%
  maptools::readShapePoly()

# make it ggplot-friendly
spdf@data$id <- rownames(spdf@data)
spdf.points <- ggplot2::fortify(spdf, region = "id")
counties <- dplyr::inner_join(spdf.points, spdf@data, by="id")

ggplot(counties) + 
  geom_polygon(
    colour="black", 
    aes(x = long, 
        y = lat, 
        group = group, 
        fill = TOTAL2011))

names(counties)