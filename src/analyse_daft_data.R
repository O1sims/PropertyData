library(rgeos)
library(ggplot2)
library(jsonlite)
library(magrittr)
library(maptools)


numberOfPropertiesHeatmap <- function(property.data) {
  number <- c()
  counties <- property.data$county %>% 
    unique()
  for (c in counties) {
    numberOfProperties <- property.data %>% 
      subset(county == c) %>% 
      nrow() %>% 
      as.integer()
    number %<>% append(numberOfProperties)
  }
  
  df <- data.frame(
    county = counties %>% 
      paste0(" County"),
    number = number,
    stringsAsFactors = FALSE)
  
  extend.df <- data.frame(
    county = c(
      "Dublin City",
      "Fingal",
      "Dún Laoghaire-Rathdown",
      "South Dublin", 
      "Limerick City", 
      "Cork City", 
      "Galway City", 
      "North Tipperary",
      "South Tipperary",
      "Waterford City"),
    number = c(
      df$number[match("Dublin County", df$county)],
      df$number[match("Dublin County", df$county)],
      df$number[match("Dublin County", df$county)],
      df$number[match("Dublin County", df$county)],
      df$number[match("Limerick County", df$county)],
      df$number[match("Cork County", df$county)],
      df$number[match("Galway County", df$county)],
      df$number[match("Tipperary County", df$county)],
      df$number[match("Tipperary County", df$county)],
      df$number[match("Waterford County", df$county)]),
    stringsAsFactors = FALSE)
  
  df %<>% rbind(extend.df)
  
  spdf <- getwd() %>%
    paste0("/data/map/ireland-admin-counties.shp") %>%
    maptools::readShapePoly()
  
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- ggplot2::fortify(spdf, region = "id")
  counties <- dplyr::inner_join(
    spdf.points, 
    spdf@data, 
    by = "id")
  
  counties$COUNTYNAME %<>% as.vector()
  counties$COUNTYNAME[counties$COUNTYNAME=="D\xfan Laoghaire-Rathdown"] <- "Dún Laoghaire-Rathdown"
  
  heatmap.data <- dplyr::left_join(
    counties, 
    df, 
    by = c("COUNTYNAME" = "county"))
  
  propertyHeatmap <- ggplot(
    data = heatmap.data) + 
    geom_polygon(
      colour = "white", 
      aes(
        x = long, 
        y = lat, 
        group = group, 
        fill = number)) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none") + 
    scale_fill_gradient(
      low = "#158cba", 
      high = "#e54f53")
  
  getwd() %>%
    paste0("/images/irelandHeatmap.png") %>%
    ggsave(width = 13.9, height = 15.4, units = "cm")
  
  return(propertyHeatmap)
}


property.data <- getwd() %>%
  paste0("/data/daftPropertyData.json") %>%
  jsonlite::fromJSON()

property.data <- property.data[complete.cases(property.data), ]

numberOfPropertiesHeatmap(
  property.data = property.data)

