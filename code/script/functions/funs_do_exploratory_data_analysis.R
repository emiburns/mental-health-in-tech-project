#load libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(countrycode)
library(geojsonio)

#plot two 
PlotMIInterference <- function(data, xcol){
    p1 <- ggplot(data, aes(as.factor(xcol))) +
    geom_bar(fill = "mediumpurple1") +
    theme_minimal() +
    xlab(" ")
}

#plot three 
PlotMIComfort <- function(data, xcol){
    p1 <- ggplot(data, aes(as.factor(xcol))) +
        geom_bar(fill = "palegreen3") +
        theme_minimal() +
        xlab(" ") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

#demo count table on comfort talking with employees re mental health
DemoTable <- function(column){
    demo_table <- clean_df %>% select_(column, "gender", "mh_dx") 
    demo_table <- dummy_cols(demo_table, select_columns = c("gender", "mh_dx"))
    return(demo_table <- demo_table %>% select(-c(gender, mh_dx)) %>%
               group_by_(column) %>% 
               summarise(sum(gender_Female), sum(gender_Male), sum(gender_Other), sum(mh_dx_Yes), sum(mh_dx_No)))
}

#creating map 
PlotWorldMap <- function(df){
    map_img <- map_data("world")
    map_img$region <- countrycode(map_img$region, origin = 'country.name', destination = 'iso3c')
    map_img = map_img %>% rename("code" = "region")
    full_map <- left_join(df, map_img, by = "code")
    
    return(ggplot(full_map, aes(long, lat, group = group))+
        geom_polygon(aes(fill = dx_ratio), color = "white")+
        scale_fill_viridis_c(option = "C"))
}