#we will be learning how to generatre a spider plot! 

#1. DATA MANIPULATION FOR SELECTED VARIABLES 
radar_raw <- read.csv("13100771.csv",
                      stringsAsFactors = FALSE, check.names = TRUE)

profile_axes <- c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec",
               "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia") 



geography <- c("Canada", "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec",
               "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia") 

age_bands <- c("51 to 70 years, females", "51 to 70 years, males")

summary(radar_raw)

library(dplyr)


#time to filter the data
annual <- radar_raw %>%
  filter(REF_DATE == 2015, GEO %in% profile_axes,
        Dietary.age.sex.reference.intake.group %in% age_bands,
        Characteristics=="Mean of percentage of total energy intake from protein") %>%
  transmute(year = as.integer(REF_DATE),
    age_group = as.factor(Dietary.age.sex.reference.intake.group),
    prov_territ = as.factor(GEO),
    calculation = as.factor(Characteristics),
    percent=VALUE
  )

#now we will make the angles. There are 10 provinces/territories 
angles <- data.frame(band = factor(profile_axes, levels = profile_axes),
                     angle = pi / 2 - 2 * pi * (0:9) / 10) #there are 10 positions, evenly around circle.


# Close each line by repeating its first row at the end.
close_path <- function(data) rbind(data, data[1, , drop = FALSE])
#this is a function where we have a dataframe. We take its first row, and add it to the bottom of the same dataframe 

# to create the position points on the circle
# bind the dataframe by rows. Creates a sequence of ring radii, Depending on the radar limit we assign, 10 sequences within it 
# ex 10 20 30 40 50 60 70 80 90 100 
#Applies this function to each radius value 
#transform is adding three columns to each radius (x, y, and radius)

radar_limit <- 50 

ring_points <- bind_rows(lapply(seq(10, radar_limit, 10), function(radius) {
  close_path(transform(angles, radius = radius,
                        x = radius * cos(angle), y = radius * sin(angle)))
}))

spokes <- transform(angles, x = radar_limit * cos(angle), y = radar_limit * sin(angle))

axis_labels <- transform(angles, x = (radar_limit + 10) * cos(angle),
                         y = (radar_limit + 10) * sin(angle))

radar_points <- annual %>%  left_join(angles, by = c("prov_territ" = "band")) %>%
  mutate(x = percent * cos(angle), y = percent * sin(angle))
#combining the angles dataframe with the annual dataframe 

radar_lines <- radar_points %>% group_by(age_group) %>% group_modify(~ close_path(.x)) %>% ungroup()
#group by age/sex group. Then run the close path function on each group separately. Then remove groupings 