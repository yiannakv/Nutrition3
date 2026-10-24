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

radar_limit <- 30 

ring_points <- bind_rows(lapply(seq(5, radar_limit, 5), function(radius) {
  close_path(transform(angles, radius = radius,
                        x = radius * cos(angle), y = radius * sin(angle)))
}))

spokes <- transform(angles, x = radar_limit * cos(angle), y = radar_limit * sin(angle))

axis_labels <- transform(angles, x = (radar_limit + 5) * cos(angle),
                         y = (radar_limit + 5) * sin(angle))

radar_points <- annual %>%  left_join(angles, by = c("prov_territ" = "band")) %>%
  mutate(x = percent * cos(angle), y = percent * sin(angle))
#combining the angles dataframe with the annual dataframe 

radar_lines <- radar_points %>% group_by(age_group) %>% group_modify(~ close_path(.x)) %>% ungroup()
#group by age/sex group. Then run the close path function on each group separately. Then remove groupings 


#plotting of the radar chart 
library(ggplot2)


sex_colours <- c(`51 to 70 years, females` = "#B56576", `51 to 70 years, males` = "#355070")
chart_ink <- "#0d0d0e"
chart_soft <- "#F0EBD2"
chart_height <- 560

clean_theme <- function() {
  theme_classic(base_size = 12) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      strip.background = element_rect(fill = "#FFF5F1", colour = NA),
      strip.text = element_text(face = "bold", margin = margin(7, 0, 7, 0)),
      panel.spacing.x = grid::unit(2, "lines"),
      plot.title = element_text(size = 16, face = "bold", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
      plot.caption = element_text(hjust = 0, size = 10, margin = margin(t = 10)),
      legend.position = "none", plot.margin = margin(12, 18, 12, 12))
}


radar_static <- ggplot() +
  geom_path(data = ring_points, aes(x, y, group = radius), colour = "#D8D2D5", linewidth = 0.45) +
  geom_segment(data = spokes, aes(x = 0, y = 0, xend = x, yend = y),
                colour = "#D8D2D5", linewidth = 0.45) +
  geom_text(data = axis_labels, aes(x, y, label = band), size = 4, colour = chart_ink) +
  # A light label backing keeps the radial scale readable over the upper spoke.
  annotate("label", x = 2, y = seq(10, radar_limit, 10),
           label = paste0(seq(10, radar_limit, 10), "%"),
           hjust = 0, size = 3, fill = "white", colour = chart_ink) +
  geom_path(data = radar_lines, aes(x, y, colour = age_group, linetype = age_group), linewidth = 1) +
  geom_point(data = radar_points, aes(x, y, colour = age_group), size = 2.6) +
  scale_colour_manual(values = sex_colours) +
  scale_linetype_manual(values = c(`51 to 70 years, females` = "solid", `51 to 70 years, males` = "dashed")) +
  coord_equal(
  xlim = c(-radar_limit - 5, radar_limit + 5),
  ylim = c(-radar_limit - 5, radar_limit + 5),
  clip = "off"
) +
  labs(title = "% of total energy intake by protein for Canadians 50 to 71 years of age",
       subtitle = paste("Canadian Provinces/Territories 2015 ·", "· percentage within each sex category"),
       colour = NULL, linetype = NULL,
       caption = "Compare positions on spokes, not enclosed area.") +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(size = 16, face = "bold", margin = margin(b = 8)),
        plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
        plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
        legend.position = "bottom", plot.margin = margin(15, 25, 15, 25))


radar_static