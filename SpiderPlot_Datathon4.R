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

#now we will make a dot plot since the values are too close

dot_data <- profiles %>% filter(year == radar_year) %>%
  mutate(row = match(as.character(band), profile_axes),
         y = row + ifelse(sex == "Females", -0.13, 0.13))



radar_dots <- ggplot(annual, aes(x = percent, y = prov_territ, colour = age_group)) +
  geom_point(size = 3) +
  scale_colour_manual(values = sex_colours) +
  scale_y_reverse(breaks = 1:10, labels = profile_axes)+
  scale_x_continuous(limits = c(0, radar_limit), breaks = seq(0, radar_limit, 5),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "dot plot", subtitle = paste("Canadian Adults 51-70 years"),
       x = "Percentage of protein intake", y = NULL, colour = NULL) +
  clean_theme() + theme(legend.position = "bottom")

#recreate the dataset so now we have the CI's

annual1 <- radar_raw %>%
  filter(REF_DATE == 2015, GEO %in% profile_axes,
        Dietary.age.sex.reference.intake.group %in% age_bands,
        Characteristics!="Coefficient of variation for mean of percentage of total energy intake from protein") %>%
  transmute(year = as.integer(REF_DATE),
    age_group = as.factor(Dietary.age.sex.reference.intake.group),
    prov_territ = as.factor(GEO),
    calculation = as.factor(Characteristics),
    percent=VALUE
  )
#pivot wider so that the CIs and mean values are on separate columns 
dot_data <- annual1 %>%
  select(age_group, prov_territ, calculation, percent) %>%
  tidyr::pivot_wider(
    names_from = calculation,
    values_from = percent
  )

dotplot <- ggplot(
  dot_data,
  aes(
    x = `Mean of percentage of total energy intake from protein`,
    y = reorder(prov_territ, `Mean of percentage of total energy intake from protein`),
    colour = age_group
  )) +
  geom_errorbarh(
    aes(
      xmin = `Low 95% confidence interval, mean of percentage of total energy intake from protein`,
      xmax = `High 95% confidence interval, mean of percentage of total energy intake from protein`
    ),
    height = 0.15,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 1.5
  ) +
  scale_colour_manual(values = sex_colours) +
  scale_x_continuous(limits = c(0, 25),
labels = function(x) paste0(x, "%")) +  
  labs(
    title = "% of total energy intake by protein for Canadians 50 to 71 years of age",
    subtitle = "Canadian Provinces/Territories, 2015",
    x = "Percent of total energy intake from protein",
    y = NULL,
    colour = NULL
  ) +
  geom_text(
  aes(
    label = sprintf("%.1f%%",
      `Mean of percentage of total energy intake from protein`)
  ),
  size = 2.2,
  hjust = -0.4,
  position = position_dodge(width = 0.4),
  show.legend = FALSE
) + 
  clean_theme() +
  theme(
  legend.position = "bottom",
  panel.grid.major.y = element_line(
    colour = "grey85",
    linewidth = 0.3,
    linetype = "dashed"
  ))


dotplot

range(dot_data$`Mean of percentage of total energy intake from protein`)
range(dot_data$`Low 95% confidence interval, mean of percentage of total energy intake from protein`)
range(dot_data$`High 95% confidence interval, mean of percentage of total energy intake from protein`)

