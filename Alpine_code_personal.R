#CONS 451 Alpine Report
#Prepared by Graeme Morey, UBC
#Code written by Graeme Morey with the assistance of Microsoft Copilot AI
#Date created September 15, 2025
#Date updated September 18, 2025
#Notes
  # denotes main heading
  ## denotes subheading 1
  ### denotes subheading 2, etc (for the most part)

#Libraries
library(tidyverse)
library(permute)
library(vegan)
library(ggrepel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggh4x)
library(purrr)
library(ggforce)  # for facet_wrap2
library(broom)

#Setwd
setwd("/Users/Graeme/Downloads/Dropbox/Graeme/UBC/classes/Year 4/CONS 451/Modules/Alpine/Assignments/A2/R_work")

#Load datasets
data <- read.csv("percent_cover.csv")
height <- read.csv("height.csv")
community_richness <- read.csv("community_richness.csv")

#Use new data object
##create dataset with only percent cover for each species
data_wide <- data[, 7:(length(data))]

###transform data_wide into a presence/absence object
data_pres <- data_wide |> 
  mutate_if(is.numeric, ~ 1 * (. != 0))

####Use data_pres to calculate species richness
data_rich <- data_pres |>
  mutate(Richness = select(data_pres, species1:species29) |>
           rowSums(na.rm = TRUE)) |>
  select(-starts_with('Species'))

###Use data_wide to calculate Shannon's index
data_shan <- data_wide |>
  mutate(Shannon = diversity(data_wide, "shannon")) |>
  select(-starts_with('Species'))

####create new object with two columns: data_rich and data_shan
data_all <- data_rich |>
  cbind(data_shan)

### Calculate Pielou's Evenness Index by using data_all, create new object for eveness
data_even <- data_all |>
  mutate(Pielou = data_all$Shannon / log(data_all$Richness)) |>
  select("Pielou")

####add data_even to data_all
data_all <- data_all |>
  cbind(data_even)

#####join data_all with data_wide
data_all_wide <- data |>
  cbind(data_all)

##reshape data_all_wide into long format for use in graphing
data_long <- data_all_wide |>
  pivot_longer(cols = starts_with("Species"),
               names_to = "Species",
               values_to = "Cover")

##Use height.csv dataset to allow us to plot height by distance
###Our we have chosen to only graph species that occur greater than or equal
  ###to five times across the 30 plots. This is to delete species where this
  ###is not the case
height_species_filtered <- data_pres[, colSums(data_pres) >= 5]

####we will now get the column names for the species that match this criteria
height_species_filtered_keep <- colnames(height_species_filtered)

##### we will now only keep those species by removing those that do not match 
  #####this criteria from the height dataset. We will also add back on the
  #####plot distance column
height_filtered <- height[, height_species_filtered_keep]

plot_distance <- data_all_wide["plot_distance"]

height_filtered <- height_filtered %>%
  mutate(plot_distance = plot_distance$plot_distance) %>%
  relocate(plot_distance, .before = 1)

###turn height_filtered into long form
height_long <- height_filtered %>%
  pivot_longer(
    cols = -plot_distance,           
    names_to = "Species",            
    values_to = "Height"              
  )

####Species that were not found in plots have a height listed as 0. This will
  ####mess with the data, so we will filter out any heights equaling 0, as 
  ####they were not found at those distances
height_long_filter <- height_long %>%
  filter(Height !=0)






#Run linear models
##Richness by distance
lm_distance_richness <- lm(Richness ~ plot_distance, data = data_all_wide)
summary(lm_distance_richness)

##Shannon's Index by distance
lm_distance_shan <- lm(Shannon ~ plot_distance, data = data_all_wide)
summary(lm_distance_shan)

##Eveness Index by distance
lm_distance_eveness <- lm(Pielou ~ plot_distance, data = data_all_wide)
summary(lm_distance_eveness)

#Create Figures
##Richness by Distance scatterplot
(
  richness_distance <- ggplot(data_all_wide, aes(x = plot_distance, y = Richness)) +
    geom_point(colour = "grey") +
    geom_smooth(
      method = lm ,
      colour = "grey",
      fill = "grey",
      se = TRUE
    ) +
    labs(x = "\nDistance from the trail (m)", y = "Species Richness\n") +
    annotate(
      "text",
      x = 5,
      y = 3,
      label =
        paste(
          "Adj. R square = ",
          round(summary(lm_distance_richness)$adj.r.squared, 2),
          "\nIntercept = ",
          round(lm_distance_richness$coef[[1]], 2),
          "\nSlope = ",
          round(lm_distance_richness$coef[[2]], 2),
          "\nt-value = ",
          round(summary(lm_distance_richness)$coef[2, 3], 2),
          "\np-value =",
          round(summary(lm_distance_richness)$coef[2, 4], 2),
          "\nsample size = ",
          length(unique(data_all_wide$plot_id))
        )
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +  # x-axis starts at 0
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      breaks = seq(0, max(data_all_wide$Richness, na.rm = TRUE), by = 2)  # whole numbers only
    ) +
    theme_classic()
)

### Save the figure
ggsave(
  "figure_richness_distance.jpg",
  richness_distance,
  width = 15,
  height = 10,
  units = "cm"
)

##Shannon by distance
(
  shannon_distance <- ggplot(data_all_wide, aes(x = plot_distance, y = Shannon)) +
    geom_point(colour = "grey") +
    geom_smooth(
      method = lm ,
      colour = "grey",
      fill = "grey",
      se = TRUE
    ) +
    labs(x = "\nDistance from the trail (m)", y = "Shannon's Index\n") +
    annotate(
      "text",
      x = 15,
      y = 0.5,
      label =
        paste(
          "Adj. R square = ",
          round(summary(lm_distance_shan)$adj.r.squared, 2),
          "\nIntercept = ",
          round(lm_distance_shan$coef[[1]], 2),
          "\nSlope = ",
          round(lm_distance_shan$coef[[2]], 2),
          "\nt-value = ",
          round(summary(lm_distance_shan)$coef[2, 3], 2),
          "\np-value =",
          round(summary(lm_distance_shan)$coef[2, 4], 2),
          "\nsample size = ",
          length(unique(data_all_wide$plot_id))
        )
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +  # x-axis starts at 0
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      breaks = seq(0, max(data_all_wide$Shannon, na.rm = TRUE), by = 0.5)  # whole numbers only
    ) +
    theme_classic()
)

### Save the figure
ggsave(
  "figure_shannon_distance.jpg",
  shannon_distance,
  width = 15,
  height = 10,
  units = "cm"
)

##Pielou's by distance
(
  pielou_distance <- ggplot(data_all_wide, aes(x = plot_distance, y = Pielou)) +
    geom_point(colour = "grey") +
    geom_smooth(
      method = lm ,
      colour = "grey",
      fill = "grey",
      se = TRUE
    ) +
    labs(x = "\nDistance from the trail (m)", y = "Pielou's Index\n") +
    annotate(
      "text",
      x = 15,
      y = 0.2,
      label =
        paste(
          "Adj. R square = ",
          round(summary(lm_distance_eveness)$adj.r.squared, 2),
          "\nIntercept = ",
          round(lm_distance_eveness$coef[[1]], 2),
          "\nSlope = ",
          round(lm_distance_eveness$coef[[2]], 2),
          "\nt-value = ",
          round(summary(lm_distance_eveness)$coef[2, 3], 2),
          "\np-value =",
          round(summary(lm_distance_eveness)$coef[2, 4], 2),
          "\nsample size = ",
          length(unique(data_all_wide$plot_id))
        )
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, NA),
      breaks = seq(0, max(data_all_wide$Pielou, na.rm = TRUE), by = 0.2)
    ) +
    theme_classic()
)

### Save the figure
ggsave(
  "figure_pielou_distance.jpg",
  pielou_distance,
  width = 15,
  height = 10,
  units = "cm"
)

##species height data

###first am I going to rename the species, so that their actual names come up 
  ###on the graph
height_long_filter$Species <- factor(height_long_filter$Species,
                                     levels = c("species1","species2", "species6", "species7", "species9", "species11", "species13", "species14", "species15", "species20", "species26", "species27", "species29"),
                                     labels = c("Abies lasiocarpa", "Antennaria lanata", "Cladonia", "Crustose lichen", "Eremogone capillaris", "Juncus drummondii", "Luetkea pectinata", "Luzula", "Phlox diffusa", "Polytrichum piliferum", "Vaccinium ovalifolium", "Vaccinium scoparium", "Veronica wormskjoldii"))
####I am now going to reorder the species so that Abies lasiocarpa is last
height_long_filter$Species <- factor(height_long_filter$Species,
                                     levels = c("Antennaria lanata", "Cladonia", "Crustose lichen", "Eremogone capillaris", "Juncus drummondii", "Luetkea pectinata", "Luzula", "Phlox diffusa", "Polytrichum piliferum", "Vaccinium ovalifolium", "Vaccinium scoparium", "Veronica wormskjoldii", "Abies lasiocarpa")
)

####run linear models for each species
lm_antennaria <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Antennaria lanata", ])
summary(lm_antennaria)

lm_cladonia <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Cladonia", ])
summary(lm_cladonia)

lm_lichen <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Crustose lichen", ])
summary(lm_lichen)

lm_eremogone <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Eremogone capillaris", ])
summary(lm_eremogone)

lm_juncus <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Juncus drummondii", ])
summary(lm_juncus)

lm_luetkea <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Luetkea pectinata", ])
summary(lm_luetkea)

lm_luzula <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Luzula", ])
summary(lm_luzula)

lm_phlox <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Phlox diffusa", ])
summary(lm_phlox)

lm_polytrichum <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Polytrichum piliferum", ])
summary(lm_polytrichum)

lm_vacciniumo <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Vaccinium ovalifolium", ])
summary(lm_vacciniumo)

lm_vacciniums <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Vaccinium scoparium", ])
summary(lm_vacciniums)

lm_veronica <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Veronica wormskjoldii", ])
summary(lm_veronica)

lm_abies <- lm(Height ~ plot_distance, data = height_long_filter[height_long_filter$Species == "Abies lasiocarpa", ])
summary(lm_abies)

###create table


# Create the summary table


lm_summary_table <- height_long_filter %>%
  group_by(Species) %>%
  group_split() %>%
  map_df(function(df) {
    species_name <- unique(df$Species)
    model <- lm(Height ~ plot_distance, data = df)
    summary_model <- summary(model)
    
    # Extract values
    slope <- round(summary_model$coefficients["plot_distance", "Estimate"], 2)
    std_error <- round(summary_model$sigma, 2)
    p_value <- signif(summary_model$coefficients["plot_distance", "Pr(>|t|)"], 3)
    t_value <- round(summary_model$coefficients["plot_distance", "t value"], 2)
    r_squared <- round(summary_model$r.squared, 3)
    f_stat <- round(summary_model$fstatistic[1], 2)
    
    # Combine into a multi-line string
    stats_string <- paste0(
      "Slope = ", slope, "\n",
      "Std. Error = ", std_error, "\n",
      "p-value = ", p_value, "\n",
      "t-value = ", t_value, "\n",
      "R² = ", r_squared, "\n",
      "F-statistic = ", f_stat
    )
    
    tibble(Species = species_name, Statistics = stats_string)
  })
write.csv(lm_summary_table, "lm_summary_table.csv", row.names = FALSE)






###now the graph is made
species_height_joined <- (
  ggplot(height_long_filter, aes(x = plot_distance, y = Height)) +
    geom_point(color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "grey") +
    facet_wrap(~ Species, ncol = 3, scales = "free_y") + 
    labs(x = "Distance from trail (m)", y = "Height (cm)") +
    theme_classic()
)
species_height_joined

### Save the figure
ggsave(
  "figure_species_heights.jpg",
  species_height_joined,
  width = 60,
  height = 35,
  units = "cm"
)

##trying multicoloured graphs
###group 1
group1 <- height_long_filter %>%
  filter(Species %in% c("Antennaria lanata", "Eremogone capillaris", "Luetkea pectinata", "Luzula"))

species_colors <- c(
  "Antennaria lanata" = "orchid1",
  "Eremogone capillaris" = "orchid1",
  "Luetkea pectinata" = "orchid1",
  "Luzula" = "orchid1"
)


# Ensure consistent facet order
group1$Species <- factor(group1$Species, levels = c("Antennaria lanata", "Eremogone capillaris", "Luetkea pectinata", "Luzula"))

# Create strip backgrounds
strip_backgrounds_group1 <- lapply(
  levels(group1$Species),
  function(sp) element_rect(fill = species_colors[sp], color = NA)
)

# Build the plot
species_height_group1 <- ggplot(group1, aes(x = plot_distance, y = Height)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +
  facet_wrap2(
    ~ Species,
    ncol = 2,
    scales = "free_y",
    strip = strip_themed(
      background_x = strip_backgrounds_group1,
      text_x = list(element_text(color = "black", face = "bold"))
    )
  ) +
  labs(x = "Distance from trail (m)", y = "Height (cm)") +
  theme_classic()
species_height_group1

ggsave(
  "figure_group1.jpg",
  species_height_group1,
  width = 15,
  height = 10,
  units = "cm"
)


##group2
group2 <- height_long_filter %>%
  filter(Species %in% c("Juncus drummondii", "Phlox diffusa", "Veronica wormskjoldii", "Abies lasiocarpa"))

species_colors <- c(
  "Juncus drummondii" = "tan",
  "Phlox diffusa" = "orchid1",
  "Veronica wormskjoldii" = "orchid1",
  "Abies lasiocarpa" = "palegreen"
)


# Ensure consistent facet order
group2$Species <- factor(group2$Species, levels = c(
  "Juncus drummondii",
  "Phlox diffusa",
  "Veronica wormskjoldii",
  "Abies lasiocarpa"
))

# Create strip backgrounds
strip_backgrounds_group2 <- lapply(
  levels(group2$Species),
  function(sp) element_rect(fill = species_colors[sp], color = NA)
)

# Build the plot
species_height_group2 <- ggplot(group2, aes(x = plot_distance, y = Height)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +
  facet_wrap2(
    ~ Species,
    ncol = 2,
    scales = "free_y",
    strip = strip_themed(
      background_x = strip_backgrounds_group2,
      text_x = list(element_text(color = "black", face = "bold"))
    )
  ) +
  labs(x = "Distance from trail (m)", y = "Height (cm)") +
  theme_classic()

species_height_group2


ggsave(
  "figure_group2.jpg",
  species_height_group2,
  width = 15,
  height = 10,
  units = "cm"
)


##Group 3
# Step 1: Filter Group 3
group3 <- height_long_filter %>%
  filter(Species %in% c(
    "Cladonia",
    "Crustose lichen",
    "Polytrichum piliferum",
    "Vaccinium ovalifolium",
    "Vaccinium scoparium"
  ))

# Step 2: Assign colors
species_colors <- c(
  "Cladonia" = "lightblue",
  "Crustose lichen" = "lightblue",
  "Polytrichum piliferum" = "lightblue",
  "Vaccinium ovalifolium" = "tan1",
  "Vaccinium scoparium" = "tan1"
)

# Step 3: Set facet order
group3$Species <- factor(group3$Species, levels = c(
  "Cladonia",
  "Crustose lichen",
  "Polytrichum piliferum",
  "Vaccinium ovalifolium",
  "Vaccinium scoparium"
))

# Step 4: Create strip backgrounds
strip_backgrounds_group3 <- lapply(
  levels(group3$Species),
  function(sp) element_rect(fill = species_colors[sp], color = NA)
)

# Step 5: Build the plot
species_height_group3 <- ggplot(group3, aes(x = plot_distance, y = Height)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "grey") +
  facet_wrap2(
    ~ Species,
    ncol = 2,
    scales = "free_y",
    strip = strip_themed(
      background_x = strip_backgrounds_group3,
      text_x = list(element_text(color = "black", face = "bold"))
    )
  ) +
  labs(x = "Distance from trail (m)", y = "Height (cm)") +
  theme_classic()
species_height_group3

ggsave(
  "figure_group3.jpg",
  species_height_group3,
  width = 15,
  height = 15,
  units = "cm"
)

##Community richness by distance


# Reshape and rename vegetation types
community_richness_long <- community_richness %>%
  pivot_longer(cols = c(Lichen_Moss, Shrub, Forbe, Grass_Rush, Tree),
               names_to = "Vegetation_Type",
               values_to = "Species_Richness") %>%
  mutate(Vegetation_Type = recode(Vegetation_Type,
                                  Lichen_Moss = "Lichen and Moss",
                                  Shrub = "Shrub",
                                  Forbe = "Forbe",
                                  Grass_Rush = "Grass and Rush",
                                  Tree = "Tree"
  ),
  Vegetation_Type = factor(Vegetation_Type,
                           levels = c("Lichen and Moss", "Shrub", "Forbe", "Grass and Rush", "Tree"))
  )

# Define custom strip backgrounds
strip_backgrounds <- list(
  "Lichen and Moss" = element_rect(fill = "lightblue", color = NA),
  "Shrub" = element_rect(fill = "tan1", color = NA),
  "Forbe" = element_rect(fill = "orchid1", color = NA),
  "Grass and Rush" = element_rect(fill = "tan", color = NA),
  "Tree" = element_rect(fill = "palegreen", color = NA)
)

# Create the ggplot
community_richness_graph <- ggplot(community_richness_long, aes(x = plot_distance, y = Species_Richness)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey") +
  facet_wrap2(
    ~ Vegetation_Type,
    ncol = 2,
    strip = strip_themed(
      background_x = strip_backgrounds,
      text_x = list(element_text(color = "black", face = "bold"))
    )
  ) +
  labs(x = "Distance from Trail (m)",
       y = "Species Richness") +
  theme_classic()

# Display the graph
community_richness_graph


# Display the graph
community_richness_graph
ggsave(
  "figure_community_richness.jpg",
  community_richness_graph,
  width = 17,
  height = 20,
  units = "cm"
)

##linear regression for the community richness



lm_community_richness <- community_richness_long %>%
  group_by(Vegetation_Type) %>%
  group_split() %>%
  map_df(function(df) {
    vegetation_name <- unique(df$Vegetation_Type)
    model <- lm(Species_Richness ~ plot_distance, data = df)
    summary_model <- summary(model)
    
    # Extract statistics
    slope <- round(summary_model$coefficients["plot_distance", "Estimate"], 2)
    std_error <- round(summary_model$sigma, 2)
    p_value <- signif(summary_model$coefficients["plot_distance", "Pr(>|t|)"], 3)
    t_value <- round(summary_model$coefficients["plot_distance", "t value"], 2)
    r_squared <- round(summary_model$r.squared, 3)
    f_stat <- round(summary_model$fstatistic[1], 2)
    
    # Combine into a multi-line string
    stats_string <- paste0(
      "Slope = ", slope, "\n",
      "Std. Error = ", std_error, "\n",
      "p-value = ", p_value, "\n",
      "t-value = ", t_value, "\n",
      "R² = ", r_squared, "\n",
      "F-statistic = ", f_stat
    )
    
    tibble(Vegetation_Type = vegetation_name, Statistics = stats_string)
  })
write.csv(lm_community_richness, "lm_community_richness.csv", row.names = FALSE)




