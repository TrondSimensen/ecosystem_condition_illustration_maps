library(sf)
library(dplyr)
library(tidyverse)
library(tmap)
library(tidytext)
# library(dotwhisker)
library(gridExtra)
library(ggpubr)
library(terra)
library(tmaptools)
library(exactextractr)

options(scipen=999)

Sys.setlocale("LC_CTYPE", "norwegian")




# Final plot --------------------------------------------------------------

tm1 <- tm_shape(komm, unit = "km") +
  tm_polygons(col = "var_1", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1), 
              palette = "Reds", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 100, 200), color.dark = "grey30") +
  tm_layout(main.title = "a)",  main.title.size = 1.5, title = "Tilstandsvariabel", title.size = 1, frame = FALSE, 
            legend.show = TRUE, legend.position = c("left", "top"), 
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE, 
            attr.outside = FALSE)

tm1

tm2 <- tm_shape(aggregated_data, unit = "km") +
  tm_polygons(col = "var_1", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1), 
              palette = "Reds", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), , just = -1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 100, 200), color.dark = "grey30") +
  tm_layout(main.title = "b)",  main.title.size = 1.5, title = "Tilstandsvariabel", title.size = 1, frame = FALSE, 
            legend.show = TRUE, legend.position = c("left", "top"), 
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE, 
            attr.outside = FALSE)


tm2

tm3 <- tm_shape(ruter2, unit = "km") +
  tm_polygons(col = "var_1", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1), 
              palette = "Reds", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 100, 200), color.dark = "grey30") +
  tm_layout(main.title = "c)",  main.title.size = 1.5, title = "Tilstandsvariabel", title.size = 1, frame = FALSE, 
            legend.show = TRUE, legend.position = c("left", "top"), 
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE, 
            attr.outside = FALSE)

tmap_arrange(tm2, tm1, tm3)

# Set width of plot
width_in_inches <- 29 / 2.54

# Combine your maps
map_combined <- tmap_arrange(tm2, tm1, tm3)

# Save the combined map
tmap_save(map_combined, filename = "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/Okologisk_tilstand/combined_map.jpg", width = width_in_inches, dpi = 300)



# Tidsserie Trøndelag -----------------------------------------------------

#data_orig <- data |> filter(Hyttekomtype == "Mountain")
data_orig <- komm |> filter(Fylke == "Trøndelag - Trööndelage")

data_orig <- data_orig |> select(Region, Kommune, var_1)
data_orig 

# tm_shape(data_orig, unit = "km") +
#   tm_polygons(col = "var_1", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1), 
#               palette = "Reds", border.col = "white", border.alpha = 0.5, title = " ") +
#   tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
#   tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
#   tm_layout(main.title = "a)",  main.title.size = 1.5, title = "Tilstandsvariabel", title.size = 1, frame = FALSE, 
#             legend.show = TRUE, legend.position = c("left", "top"), 
#             legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE, 
#             attr.outside = FALSE)


library(dplyr)
library(scales)

# Assuming data_orig is your original data frame
set.seed(123) # for reproducibility

# Generate 6 new variables with decreasing trend and random noise
for (i in 2:7) {
  data_orig[[paste0("var_", i)]] <- data_orig$var_1 * (1 - (i - 1) / 10) + runif(nrow(data_orig), -0.05, 0.05)
}

# Scale variables to 0-1
data_orig_scaled <- data_orig %>%
  mutate(across(starts_with("var_"), ~ rescale(.x, to = c(0, 1))))

# Check the results
print(head(data_orig_scaled))

tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_1", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Reds", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "a)",  main.title.size = 1.5, title = "Tilstandsvariabel", title.size = 1, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)


# Create a tmap plot for each variable
# tm1 <- tm_shape(data_orig_scaled) +
#   tm_polygons(col = "var_1", title = "var_1") +
#   tm_facets(free.scales = FALSE, ncol = 3, nrow = 2) +
#   tm_layout(frame = FALSE, legend.position = c("left", "top"))
# 
# tm1
# 
# # library(tmap)
# # library(sf)
# 
# # # Assuming data_orig_scaled is already in your workspace
# # # Set the tmap mode to plotting
# tmap_mode("plot")
# # 
# # Create individual tmap plots for each variable
# tm1 <- tm_shape(data_orig_scaled) +
#   tm_polygons(col = "var_1", palette = "Greens", border.col = "white", border.alpha = 0.5) +
#   tm_layout(main.title = "Var 1", frame = FALSE, legend.position = c("left", "top"))
# 
# tm1

data_orig


tm1 <- tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_1", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Greens", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "a) Tidspunkt 1",  main.title.size = 1.5, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)

tm1

tm2 <- tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_2", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Greens", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "b) Tidspunkt 2",  main.title.size = 1.5, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)

tm2

tm3 <- tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_3", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Greens", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "c) Tidspunkt 3",  main.title.size = 1.5, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)

tm3

tm4 <- tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_4", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Greens", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "d) Tidspunkt 4",  main.title.size = 1.5, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)

tm4

tm5 <- tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_5", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Greens", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "e) Tidspunkt 5",  main.title.size = 1.5, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)

tm5

tm6 <- tm_shape(data_orig, unit = "km") +
  tm_polygons(col = "var_6", style = "fixed", breaks = c(0, 0.25, 0.5, 0.75, 1),
              palette = "Greens", border.col = "white", border.alpha = 0.5, title = " ") +
  tm_compass(type = "arrow", color.dark = "grey30", position = c("right", "bottom"), just = 1, size = 1.5) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5, breaks = c(0, 50, 100), color.dark = "grey30") +
  tm_layout(main.title = "f) Tidspunkt 6",  main.title.size = 1.5, frame = FALSE,
            legend.show = TRUE, legend.position = c("left", "top"),
            legend.text.size = 1, legend.title.size = 1, legend.outside = FALSE,
            attr.outside = FALSE)

tm6

# # Arrange the tmap plots in a 3x2 grid
# tmap_arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol = 3, nrow = 2)
# 

# Set width of plot
width_in_inches <- 29 / 2.54

# Combine your maps
map_combined_multiple <- tmap_arrange(tm1, tm2, tm3, tm4, tm5, tm6, ncol = 3, nrow = 2)

# Save the combined map
tmap_save(map_combined_multiple, filename = "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/Okologisk_tilstand/combined_map_multiples.jpg", width = width_in_inches, dpi = 300)


