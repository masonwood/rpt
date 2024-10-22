library(ggplot2)
library(dplyr)
library(GeomMLBStadiums)
library(gridExtra)

# Load your dataset
df <- read.csv("/Users/Mason/Desktop/R Projects/Data/mlb/savant_data (1).csv")

# Calculate the mean strike zone top and bottom
mean_strike_zone_top <- mean(df$sz_top, na.rm = TRUE)
mean_strike_zone_bottom <- mean(df$sz_bot, na.rm = TRUE)

# Filter the data for 'hit_into_play' description and relevant columns
df_filtered <- df %>% 
  filter(description == "hit_into_play", 
         !is.na(plate_x), !is.na(plate_z), !is.na(launch_speed)) %>%
  mutate(launch_speed_rounded = round(launch_speed))  # Round the launch speed

# Create the scatterplot where point color and size are based on launch_speed
plot_exit_velo <- ggplot(df_filtered, aes(x = plate_x, y = plate_z)) +
  # Use annotate to draw the rectangle for the strike zone
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = mean_strike_zone_bottom, ymax = mean_strike_zone_top,
           fill = NA, color = "black") +
  geom_point(aes(fill = launch_speed, color = launch_speed), 
             shape = 21, size = 3, stroke = 1) +
  # Add the rounded launch_speed inside each point and color the text by launch_speed
  geom_text(aes(label = launch_speed_rounded, color = launch_speed), size = 1, vjust = 0.5, hjust = 0.5) +
  # Set the fill color gradient with a white midpoint
  scale_fill_gradient2(low = "#8BA1CAB3", mid = "#ffffffB3", high = "#CB4240B3", midpoint = 89) +
  # Set the stroke color gradient with a white midpoint
  scale_color_gradient2(low = "#8BA1CA", mid = "#ffffff", high = "#CB4240", midpoint = 89) +
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(
    title = "Exit Velo",
    x = "",
    y = ""
  ) +
  xlim(-3, 3) +  # Set a wide horizontal range
  ylim(0, 5) +   # Set vertical range for pitch height
  coord_fixed(ratio = 1.35) +
  labs(title = "Exit Velo") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),   # Remove the axis numbers
    axis.ticks = element_blank()
  )

hit_data <- subset(df_filtered, !is.na(events) & events %in% c("single", "double", "triple", "home_run", "field_out"))

# Apply the MLBAM coordinate transformation
transformed_hit_data <- mlbam_xy_transformation(hit_data)

# Spray chart plot with correct color matching
plot_spray <- ggplot(transformed_hit_data, aes(x = hc_x_, y = hc_y_, fill = events)) +
  geom_point(shape = 21, stroke = .5, size = 3, color = 'black', show.legend = FALSE) +  # Remove redundant top legend and use fill only
  geom_mlb_stadium(stadium_ids = 'yankees',
                   stadium_transform_coords = TRUE, 
                   stadium_segments = c('outfield_outer', 'infield_outer', 'infield_inner', 'foul_lines'),  # Exclude 'outfield_inner'
                   linewidth = .5, 
                   color = 'black') + 
  theme_void() + 
  coord_fixed() +  # Keeps the aspect ratio of the field correct
  labs(title = "Spray", x = "", y = "") +
  theme_minimal() +
  scale_fill_manual(values = c("field_out" = "grey", "single" = "#FE6100", "double" = "#785EF0", "triple" = "#FFB000", "home_run" = "#DC267F"),
                    name = "Event", # Add a cleaner legend title
                    labels = c("Field Out", "Single", "Double", "Triple", "Home Run")) +  # Adjust the labels
  
  # Modify the legend theme
  theme(
    legend.title = element_text(size = 12, face = "bold"),  # Make the legend title bold
    legend.text = element_text(size = 10),                  # Adjust the size of legend text
    legend.key = element_rect(fill = "white", color = NA),  # Set background of legend keys
    legend.key.size = unit(0.75, "cm"),                     # Adjust the size of legend keys
    legend.position = "right",                              # Position the legend to the right
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),   # Remove the axis numbers
    axis.ticks = element_blank()
  )

# Filter data for specific events
contour_df <- df %>%
  filter(events %in% c("single", "double", "triple", "home_run"))

plot_damage <- ggplot(contour_df, aes(x = plate_x, y = plate_z)) +
  # Create a smooth density-based heatmap for event occurrence
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour = TRUE, bins = 8) +
  # Set the color gradient (you can change the colors if needed)
  scale_fill_gradientn(colors = c("#8BA1CA", "#ffffff", "#CB4240")) +
  # Draw the strike zone rectangle
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = mean_strike_zone_bottom, ymax = mean_strike_zone_top,
           fill = NA, color = "black") +
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(title = "Damage", x = "", y = "") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),   # Remove the axis numbers
    axis.ticks = element_blank()
  ) +
  xlim(-3, 3) +  # Set a wide horizontal range
  ylim(0, 5) +   # Set vertical range for pitch height
  coord_fixed(ratio = 1.35)

df <- df %>%
  mutate(
    swing_take = case_when(
      description %in% c("swinging_strike", "foul", "hit_into_play", "swinging_strike_blocked", "foul_tip") ~ "swing",
      description %in% c("ball", "called_strike") ~ "take",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(swing_take))  # Filter out rows without swing/take

# Create the plot
plot_swings <- ggplot(df, aes(x = plate_x, y = plate_z, color = swing_take)) +
  geom_point(size = 3, stroke = 1, alpha = .65) +
  # Define colors for swings (green) and takes (red)
  scale_color_manual(values = c("swing" = "#1DBE3A", "take" = "#D22D49")) +
  # Add strike zone rectangle
  annotate("rect", xmin = -0.85, xmax = 0.85, ymin = 1.5, ymax = 3.5,
           fill = NA, color = "black") +
  theme_minimal() +
  labs(title = "Swings", x = "", y = "") +
  theme(
    legend.position='none',
    plot.title = element_text(hjust = 0.5),
    plot.background = element_blank(), 
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),   # Remove the axis numbers
    axis.ticks = element_blank()
  ) +
  coord_fixed(ratio = 1.35)


pdf("/Users/Mason/Desktop/rimages/battercard.pdf", width = 8, height = 10)  # Set the desired dimensions
grid.arrange(plot_swings, plot_damage, plot_exit_velo, plot_spray, nrow = 2, ncol = 2)
dev.off()