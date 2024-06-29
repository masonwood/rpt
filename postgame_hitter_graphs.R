library(ggplot2)
library(gridExtra)
library(grid)
library(plotly)
library(dplyr)
library(tidyverse)

df <- read_csv("/Users/Mason/Downloads/Orleans Firebirds Trackman 2024 - 6_27.csv")

df <- df %>%
  mutate(RelSpeed = round(RelSpeed, 1))

# Define a batter name that you want to plot
desired_batter <- "Keenan, Jimmy"  # Replace this with the exact name of the batter you want

output_pdf <- "Keenan_6_27_24.pdf"

pitch_colors <- c('Four-seam Fastball' = rgb(210/255, 45/255, 73/255),
                  'One-seam Fastball' = rgb(210/255, 45/255, 73/255),
                  'Fastball' = rgb(210/255, 45/255, 73/255),
                  'Two-seam Fastball' = rgb(222/255, 106/255, 4/255),
                  'Sinker' = rgb(254/255, 157/255, 0/255),
                  'Cutter' = rgb(147/255, 63/255, 44/255),
                  'Slider' = rgb(238/255, 231/255, 22/255),
                  'Splitter' = rgb(136/255, 136/255, 136/255),
                  'ChangeUp' = rgb(29/255, 190/255, 58/255),
                  'Sweeper' = rgb(221/255, 179/255, 58/255),
                  'Knuckle Curve' = rgb(98/255, 54/255, 205/255),
                  'Curveball' = rgb(0/255, 209/255, 237/255),
                  'Slurve' = rgb(147/255, 175/255, 212/255),
                  'Screwball' = rgb(96/255, 219/255, 51/255),
                  'Forkball' = rgb(85/255, 204/255, 171/255),
                  'Eephus' = rgb(136/255, 136/255, 136/255),
                  'Knuckleball' = rgb(60/255, 82/255, 205/255),
                  'Other' = rgb(136/255, 136/255, 136/255))

event_shapes <- c('BallCalled' = 0, 
                  'BallInDirt' = 0, 
                  'BallIntentional' = 0,
                  'InPlay' = 2, 
                  'StrikeSwinging' = 4, 
                  'FoulBall' = 8, 
                  'FoulBallNotFieldable' = 8, 
                  'FoulBallFieldable' = 8, 
                  'StrikeCalled' = 1,
                  'HitByPitch' = 10)

# Coordinates for the rectangle
xmin <- -1.1
xmax <- 1.1
ymin <- 1.35
ymax <- 3.15

# Calculate one third of the horizontal and vertical distances
x_third <- (xmax - xmin) / 3
y_third <- (ymax - ymin) / 3

# Calculate positions for the vertical lines
x_vert1 <- xmin + x_third
x_vert2 <- xmax - x_third

# Calculate positions for the horizontal lines
y_horiz1 <- ymin + y_third
y_horiz2 <- ymax - y_third

rectangle_data <- data.frame(
  xmin = c(-1.1),
  xmax = c(1.1),
  ymin = c(1.35),
  ymax = c(3.15)
)

inner_xmin <- xmin + (xmax - xmin) * 0.135
inner_xmax <- xmax - (xmax - xmin) * 0.135
inner_ymin <- ymin + (ymax - ymin) * 0.135
inner_ymax <- ymax - (ymax - ymin) * 0.135

inner_rectangle_data <- data.frame(
  xmin = c(inner_xmin),
  xmax = c(inner_xmax),
  ymin = c(inner_ymin),
  ymax = c(inner_ymax)
)

outer_xmin <- xmin - (xmax - xmin) * 0.125
outer_xmax <- xmax + (xmax - xmin) * 0.125
outer_ymin <- ymin - (ymax - ymin) * 0.125
outer_ymax <- ymax + (ymax - ymin) * 0.125

# Define your rectangles
top_rect <- data.frame(xmin = outer_xmin, xmax = outer_xmax, ymin = ymax, ymax = outer_ymax)
bottom_rect <- data.frame(xmin = outer_xmin, xmax = outer_xmax, ymin = outer_ymin, ymax = ymin)
left_rect <- data.frame(xmin = outer_xmin, xmax = xmin, ymin = ymin, ymax = ymax)
right_rect <- data.frame(xmin = xmax, xmax = outer_xmax, ymin = ymin, ymax = ymax)

df_selected <- df %>% 
  dplyr::select(Batter, TaggedPitchType, PitchCall, TaggedHitType, Pitcher, PitcherThrows, Inning, KorBB, PlayResult, ExitSpeed, Angle, BatterTeam, PitchofPA, PlateLocHeight, PlateLocSide, Balls, Strikes, RelSpeed) %>%
  mutate(Count = paste(Balls, Strikes, sep="-"))

df_selected <- df_selected %>%
  mutate(
    PitchLocation = case_when(
      # Inner region
      PlateLocSide >= inner_xmin & PlateLocSide <= inner_xmax & 
        PlateLocHeight >= inner_ymin & PlateLocHeight <= inner_ymax ~ "Heart",
      
      # Outer region (but not in inner)
      PlateLocSide >= outer_xmin & PlateLocSide <= outer_xmax & 
        PlateLocHeight >= outer_ymin & PlateLocHeight <= outer_ymax ~ "Shadow",
      
      # Neither
      TRUE ~ "Chase"
    )
  )

df_selected <- df_selected %>%
  mutate(
    Decision = case_when(
      # Swing decision
      PitchCall %in% c("InPlay", "FoulBall", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable") ~ "Swing",
      
      # Take decision
      PitchCall %in% c("BallCalled", "StrikeCalled", "HitByPitch") ~ "Take",
      
      # Default (if there are any other cases you haven't mentioned)
      TRUE ~ NA_character_
    )
  )

#sju_data <- df_selected %>% 
#dplyr::filter(BatterTeam == "STJ_RED")

pa_sju_data <- df_selected %>%
  group_by(Batter) %>%
  mutate(PA = cumsum(PitchofPA == 1)) %>%
  ungroup() %>%
  mutate(Angle = round(Angle, 2),
         ExitSpeed = round(ExitSpeed, 2))

# Filter the data for the desired batter
desired_batter_data <- pa_sju_data %>% filter(Batter == desired_batter)

# Split the data by PA
split_data <- split(desired_batter_data, desired_batter_data$PA)

title_grob <- textGrob(desired_batter, gp = gpar(fontsize = 20, col = "black"), vjust = 1)

plot_height <- unit(2, "inches") # Adjust as necessary

add_header_to_plot <- function(plot_grob, batter_name) {
  # Create a text grob for the header
  header_grob <- textGrob(batter_name, gp = gpar(fontsize = 8, fontface = "bold"))
  
  # Calculate the height for the header, adjusting if necessary
  header_height <- grobHeight(header_grob) + unit(2, "mm")  # Add a small buffer
  
  # Add the header to the plot grob with enough space
  plot_grob <- gtable::gtable_add_rows(plot_grob, heights = header_height, 0)
  plot_grob <- gtable::gtable_add_grob(plot_grob, list(header_grob), t = 1, l = 1, r = ncol(plot_grob))
  
  return(plot_grob)
}

split_into_chunks <- function(x, chunk_size) {
  split(x, ceiling(seq_along(x) / chunk_size))
}

# Generate a list of plots with tables
plot_list <- lapply(split_data, function(data){
  gg <- ggplot(data = data, aes(x = -PlateLocSide, y = PlateLocHeight, color = TaggedPitchType, shape = PitchCall)) +
    annotate("rect", xmin = -Inf, xmax = outer_xmin, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.15) +  # Left
    annotate("rect", xmin = outer_xmax, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.15) +  # Right
    annotate("rect", xmin = outer_xmin, xmax = outer_xmax, ymin = -Inf, ymax = outer_ymin, fill = "blue", alpha = 0.15) +  # Bottom
    annotate("rect", xmin = outer_xmin, xmax = outer_xmax, ymin = outer_ymax, ymax = Inf, fill = "blue", alpha = 0.15) +  # Top  
    ylim(0,4) +
    xlim(-3,3) +
    geom_point(size = 3) +
    geom_text(aes(label = PitchofPA), size = 2, color = "black") +  # New layer for text labels
    geom_rect(data = rectangle_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, show.legend = FALSE, fill = "transparent", color = "black") +
    geom_rect(data = rectangle_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, show.legend = FALSE, fill = "transparent", color = "black") +
    geom_rect(data = inner_rectangle_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, show.legend = FALSE, fill = "red", alpha = 0.15) +
    geom_text(
      data = subset(data, PitchCall == 'InPlay'),
      aes(
        label = paste(
          "Result:", PlayResult, "\n",
          "EV:", ExitSpeed, "\n",
          "LA:", Angle
        ),
        x = 0, y = 0  # Set x to 0 for centering
      ), 
      vjust = -0.5,
      size = 2, 
      color = "black"
    ) +
    scale_color_manual(values = pitch_colors) +
    scale_shape_manual(values = event_shapes) + 
    guides(color = guide_legend(override.aes = list(shape = 19))) +
    guides(shape = guide_legend(override.aes = list(color = "black"))) +
    theme_classic() +
    xlab("Plate Side (ft)") +
    ylab("Plate Height (ft)") +
    labs(title = ifelse(unique(data$PitcherThrows) == "Left", 
                        paste("PA", unique(data$PA), "Inning", unique(data$Inning), "(vs. LHP", unique(data$Pitcher),")"), 
                        paste("PA", unique(data$PA), "Inning", unique(data$Inning), "(vs.", unique(data$Pitcher),")"))) +
    theme(plot.title = element_text(hjust = 0.5, size = 10),  # Adjusted size
          axis.title.x = element_text(size = 5),             # Adjusted size
          axis.title.y = element_text(size = 5)) + 
    coord_fixed(ratio = 2) +
    theme(legend.position = "none")
  
  table_data <- data %>% 
    select(PitchofPA, TaggedPitchType, Count, PitchCall, PitchLocation, Decision, RelSpeed) %>%  # Added RelSpeed here
    rename(
      `#` = PitchofPA,
      `Pitch Type` = TaggedPitchType,
      Count = Count,
      Result = PitchCall,
      Zone = PitchLocation,
      Decision = Decision,
      `Velocity` = RelSpeed  # Renamed RelSpeed here
    ) %>% 
    select(`#`, Count, `Pitch Type`, Velocity, Zone, Decision, Result) %>%  # Adjusted the order here
    distinct()
  
  my_theme <- gridExtra::ttheme_minimal(
    core = list(fg_params = list(cex = .4, col = "black"),
                bg_params = list(fill = "white", col = "darkgrey")),
    colhead = list(fg_params = list(cex = .5, col = "white", fontface = 1),
                   bg_params = list(fill = "#BA0C2F", col = "lightgrey"))
  )
  
  table_grob <- gridExtra::tableGrob(table_data, theme = my_theme, rows = NULL)
  
  plot_grob <- ggplotGrob(gg)
  
  # Add space for the plot at a standardized height
  combined_plot <- gtable::gtable_add_rows(plot_grob, plot_height, -1)
  
  # Add the table below the plot
  combined_plot <- gtable::gtable_add_grob(combined_plot, list(table_grob), t = -.5, l = 1, r = 9.5)
  
  return(combined_plot)
})

plot_list_with_headers <- lapply(plot_list, function(combined_plot_grob) {
  # Add the header to each combined plot grob
  add_header_to_plot(combined_plot_grob, desired_batter)
})

output_file_path <- output_pdf

pdf(file = output_file_path, width = 8.25, height = 11)  # Adjust width and height as needed

plot_chunks <- split_into_chunks(plot_list_with_headers, 4)

# Loop through each chunk and arrange the plots
for (chunk in plot_chunks) {
  do.call(gridExtra::grid.arrange, c(chunk, ncol=2))
}

# Close the PDF device
dev.off()
