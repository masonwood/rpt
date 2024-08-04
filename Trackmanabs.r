library(ggplot2)
library(dplyr)
library(data.table)

# Load the data
df <- fread("/Users/Mason/Downloads/Season.csv") %>%
  distinct()

df <- df %>%
  filter(Batter == "Player, Test")

df <- df %>%
  filter(PitchCall == "InPlay")

# Define the breaks for the grid
x_breaks <- seq(-1.05, 1.05, length.out = 4)
y_breaks <- seq(1.6, 3.3, length.out = 4)

# Create a grid of tiles
tiles <- expand.grid(
  x = x_breaks[-length(x_breaks)] + diff(x_breaks) / 2,
  y = y_breaks[-length(y_breaks)] + diff(y_breaks) / 2
)

# Print tiles for debugging
print("Tiles:")
print(tiles)

# Count the number of points in each tile
counts <- df %>%
  mutate(
    x_bin = cut(-PlateLocSide, breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
    y_bin = cut(PlateLocHeight, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
  ) %>%
  filter(!is.na(x_bin) & !is.na(y_bin)) %>%  # Remove NA bins
  group_by(x_bin, y_bin) %>%
  summarise(count = n(), .groups = 'drop')

# Print counts for debugging
print("Counts:")
print(counts)

# Calculate the midpoint for each bin
counts <- counts %>%
  mutate(
    x = (x_breaks[x_bin] + x_breaks[x_bin + 1]) / 2,
    y = (y_breaks[y_bin] + y_breaks[y_bin + 1]) / 2
  )

# Print counts with midpoints for debugging
print("Counts with Midpoints:")
print(counts)

# Merge the tile data with the counts
tiles <- merge(tiles, counts[, c("x", "y", "count")], by = c("x", "y"), all.x = TRUE)
tiles$count[is.na(tiles$count)] <- 0  # Replace NA counts with 0

# Print final tile data for debugging
print("Final Tiles:")
print(tiles)

# Calculate the density (count / max count) for color scaling
tiles$density <- tiles$count / max(tiles$count)

# Print tiles with density for debugging
print("Tiles with Density:")
print(tiles)

# Plot the heatmap with individual points
plot1 <- ggplot() +
  geom_tile(data = tiles, aes(x = x, y = y, fill = density), color = "black") +
  scale_fill_gradient2(low = "#3661ad", mid = "white", high = "#d82129", midpoint = 0.5, na.value = "white") +
  geom_point(data = df, aes(x = -PlateLocSide, y = PlateLocHeight), color = "black", size = 1, alpha = 0.6) +
  geom_text(data = tiles, aes(x = x, y = y, label = count), color = "black") +
  geom_rect(xmin = -1.05, xmax = 1.05, ymin = 1.6, ymax = 3.3, fill = "transparent", color = "black") +
  ylim(.75, 3.75) +
  xlim(-2, 2) +
  theme_classic() +
  xlab("") +
  ylab("") +
  labs(title = "In Play (Keenan, Jimmy)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  coord_fixed(ratio = 1.3)

print(plot1)
