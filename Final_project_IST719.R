file_path <- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\appearances.csv"

file_path_2 <-"C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\club_games.csv"

file_path_3 <- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\clubs.csv"

file_path_4 <- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\competitions.csv"

file_path_5<- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\game_events.csv"

file_path_6 <- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\games.csv"

file_path_7 <- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\player_valuations.csv"

file_path_8 <- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\players.csv"

file_path_9<- "C:\\Users\\akank\\OneDrive\\Desktop\\ist 719\\Project\\Final Project Data\\transfers.csv"

appearances <- read.csv(file = file_path, stringsAsFactors = TRUE)
club_games<- read.csv(file = file_path_2, stringsAsFactors = TRUE)
clubs <- read.csv(file = file_path_3, stringsAsFactors = TRUE)
competitions <- read.csv(file = file_path_4, stringsAsFactors = TRUE)
game_events <-read.csv2(file = file_path_5, stringsAsFactors = TRUE)
games<-read.csv(file=file_path_6, stringsAsFactors = TRUE)
player_valuations <- read.csv(file = file_path_7, stringsAsFactors = TRUE)
players<- read.csv(file = file_path_8, stringsAsFactors = TRUE)
transfer <- read.csv(file = file_path_9, stringsAsFactors = TRUE)

# bar chart 

library(dplyr)
library(ggplot2)
library(scales)

library(dplyr)
library(ggplot2)
library(scales)

# Step 1: Qualified countries
qualified_countries <- c(
  "France", "Germany", "England", "Spain", "Portugal", "Netherlands", "Italy", "Belgium",
  "Croatia", "Switzerland", "Denmark", "Serbia", "Poland", "Austria", "Czech Republic", "Ukraine",
  "Brazil", "Argentina", "Uruguay", "Colombia", "Ecuador", "Chile", "Paraguay",
  "Japan", "South Korea", "Iran", "Australia", "Saudi Arabia", "Qatar", "Uzbekistan", "Iraq", "UAE",
  "Senegal", "Morocco", "Nigeria", "Egypt", "Tunisia", "Algeria", "Ghana", "Cameroon", "Mali", "South Africa",
  "United States", "Mexico", "Canada", "Costa Rica", "Panama", "Jamaica"
)

squad_value <- players %>%
  filter(country_of_citizenship %in% qualified_countries) %>%
  group_by(country_of_citizenship) %>%
  summarize(total_market_value = sum(market_value_in_eur, na.rm = TRUE)) %>%
  arrange(desc(total_market_value)) %>%
  slice_head(n = 48)

ggplot(squad_value, aes(x = reorder(country_of_citizenship, -total_market_value), 
                        y = total_market_value, 
                        fill = total_market_value)) +
  geom_col() +
  scale_fill_gradient(
    low = "lightblue",
    high = "navy",
    name = "Squad Value"
  ) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(
    title = "Top 48 FIFA 2026 Teams by Squad Market Value",
    x = "Country", y = "Total Market Value (USD)",
    caption = "Data Source: Transfermarkt"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 9),  # Vertical labels
    axis.text.y = element_text(size = 10)
  )



#map


library(countrycode)
library(RColorBrewer)

world_map <- map_data("world")

#country names merge
squad_value_map <- squad_value %>%
  mutate(region = countrycode(country_of_citizenship, origin = "country.name", destination = "country.name"))

map_df <- left_join(world_map, squad_value_map, by = "region")

# Plot
ggplot(map_df, aes(long, lat, group = group, fill = total_market_value)) +
  geom_polygon(color = "gray90", size = 0.2) +
  scale_fill_gradient(
    low = "lightblue",     # very light purple for low or 0 values
    high = "navy",    # dark indigo purple for high values
    na.value = "gray95",
    labels = label_dollar(scale_cut = cut_short_scale()),
    name = "Market Value (€)"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Total Squad Market Value by Country – FIFA 2026",
    caption = "Data Source: Transfermarkt"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )


# Alluvial Plot
# Required libraries
library(tidyverse)
library(ggalluvial)
library(forcats)
library(stringr)

#Merge and compute contribution
merged_data <- players %>%
  left_join(appearances, by = "player_id") %>%
  filter(!is.na(market_value_in_eur), !is.na(goals), !is.na(assists)) %>%
  mutate(contribution = goals + assists)

#Top 10 clubs and countries
top10_clubs <- players %>%
  filter(!is.na(current_club_name)) %>%
  group_by(current_club_name) %>%
  summarise(squad_value = sum(market_value_in_eur, na.rm = TRUE)) %>%
  arrange(desc(squad_value)) %>%
  slice_head(n = 10) %>%
  pull(current_club_name)

top10_countries <- merged_data %>%
  count(country_of_citizenship, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(country_of_citizenship)

#Filter & assign tiers
alluvial_data <- merged_data %>%
  filter(current_club_name %in% top10_clubs,
         country_of_citizenship %in% top10_countries) %>%
  mutate(
    MarketValueTier = case_when(
      market_value_in_eur >= quantile(market_value_in_eur, 0.66, na.rm = TRUE) ~ "Top",
      market_value_in_eur >= quantile(market_value_in_eur, 0.33, na.rm = TRUE) ~ "Mid",
      TRUE ~ "Low"
    ),
    ContributionTier = case_when(
      ntile(contribution, 3) == 3 ~ "High",
      ntile(contribution, 3) == 2 ~ "Medium",
      TRUE ~ "Low"
    ),
    current_club_name = str_wrap(current_club_name, width = 20),
    country_of_citizenship = str_wrap(country_of_citizenship, width = 15),
    MarketValueTier = factor(MarketValueTier, levels = c("Low", "Mid", "Top")),
    ContributionTier = factor(ContributionTier, levels = c("Low", "Medium", "High"))
  ) %>%
  group_by(current_club_name, country_of_citizenship, MarketValueTier, ContributionTier) %>%
  summarize(n = n(), .groups = "drop")

# Step 4: Plot
ggplot(alluvial_data,
       aes(axis1 = current_club_name,
           axis2 = country_of_citizenship,
           axis3 = MarketValueTier,
           axis4 = ContributionTier,
           y = n)) +
  geom_alluvium(aes(fill = MarketValueTier), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/2, fill = "gray95", color = "gray40") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Top" = "navy", "Mid" = "#4292c6", "Low" = "lightblue")) +
  labs(
    title = "Player Flow: Club → Country → Market Value Tier → Contribution Tier",
    subtitle = "Top 10 Clubs & Countries - FIFA 2026",
    y = "Player Count",
    fill = "Market Value Tier"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold", angle = 25, hjust = 1),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

#bubble plot

library(dplyr)
library(ggplot2)
library(ggrepel)

#Filter and compute per‑appearance contributions
contrib_data <- players %>%
  left_join(appearances, by = "player_id") %>%
  filter(
    !is.na(goals),
    !is.na(assists),
    !is.na(market_value_in_eur),
    !is.na(minutes_played),
    format(as.Date(date), "%Y") == "2025"
  ) %>%
  mutate(contribution = goals + assists)

#Summarize by player (total contributions + minutes)
player_summary <- contrib_data %>%
  group_by(player_id, name, position, market_value_in_eur) %>%
  summarise(
    total_contribution = sum(contribution),
    total_minutes    = sum(minutes_played),
    .groups = "drop"
  )

#Grab the top 20 players (no ties)
top20 <- player_summary %>%
  slice_max(order_by = total_contribution, n = 20, with_ties = FALSE)

#Plot only those 20
ggplot(top20, aes(
  x    = market_value_in_eur,
  y    = total_contribution,
  size = total_minutes,
  color= position
)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = name),
                  size = 3.5,
                  fontface = "bold",
                  max.overlaps = 20) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M €")) +
  scale_size(range = c(3, 12)) +
  labs(
    title    = "Top 20 Players (2025): Market Value vs. Total Goal Contribution",
    subtitle = "Bubble size = Total Minutes Played | Color = Position",
    x        = "Market Value (€)",
    y        = "Goals + Assists",
    size     = "Minutes Played",
    color    = "Position"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title     = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )


ggplot(top20, aes(
  x     = market_value_in_eur,
  y     = total_contribution,
  size  = total_minutes,
  color = total_contribution
)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(aes(label = name),
                  size         = 3.5,
                  fontface     = "bold",
                  max.overlaps = 20) +
  scale_x_continuous(
    labels = scales::dollar_format(scale = 1e-6, suffix = " M €")
  ) +
  scale_size(range = c(3, 12)) +
  scale_color_gradient(
    low  = "lightblue",
    high = "navyblue",
    name = "Goals + Assists"
  ) +
  labs(
    title    = "Top 20 Players (2025): Value vs. Total Goal Contribution",
    subtitle = "Bubble size = Minutes Played | Color = Contribution",
    x        = "Market Value (€)",
    y        = "Goals + Assists",
    size     = "Minutes Played"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )


#side by side bar chart

players$date_of_birth <- as.character(players$date_of_birth)

players$date_of_birth <- gsub(" 0:00", "", as.character(players$date_of_birth))
# Step to calculate age
players <- players %>%
  mutate(age = as.numeric(floor(interval(ymd(date_of_birth), ymd("2026-06-01")) / years(1))))


head(players)


# Correct date parsing using mdy()
players <- players %>%
  mutate(age = as.numeric(floor(interval(mdy(date_of_birth), ymd("2026-06-01")) / years(1))))

compare_goal_contribution_by_age <- function(appearances, players, country1, country2, title) {
  # Merge and calculate age contribution
  merged_data <- appearances %>%
    left_join(players, by = "player_id") %>%
    filter(country_of_citizenship %in% c(country1, country2), !is.na(age)) %>%
    mutate(
      contribution = goals + assists,
      age_group = cut(age, breaks = seq(15, 50, by = 5), right = FALSE),
      side = ifelse(country_of_citizenship == country1, country1, country2)
    ) %>%
    group_by(age_group, side) %>%
    summarize(total_contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
    mutate(total_contribution = ifelse(side == country1, -total_contribution, total_contribution))
  
  # Custom navy blue and light blue colors
  color_palette <- c(country1 = "navyblue", country2 = "lightblue")
  
  # Plot
  ggplot(merged_data, aes(x = total_contribution, y = age_group, fill = side)) +
    geom_col(width = 0.9) +
    scale_fill_manual(values = color_palette) +
    scale_x_continuous(labels = abs) +
    labs(
      title = title,
      x = "Total Goal Contributions (Goals + Assists)",
      y = "Age Group",
      fill = "Country"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

compare_goal_contribution_by_age(appearances, players, "Argentina", "France",
                                 "Argentina vs France: Which Age Bracket Contributes More on the Pitch?")

compare_goal_contribution_by_age(appearances, players, "Spain", "Brazil",
                                 "Spain vs Brazil: Young Guns or Veterans Delivering Goals?")

compare_goal_contribution_by_age(appearances, players, "England", "Germany",
                                 "England vs Germany: Age-wise Goal Contribution Breakdown")


# box plot
#Top 500 Players
top_500 <- players %>%
  filter(!is.na(market_value_in_eur)) %>%
  arrange(desc(market_value_in_eur)) %>%
  slice_head(n = 500)

#Top 10 Countries by Count
top_countries <- top_500 %>%
  count(country_of_citizenship, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  pull(country_of_citizenship)

#Filter & Prepare
top_500_filtered <- top_500 %>%
  filter(country_of_citizenship %in% top_countries)

#Get outliers for labeling
get_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper <- q3 + 1.5 * iqr
  lower <- q1 - 1.5 * iqr
  return(x > upper | x < lower)
}

top_500_filtered <- top_500_filtered %>%
  group_by(country_of_citizenship) %>%
  mutate(is_outlier = get_outliers(market_value_in_eur)) %>%
  ungroup()

#Plot
ggplot(top_500_filtered,
       aes(x = fct_reorder(country_of_citizenship, market_value_in_eur, .fun = median),
           y = market_value_in_eur / 1e6, fill = market_value_in_eur)) +
  
  geom_boxplot(fill= "lightblue", outlier.shape = NA, width = 0.5, color = "lightgrey") +
  
  # Jittered dots
  geom_jitter(aes(color = "Player"), width = 0.15, alpha = 0.4, size = 1.5, show.legend = FALSE) +
  
  # Outlier labels
  geom_text_repel(data = subset(top_500_filtered, is_outlier),
                  aes(label = name),
                  size = 3, max.overlaps = 10, color = "black") +
  
  # Gradient fill for boxplot
  scale_fill_gradient(low = "lightblue", high = "black") +
  
  # Use black for jitter points
  scale_color_manual(values = c("Player" = "navy")) +
  
  labs(
    title = "Distribution of Market Values in Top 10 Countries (Top 500 Players)",
    x = "Country",
    y = "Player Market Value (€ Million)",
    fill = "Value (€)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")



# Area plot
# Create the dataset for all countries
all_yearly_contrib <- appearances %>%
  inner_join(players, by = "player_id") %>%
  mutate(year = as.numeric(format(as.Date(date), "%Y"))) %>%
  filter(year >= 2010, year <= 2024) %>%
  group_by(country_of_citizenship, year) %>%
  summarise(total_contribution = sum(goals + assists, na.rm = TRUE), .groups = "drop")

# Find Top 10 countries overall
top10_countries <- all_yearly_contrib %>%
  group_by(country_of_citizenship) %>%
  summarise(overall_contribution = sum(total_contribution, na.rm = TRUE)) %>%
  arrange(desc(overall_contribution)) %>%
  slice_head(n = 10) %>%
  pull(country_of_citizenship)

# Filter only for Top 10 countries
top10_yearly <- all_yearly_contrib %>%
  filter(country_of_citizenship %in% top10_countries)

#plot
ggplot(top10_yearly, aes(x = year, y = total_contribution, fill = country_of_citizenship)) +
  geom_area(alpha = 0.8, size = 0.5, colour = "white") +
  scale_fill_brewer(palette = "Set3", name = "Country") +   # Nice colorful palette
  labs(
    title = "Top 10 Countries by Attacking Contribution (2010–2024)",
    subtitle = "Total Goals + Assists Over Time",
    x = "Year",
    y = "Total Contributions",
    fill = "Country"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.text = element_text(size = 10)
  )
