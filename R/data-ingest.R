
library(tidyverse)
library(janitor)
library(plotly)
library(flexdashboard)

advent_appraisals_2024 <- read_csv("./Data/Advent Appraisal.csv")  %>% 
 clean_names()

pair_cols <- unique(advent_appraisals_2024$reviewer)
 
# Gives the highest scores ####
advent_averages <- advent_appraisals_2024 %>%
 group_by(tea_name, biscuit_name) %>% 
 summarise(
  tea_score = mean(tea_score_out_of_10, na.rm = TRUE), 
  biscuit_score = mean(biscuit_score_out_of_10 , na.rm = TRUE),
  pairing_score = mean(combination_score_out_of_10, na.rm = TRUE), 
  .groups = "drop"
 )

 # Tea
 filter(advent_averages, tea_score == max(tea_score, na.rm = TRUE))
 
 # Biscuit
 filter(advent_averages, biscuit_score == max(biscuit_score, na.rm = TRUE))
 
 # Pairing
 filter(advent_averages, pairing_score == max(pairing_score, na.rm = TRUE))

 # Cleaned data
 advent_appraisals_2024_clean <- advent_appraisals_2024 %>%
  pivot_longer(cols = c(tea_score_out_of_10, 
                        biscuit_score_out_of_10, 
                        combination_score_out_of_10), 
               names_to = "edible",
               values_to = "rating") %>%
  group_by(reviewer) %>%
  mutate(tooltip = paste0(reviewer, ": ", rating), 
         max_score = if_else(max(rating) == rating,
                             paste0(reviewer, "'s Best"), NA)) 
 
 # Tea ####
 
 gg_tea <- advent_appraisals_2024_clean %>%
  filter(edible == "tea_score_out_of_10") %>%
  rename(Reviewer = reviewer) %>% 
  ggplot(aes(x = day, y = rating, colour = Reviewer)) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  theme_minimal()
 
 tea_plot <- ggplotly(gg_tea) %>%
  add_text(x = ~day, y = ~rating, showlegend = FALSE, legendgroup = ~Reviewer,
           text = ~max_score, textposition = "topright")
 
tea_scoresheet <- advent_appraisals_2024 %>% 
  group_by(day) %>% 
  select(day, reviewer, tea_score_out_of_10)

tea_closest_score <- left_join(tea_scoresheet, 
            tea_scoresheet %>%  
             pivot_wider(names_from = reviewer, values_from = tea_score_out_of_10),
            by = "day") %>% 
  pivot_longer(names_to = "pair_name", values_to = "pair_score", cols = all_of(pair_cols)) %>% 
  filter(reviewer != pair_name) %>% 
  mutate(score_diff = abs(tea_score_out_of_10 - pair_score)) %>% 
  group_by(reviewer, pair_name) %>% 
  summarise(
   total_abs_difference = sum(score_diff, na.rm = TRUE), .groups = "drop"
  )
 
gg_tea_close_score <- tea_closest_score %>% 
  rename(Reviewer = reviewer, 
         "Score difference" = total_abs_difference, 
         Comparator = pair_name) %>% 
  ggplot(aes(x = Reviewer, y = `Score difference`, fill = Comparator)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(title = "Comparator")) +
  ggtitle("Similar Scorers \n<span style='font-size:10pt'>Those with smaller areas scored the closest to you</span>") +
  xlab("") +
  ylab("score difference")

 tea_close_score_plot <- ggplotly(gg_tea_close_score)
 
 tea_close_score_plot %>% 
  layout(hovermode = "y unified")

 # Biscuit ####
 
 gg_biscuit <- advent_appraisals_2024_clean %>%
  filter(edible == "biscuit_score_out_of_10") %>%
  rename(Reviewer = reviewer) %>% 
  ggplot(aes(x = day, y = rating, colour = Reviewer)) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  theme_minimal()
 
 biscuit_plot <- ggplotly(gg_biscuit) %>%
  add_text(x = ~day, y = ~rating, showlegend = FALSE, legendgroup = 1,
           text = ~max_score, textposition = "topright")
 
 for (i in seq_along(pair_cols)){
  biscuit_plot$x$data[[i]]$legendgroup <- 1
 }
 
 biscuit_scoresheet <- advent_appraisals_2024 %>% 
  group_by(day) %>% 
  select(day, reviewer, biscuit_score_out_of_10)
 
 biscuit_closest_score <- left_join(biscuit_scoresheet, 
                                    biscuit_scoresheet %>%  
                                 pivot_wider(names_from = reviewer, values_from = biscuit_score_out_of_10),
                                by = "day") %>% 
  pivot_longer(names_to = "pair_name", values_to = "pair_score", cols = all_of(pair_cols)) %>% 
  filter(reviewer != pair_name) %>% 
  mutate(score_diff = abs(biscuit_score_out_of_10 - pair_score)) %>% 
  group_by(reviewer, pair_name) %>% 
  summarise(
   total_abs_difference = sum(score_diff, na.rm = TRUE), .groups = "drop"
  )
 
 gg_biscuit_close_score <- biscuit_closest_score %>% 
  rename(Reviewer = reviewer, 
         "Score difference" = total_abs_difference, 
         Comparator = pair_name) %>% 
  ggplot(aes(x = Reviewer, y = `Score difference`, fill = Comparator)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(title = "Comparator")) +
  ggtitle("Similar Scorers \n<span style='font-size:10pt'>Those with smaller areas scored the closest to you</span>") +
  xlab("") +
  ylab("score difference")
 
 biscuit_close_score_plot <- ggplotly(gg_biscuit_close_score)
 
 
 # Pairing ####
 
 gg_combination <- advent_appraisals_2024_clean %>%
  filter(edible == "combination_score_out_of_10") %>%
  rename(Reviewer = reviewer) %>% 
  ggplot(aes(x = day, y = rating, colour = Reviewer)) +
  geom_line(show.legend = FALSE) +
  geom_point() +
  theme_minimal()
 
 combination_plot <- ggplotly(gg_combination) %>%
  add_text(x = ~day, y = ~rating, showlegend = FALSE, legendgroup = 1,
           text = ~max_score, textposition = "topright")
 
 for (i in seq_along(pair_cols)){
  combination_plot$x$data[[i]]$legendgroup <- 1
 }
 
 combination_scoresheet <- advent_appraisals_2024 %>% 
  group_by(day) %>% 
  select(day, reviewer, combination_score_out_of_10)

 combination_closest_score <- left_join(combination_scoresheet, 
                                combination_scoresheet %>%  
                                 pivot_wider(names_from = reviewer, values_from = combination_score_out_of_10),
                                by = "day") %>% 
  pivot_longer(names_to = "pair_name", values_to = "pair_score", cols = all_of(pair_cols)) %>% 
  filter(reviewer != pair_name) %>% 
  mutate(score_diff = abs(combination_score_out_of_10 - pair_score)) %>% 
  group_by(reviewer, pair_name) %>% 
  summarise(
   total_abs_difference = sum(score_diff, na.rm = TRUE), .groups = "drop"
  )
 
 gg_combination_close_score <- combination_closest_score %>% 
  rename(Reviewer = reviewer, 
         "Score difference" = total_abs_difference, 
         Comparator = pair_name) %>% 
  ggplot(aes(x = Reviewer, y = `Score difference`, fill = Comparator)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(title = "Comparator")) +
  ggtitle("Similar Scorers \n<span style='font-size:10pt'>Those with smaller areas scored the closest to you</span>") +
  xlab("") +
  ylab("score difference")
 
 combination_close_score_plot <- ggplotly(gg_combination_close_score)
 