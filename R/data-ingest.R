
library(tidyverse)
library(ggiraph)


advent_appraisals_2024 <- read_csv("./Data/Advent Appraisal.csv") 
 

tias_link <- "https://onedrive.live.com/personal/85b6f23bc6f873c3/_layouts/15/Doc.aspx?sourcedoc=%7B586fda48-c520-4823-9a18-05e43a3f9ffa%7D&action=default&redeem=aHR0cHM6Ly8xZHJ2Lm1zL3gvYy84NWI2ZjIzYmM2Zjg3M2MzL0VVamFiMWdneFNOSW1oZ0Y1RG9fbl9vQlNybERhS0tnc2tQS2R1WmpoU3E3Tmc_ZT00OkFxMndxaiZhdD05&slrid=ca996ba1-9092-a000-bdea-d71ea00d1da2&originalPath=aHR0cHM6Ly8xZHJ2Lm1zL3gvYy84NWI2ZjIzYmM2Zjg3M2MzL0VVamFiMWdneFNOSW1oZ0Y1RG9fbl9vQlNybERhS0tnc2tQS2R1WmpoU3E3Tmc_cnRpbWU9cXNlbmQ3SVgzVWc&CID=d430dea2-2dc5-4f48-9edd-cae0fff67c25&_SRM=0:G:39"


# Gives the highest scores
advent_averages <- advent_appraisals_2024 %>%
 group_by(`Tea Name`, `Biscuit Name`) %>% 
 summarise(
  tea_score = mean(`Tea Score (out of 10)`, na.rm = TRUE), 
  biscuit_score = mean(`Biscuit Score (out of 10)`, na.rm = TRUE),
  pairing_score = mean(`Combination Score (out of 10)`, na.rm = TRUE), 
  .groups = "drop"
 )

 # Tea
 filter(advent_averages, tea_score == max(tea_score, na.rm = TRUE))
 
 # Biscuit
 filter(advent_averages, biscuit_score == max(biscuit_score, na.rm = TRUE))
 
 # Pairing
 filter(advent_averages, pairing_score == max(pairing_score, na.rm = TRUE))


 advent_appraisals_2024 %>%
  pivot_longer(cols = c(`Tea Score (out of 10)`, 
                        `Biscuit Score (out of 10)`, 
                        `Combination Score (out of 10)`), 
               names_to = "edible",
               values_to = "score") %>%
  filter(edible == "Tea Score (out of 10)") %>% 
  ggplot(aes(x = Day, y = score, colour = Reviewer)) +
  geom_line_interactive()
  #geom_smooth(se = FALSE)
 
 
 
 
 
 
 
 
 


