library(tidyverse)
library(readxl)

setwd("Emory_Projects/Anthony_Mendel/")

###### Read and Process Responses ######
times = readRDS("Data/Final_Data/adjusted_times.RDS")

wilcox.test(times %>% filter(arm == "human_alone") %>% arrange(patient_id) %>% pull(minutes), 
            times %>% filter(arm == "augmented") %>% arrange(patient_id) %>% pull(minutes),
            paired = T)

times %>% group_by(arm) %>% 
    summarize(N = n(), Mean = mean(minutes), sd = sd(minutes), 
              Median = median(minutes), Lower_quartile = quantile(minutes, 0.25), 
              Upper_quartile = quantile(minutes, 0.75), Range = max(minutes) - min(minutes))

ggplot(times) + 
  geom_density(aes(x = minutes, color = arm)) + 
  theme_bw() + 
  labs(x = "Minutes", y = "Density") +
  scale_color_discrete(name = "Arm", labels = c("Human+AI", "Human-alone"))

ggplot(times %>% mutate(arm = case_when(arm == "augmented" ~ "Human+AI", 
                                        arm == "human_alone" ~ "Human-alone"))) +
  geom_boxplot(aes(x = arm, y = minutes, fill = arm)) +
  ggtitle("Chart Timeliness") + theme_minimal() +
  ylab("Minutes") + xlab("Arm") + labs(fill = "Review Group") +
  scale_fill_manual(values = c("Human+AI" = "mediumpurple3", "Human-alone" = "red3"))
