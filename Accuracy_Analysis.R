library(tidyverse)
library(readxl)

###### Read Data ######

setwd("Emory_Projects/Anthony_Mendel/")

human = readRDS("Data/Final_Data/human_analytic.RDS")
aug = readRDS("Data/Final_Data/aug_analytic.RDS")
resolve = readRDS("Data/Final_Data/resolve_analytic.RDS")

#########################################################
################### Accuracy Analysis ###################
#########################################################

################### Chart Level Accuracy ################### 
human_chart_level = human %>% group_by(`Patient ID`) %>%
  summarize(chart_accuracy = sum(accurate)/n())

aug_chart_level = aug %>% group_by(`Patient ID`) %>%
  summarize(chart_accuracy = sum(accurate)/n())

resolve_chart_level = resolve %>% group_by(`Patient ID`) %>%
  summarize(chart_accuracy = sum(accurate)/n())

# Chart-Level Accuracy Plots
ggplot() + geom_boxplot(aes(x = review_group, y = 100*chart_accuracy, fill = review_group),
     data = rbind(human_chart_level %>% mutate(review_group = "Human-alone"),
                  aug_chart_level %>% mutate(review_group = "Human+AI"),
                  resolve_chart_level %>% mutate(review_group = "AI-alone"))) +
  ggtitle("Accuracy Distribution Between Review Groups") + theme_minimal() +
  ylab("Chart Accuracy (%)") + xlab("") + labs(fill = "Review Group") +
  geom_segment(aes(x = 2, xend = 3, y = 104, yend = 104)) +
  geom_segment(aes(x = 2, xend = 2, y = 102, yend = 104)) +
  geom_segment(aes(x = 3, xend = 3, y = 102, yend = 104)) +
  annotate("text",x=2.5,y=108,label="***") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_fill_manual(values = c("Human+AI" = "mediumpurple3", "Human-alone" = "red3",
                               "AI-alone" = "skyblue2"))

###################  Criteria-Level Accuracy ###################

human_criteria_level = human %>% group_by(`Event Detail`) %>%
  summarize(event_type_N = n(), event_type_accuracy = sum(accurate)/n(),
            accurate_N = sum(accurate), innacurate_N = event_type_N - accurate_N)

augmented_criteria_level = aug %>% group_by(`Event Detail`) %>%
  summarize(event_type_N = n(), event_type_accuracy = sum(accurate)/n(),
            accurate_N = sum(accurate), innacurate_N = event_type_N - accurate_N)

resolve_criteria_level = resolve %>% group_by(`Event Detail`) %>%
  summarize(event_type_N = n(), event_type_accuracy = sum(accurate)/n(),
            accurate_N = sum(accurate), innacurate_N = event_type_N - accurate_N)

criteria_frequency = human %>% 
    group_by(`Patient ID`, `Event Detail`) %>%
    summarize(event_type_N = n()) %>%
    mutate(present = ifelse(event_type_N > 0, 1, 0)) %>% 
    dplyr::select(-event_type_N) %>%
    pivot_wider(names_from = `Event Detail`, 
                values_from = present,
                values_fill = 0) %>% ungroup()
  
round(colSums(criteria_frequency %>% dplyr::select(-`Patient ID`))*100/nrow(criteria_frequency),1)

##########################################################
################### Hypothesis Testing ###################
##########################################################

# Paired t-test for non inferiority at chart level
t.test(aug_chart_level$chart_accuracy, human_chart_level$chart_accuracy, mu = -0.05, paired = TRUE, alternative = "greater")
# Paired t-test for SUPERIORITY at chart level
t.test(aug_chart_level$chart_accuracy, human_chart_level$chart_accuracy, paired = TRUE, alternative = "greater")

# Tests for Criteria-Level differences, adjusted with Bonferroni correction
for(i in 1:nrow(human_criteria_level)){
  hv = human %>% filter(`Event Detail` == human_criteria_level$`Event Detail`[i])
  av = aug %>% filter(`Event Detail` == human_criteria_level$`Event Detail`[i])
  # Two-sided Test
  inf.test = t.test(av$accurate, hv$accurate, paired = TRUE)
  print(paste(human_criteria_level$`Event Detail`[i], round(inf.test$p.value,4)))
  if(inf.test$p.value <= 0.05/nrow(human_criteria_level)){
    print("SIGNIFICANT DIFFERENCE UNDER BONFERONNI CORRECTION")
    # One-sided Test (Superiority of Human+AI)
    sup.test = t.test(av$accurate, hv$accurate, paired = TRUE, alternative = "greater")
    if (sup.test$p.value <= 0.05/nrow(human_criteria_level)){
      print("SUPERIOR UNDER BONFERONNI CORRECTION")
    }
  }
}
