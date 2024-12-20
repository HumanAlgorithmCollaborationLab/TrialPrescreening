library(tidyverse)
library(readxl)

###### Read and Process Timeliness Data ######

# Filter out charts opened in the wrong review configuration (incorrect duplicates)
carbon_times_v2 = read_excel("Data/Final_Data/Carbon_Time_Report_v2.xlsx") %>% filter(is.na(`wrong config?`))
 
# Load worklog data containing number of documents:
worklog = read_excel("Data/UPENN_Mendel_Research_Worklog_Excel.xlsx", sheet = 1) %>% 
  rename(patient_id = `PT IDs`, num_docs = `# of docs`) %>%
  select(c(patient_id, num_docs))

# Prepare data for analysis
  # Create order number for charts for each reviewer
  # assign to human alone or human + AI arm for matching to accuracy data
carbon_times <-  carbon_times_v2 %>%
        group_by(user_name) %>%
        arrange(user_name, started_date, started_at) %>%
        mutate(order_num = row_number(),
               arm = if_else(grepl("AI", config_name), "augmented", "human_alone"),
               cancer_type = if_else(grepl("Lung", config_name), "NSCLC", "CrCa")) %>%
        select(c(user_name, arm, patient_id, cancer_type, duration_in_seconds, order_num)) %>%
        left_join(worklog, by = "patient_id")

# Convert seconds to minutes
carbon_times$minutes = carbon_times$duration_in_seconds/60

# Patient had duplicate augmented arm charts
carbon_times$arm[carbon_times$patient_id == "pt-01h9p699fmwkq58dg377g34q20" &
                   carbon_times$user_name == "Brenda Laventure"] = "human_alone"

###### Substitute Harvest Times for Carbon Times as noted on Worklog ######
liz_harvest = data.frame(patient_id = c("pt-01h9p699ssmzhp2kqxec4cb6n0", "pt-01h9p699sq26kgzvwmt8p0ecxd", 
                                        "pt-01h9p699n3bvg975q951qfn2zc", "pt-01h9p699g0hydn66fdnxehhn72", 
                                        "pt-01h9p699bzekpr7ee1cxygpc8z", "pt-01h9p699dvp725q786akjz84gb",
                                        "pt-01h9p699q3rzvkph1y30bjdy4x", "pt-01h9p6995sb3z5ctsqc6bfndqr",
                                        "pt-01h9p699kb9z00fep20ecmavre", "pt-01h9p6999ed71awe4dx39pyqc6",
                                        "pt-01h9p699kvx7wqpxvf7h6ct373", "pt-01h9p6997em1y6934n0p88ax7v"), 
                         minutes = c(47, 47, 40, 35, 89, 31, 4, 31, 67, 27, 53, 51))

brenda_harvest =  data.frame(patient_id = c("pt-01h9p699p13rxcn9nsz2e842ca", "pt-01h9p699dc8ykr2hcjm4y7g6b6", 
                                            "pt-01h9p699t1jq4mk5nvmt60zntt", "pt-01h9p6997byrmtc3fexgnynpjd", 
                                            "pt-01h9p699sn588n42828gtg2gay"), 
                             minutes = c(43, 61, 35, 33, 64))

times_harvest_adjust = carbon_times

for (i in 1:nrow(liz_harvest)){
  match_row <- which(times_harvest_adjust$patient_id == liz_harvest$patient_id[i] & 
                       times_harvest_adjust$user_name == "Elizabeth Beothy")
  
  # Update minutes in df2 if there's a match
  if (length(match_row) > 0) {
    times_harvest_adjust$minutes[match_row] <- liz_harvest$minutes[i]
  }
  
}

for (i in 1:nrow(brenda_harvest)){
  match_row <- which(times_harvest_adjust$patient_id == brenda_harvest$patient_id[i] & 
                       times_harvest_adjust$user_name == "Brenda Laventure")
  
  # Update minutes in df2 if there's a match
  if (length(match_row) > 0) {
    times_harvest_adjust$minutes[match_row] <- brenda_harvest$minutes[i]
  }
  
}

###### Read and Process Accuracy Data ######
human = read_excel("Data/Final_Data/Human_Alone_Final.xlsx",
                   .name_repair = "unique_quiet")
aug = read_excel("Data/Final_Data/Human_AI_Final.xlsx",
                 .name_repair = "unique_quiet")
resolve = read_excel("Data/Final_Data/Resolve_Final.xlsx",
                     .name_repair = "unique_quiet")

# Accuracy Classification
human$accurate = ifelse(human$Match == "IDENTICAL",1,0)
aug$accurate = ifelse(aug$Match == "IDENTICAL",1,0)
resolve$accurate = ifelse(resolve$Match == "IDENTICAL",1,0)

human$accurate_secondary = ifelse(human$Match == "IDENTICAL" | human$Match == "PARTIAL_SUB_TYPE",1,0)
aug$accurate_secondary = ifelse(aug$Match == "IDENTICAL" | aug$Match == "PARTIAL_SUB_TYPE",1,0)
resolve$accurate_secondary = ifelse(resolve$Match == "IDENTICAL" | resolve$Match == "PARTIAL_SUB_TYPE",1,0)

# Filter Out Missing in First Match Types (IGNORE already filtered out)
human = human %>% filter(Match != "MISSING_IN_FIRST") %>% arrange(`Patient ID`, `Gold Event ID`, `Event Detail`)
aug = aug %>% filter(Match != "MISSING_IN_FIRST") %>% arrange(`Patient ID`, `Gold Event ID`, `Event Detail`)
resolve = resolve %>% filter(Match != "MISSING_IN_FIRST") %>% arrange(`Patient ID`, `Gold Event ID`, `Event Detail`)

# Include indicator of treatment arm for matching to time data
human$arm = "human_alone"
aug$arm = "augmented"

# Filter out the 1 patient who should have been rejected
human = human %>% filter(`Patient ID` != "pt-01h9p6998ny2qzeevaevd3pgwf")
aug = aug %>% filter(`Patient ID` != "pt-01h9p6998ny2qzeevaevd3pgwf")
resolve = resolve %>% filter(`Patient ID` != "pt-01h9p6998ny2qzeevaevd3pgwf")


#saveRDS(times_harvest_adjust, "Data/Final_Data/adjusted_times.RDS")
#saveRDS(human, "Data/Final_Data/human_analytic.RDS")
#saveRDS(aug, "Data/Final_Data/aug_analytic.RDS")
#saveRDS(resolve, "Data/Final_Data/resolve_analytic.RDS")




