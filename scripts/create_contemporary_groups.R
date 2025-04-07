


###########################

# Load in the datasets

library(dplyr)
ewes <- read.csv("/home/dermot.kelly/Dermot_analysis/Phd/Paper_1/Re-run 2024/data/full_ewes.csv")
growings <- read.csv("/home/dermot.kelly/Dermot_analysis/Phd/Paper_1/Re-run 2024/data/full_growings.csv")


# Function to create contemporary groups based on three columns

create_contemporary_groups <- function(data, date_col, flock_col, run_col, metric_cols, id_col, merge_col, methane_col) {
  
  # Step 1: Initialize lists to hold metric-specific data and summaries
  metric_group_columns <- list()
  metric_summaries <- list()
  
  # Step 2: Process each methane metric independently
  for (metric_col in metric_cols) {
    if (metric_col %in% colnames(data)) {
      
      # Subset the data for rows where the current metric is not NA
      metric_data <- data %>%
        filter(!is.na(!!sym(metric_col))) %>%
        mutate(
          # Create GroupID by concatenating Date, Flock, and Run
          GroupID = paste(!!sym(date_col), !!sym(flock_col), !!sym(run_col), sep = "_"),
          # Create GroupNumber as a numeric ID based on GroupID
          GroupNumber = as.integer(as.factor(GroupID))
        )
      
      # Calculate group-level summary: number of animals in each group
      group_summary <- metric_data %>%
        group_by(GroupID, GroupNumber) %>%
        summarise(
          AnimalCount = n(),  # Number of animals in the group
          .groups = "drop"
        ) %>%
        arrange(desc(AnimalCount))  # Optional: Sort by group size
      
      # Save the group summaries and the total number of groups
      metric_summaries[[metric_col]] <- list(
        group_summary = group_summary,
        total_groups = n_distinct(group_summary$GroupID)
      )
      
      # Save GroupID and GroupNumber with metric-specific column names
      metric_data <- metric_data %>%
        select(!!sym(id_col), !!sym(merge_col), GroupID, GroupNumber, !!sym(methane_col)) %>%
        rename(
          !!paste0(metric_col, "_GroupID") := GroupID,
          !!paste0(metric_col, "_GroupNumber") := GroupNumber
        )
      
      metric_group_columns[[metric_col]] <- metric_data
    } else {
      warning(paste("Metric column", metric_col, "not found in the dataset. Skipping."))
    }
  }
  
  # Step 3: Merge all metric-specific columns back into the original dataset
  for (metric_col in names(metric_group_columns)) {
    data <- data %>%
      left_join(
        metric_group_columns[[metric_col]],
        by = c(id_col, merge_col, methane_col)  # Merge using ANI_ID and date (or other identifiers)
      )
  }
  
  # Step 4: Return the updated dataset and summaries
  results <- list(
    data_with_groups = data,
    metric_summaries = metric_summaries
  )
  
  return(results)
}


# Specify the input and coutput columns for each dataset

ewe_results <- create_contemporary_groups(
  data = ewes, 
  date_col = "date", 
  flock_col = "source", 
  run_col = "lot_no", 
  metric_cols = c("ch4_g_day2_1v3"),
  id_col = "ANI_ID", 
  merge_col = "date",
  methane_col = "ch4_g_day2_1v3"
)


growings_results <- create_contemporary_groups(
  data = growings, 
  date_col = "date", 
  flock_col = "source", 
  run_col = "lot_no", 
  metric_cols = c("ch4_g_day2_1v3"),
  id_col = "ANI_ID", 
  merge_col = "date",
  methane_col = "ch4_g_day2_1v3"
)


# Merge the results back into the original datasets, so to get contemporary groupings and number of animals in each


growing_groups <- growings_results$data_with_groups
growing_counts <- growings_results$metric_summaries$ch4_g_day2_1v3$group_summary

growings <- growing_groups %>%
  left_join(
    growing_counts %>% select(GroupNumber, AnimalCount), 
    by = c("ch4_g_day2_1v3_GroupNumber" = "GroupNumber")
  )




ewe_groups <- ewe_results$data_with_groups
ewe_counts <- ewe_results$metric_summaries$ch4_g_day2_1v3$group_summary


ewes <- ewe_groups %>%
  left_join(
    ewe_counts %>% select(GroupNumber, AnimalCount), 
    by = c("ch4_g_day2_1v3_GroupNumber" = "GroupNumber")
  )



# Save the new datasets with newly added groupings
write.csv(growings, "/home/dermot.kelly/Dermot_analysis/mixed_linear_model/data/growings_with_groups.csv", row.names = F)

write.csv(ewes, "/home/dermot.kelly/Dermot_analysis/mixed_linear_model/data/ewes_with_groups.csv", row.names = F)






