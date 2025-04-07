

library(tidyr)
library(lme4)
library(lmerTest)
#install.packages("performance")
library(performance)

growings <- read.csv("/home/dermot.kelly/Dermot_analysis/mixed_linear_model/data/growings_with_groups.csv")

ewes <- read.csv("/home/dermot.kelly/Dermot_analysis/mixed_linear_model/data/ewes_with_groups.csv")



# Function to filter data for the mixed linear model

filter_groups <- function(data, group_var, filter_vars, min_group_size, max_group_size = Inf, breed_vars = NULL, min_breed_count = 5, id_var = "ANI_ID") {
  # Step 1: Filter out rows with missing values in the specified variables
  filtered_data <- data %>%
    filter(if_all(all_of(filter_vars), ~ !is.na(.)))
  
  # Step 2: Identify breeds meeting the minimum count threshold
  if (!is.null(breed_vars)) {
    breed_counts <- filtered_data %>%
      summarise(across(all_of(breed_vars), ~ sum(. > 0), .names = "count_{.col}")) %>%
      pivot_longer(everything(), names_to = "breed", values_to = "count") %>%
      mutate(breed = gsub("count_", "", breed)) %>%
      filter(count >= min_breed_count)
    
    valid_breeds <- breed_counts$breed
    
    # Filter data to include only rows with full proportions for valid breeds
    filtered_data <- filtered_data %>%
      select(all_of(c(id_var, filter_vars, group_var, valid_breeds))) %>%  # Retain `ANI_ID`
      rowwise() %>%
      mutate(total_proportion = sum(c_across(all_of(valid_breeds)), na.rm = TRUE)) %>%
      filter(total_proportion == 1) %>%
      ungroup() %>%
      select(-total_proportion)
  }
  
  # Step 3: Filter groups with enough valid rows
  filtered_data <- filtered_data %>%
    group_by(across(all_of(group_var))) %>%
    filter(n() >= min_group_size & n() <= max_group_size) %>%
    ungroup()
  
  return(list(filtered_data = filtered_data, valid_breeds = valid_breeds))
}



#####################################

### LW Growings

#####################################

# Identifies group column and breed prportion columns
# Keeps only rows with values for the specified variables
# Allows only groups with between 5 - 12 animals
# Allows only breeds that are present in at least 100 animals

result_ewes_LW <- filter_groups(
  data = ewes, 
  group_var = "ch4_g_day2_1v3_GroupNumber", 
  filter_vars = c("ch4_g_day2_1v3", "weight", "Age_in_months", "lactation_status", "het", "rec"), 
  min_group_size = 5,
  max_group_size = 12,
  breed_vars = c("MGS", "SU", "CL", "VN", "TX", "IF", "BR", "UN", "RL", "BM", "DT", "CV", "BL", 
                 "BX", "JO", "LY", "BC", "BN", "EC", "WS", "HD", "CM", "SH", "GL", "CO", "CA", 
                 "PR", "HL", "ZB", "BT", "MF", "DK", "RM", "NW", "NF", "CD", "OD", "RY", "DS", 
                 "RO", "VB", "BO", "KB", "LK", "MC", "NC", "PE", "SD"), 
  min_breed_count = 100,
  id_var = "ANI_ID"
)

# Inspects valid breeds
valid_beeds_ewes_LW <- result_ewes_LW$valid_breeds

# saves the data for this model
filtered_ewes_LW <- result_ewes_LW$filtered_data




# Runs the MLM
model_LW_ewes <- lmer(ch4_g_day2_1v3 ~ weight + Age_in_months + het + rec +
                        TX + UN + VN + CV + LY + BR + CL + lactation_status +
                        (1 | ch4_g_day2_1v3_GroupNumber) + 
                        (1 | ANI_ID),
                      data = filtered_ewes_LW
)


summary(model_LW_ewes)
