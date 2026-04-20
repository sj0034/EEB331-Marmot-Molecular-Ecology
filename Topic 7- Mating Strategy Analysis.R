install.packages("readxl")
library(readxl)
library(dplyr)
library(tidyr)

setwd("/Users/sj/Desktop/EEB_331") 
list.files()

################Small Data Set#################

#load data
small_ped <- read.csv("radseq_pedigree_metadata.csv", header = TRUE)
ped_parent_ids <- read.csv("marmot_Parentage_Informed_Pedigree.csv", header = TRUE)

####Cleaning####

#make the data sets match so i can merge based on uid

parent_new <- ped_parent_ids %>%
  rename(uid = id) %>%
  mutate(uid = gsub("^UID_", "", uid))

small_ped <- small_ped %>%
  mutate(uid = gsub("^UID_", "", uid))

#match data sets
parent_new <- parent_new %>% mutate(uid = as.character(uid))
small_ped  <- small_ped %>% mutate(uid = as.character(uid))
parent_new <- parent_new %>% mutate(uid = gsub("^UID_", "", uid))
small_ped  <- small_ped %>% mutate(uid = gsub("^UID_", "", uid))
merged_ped <- full_join(parent_new, small_ped, by = "uid")

#clean merged data set
merged_ped_2 <- merged_ped %>%
  mutate(yrborn = na_if(yrborn, "N/A")) %>% 
  filter(!is.na(sire.x), !is.na(yrborn)) %>%
  select(uid, dam = dam.x, sire = sire.x, yrborn, col_area)

#Remove duplicate offspring from same litter and repeated dam/sire pairs in same year
unique_matings <- merged_ped %>%
  distinct(uid, sire.x, dam.x, yrborn)


#so that there's no double counting for pups in the same litter
unique_sire_dam_year <- merged_ped %>%
  filter(!is.na(sire.x), !is.na(dam.x), !is.na(yrborn)) %>%
  distinct(sire.x, dam.x, yrborn)

sire_classification <- unique_sire_dam_year %>%
  group_by(sire.x, yrborn) %>%
  summarise(
    n_dams = n_distinct(dam.x),
    .groups = "drop"
  ) %>%
  mutate(strategy = ifelse(n_dams > 1, "Polygynous", "Monogamous"))

#make counts
polygynous_sire <- sire_classification %>%
  filter(strategy == "Polygynous") %>%
  arrange(sire.x, yrborn)

monogamous_sire <- sire_classification %>%
  filter(strategy == "Monogamous") %>%
  arrange(sire.x, yrborn)


#combine
summary_df <- bind_rows(monogamous_sire, polygynous_sire)


####Plotting Poly/Mono Sire Count####

#plotting poly and mono individuals per year
library(ggplot2)

ggplot(summary_df, aes(x = as.factor(yrborn), y = n_dams, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(
    x = "Year",
    y = "Number of Sires",
    fill = "Mating Strategy",
    title = "Number of Sires by Mating Strategy Across Years"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

####Plotting Poly/Mono Colony Area Count#####
merged_ped_2 <- merged_ped_2 %>% mutate(uid = as.character(uid)) %>% mutate(uid = gsub("^UID_", "", uid))

sire_strategy_again <- merged_ped_2 %>%
  filter(!is.na(sire), !is.na(dam), !is.na(yrborn)) %>%
  distinct(sire, dam, yrborn) %>% #one row per sire/dam/year to avoid double counting pups
  group_by(sire, yrborn) %>%
  summarise(
    n_dams = n_distinct(dam),
    .groups = "drop"
  ) %>%
  mutate(strategy = ifelse(n_dams > 1, "Polygynous", "Monogamous")) %>%
  select(sire, yrborn, strategy)

merged_ped_strategy_again <- merged_ped_2 %>%
  left_join(sire_strategy_again, by = c("sire", "yrborn")) %>%
  filter(!is.na(strategy)) %>% #keep only individuals with known strategy
  distinct(uid, .keep_all = TRUE) #remove duplicates if a pup appears multiple times (like in the same litter)

summary_colony <- merged_ped_strategy_again %>%
  group_by(col_area, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

ggplot(summary_colony, aes(x = col_area, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Colony",
    y = "Number of Individuals",
    fill = "Mating Strategy",
    title = "Offspring Born to Monogamous vs Polygynous Sires by Colony"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Assign colonies to valley groups
colony_types <- merged_ped %>%  #use merged_ped or cleaned dataset
  distinct(dam.x, sire.x, yrborn, col_area, sex) %>%  #unique matings
  mutate(strategy = case_when(
    sire.x %in% polygynous_sire$sire.x & yrborn %in% polygynous_sire$yrborn ~ "Polygynous",
    sire.x %in% monogamous_sire$sire.x & yrborn %in% monogamous_sire$yrborn ~ "Monogamous",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(strategy)) %>%
  mutate(valley = case_when(
    col_area %in% c("northpk", "mm_aspen", "picnic_upper", "picnic_middle", "picnic_lower", 
                    "boulder", "avalanche", "mm_maintalus") ~ "Upper Valley",
    col_area %in% c("gothictown", "river_rivermound", "horsemound", "bench", "river_southmound", "river_sagemound", "river") ~ "Lower Valley",
    TRUE ~ "Other"
  ))

#Sum by valley instead of individual colony
summary_valley <- colony_types %>%
  group_by(valley, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")


ggplot(summary_valley, aes(x = valley, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Valley",
    y = "Number of Individuals",
    fill = "Mating Strategy",
    title = "Offspring Born to Monogamous vs Polygynous Sires by Valley"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )


#add factor of per year instead of over all years
summary_valley_year <- colony_types %>%
  group_by(valley, yrborn, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

ggplot(summary_valley_year, 
       aes(x = yrborn, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ valley) +
  labs(
    x = "Year",
    y = "Proportion of Individuals",
    fill = "Mating Strategy",
    title = "Proportion of Offspring by Mating Strategy Across Years and Valleys"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#######Stats#######
#For Sire Count

#Sire level data set
sire_level <- unique_matings %>%
  mutate(
    yrborn = trimws(yrborn),
    yrborn = na_if(yrborn, "N/A"),
    yrborn = na_if(yrborn, ""),
    yrborn = as.numeric(yrborn) 
  ) %>%
  filter(!is.na(yrborn)) %>%
  group_by(sire.x, yrborn) %>%
  summarise(
    n_dams = n(),
    .groups = "drop"
  ) %>%
  mutate(
    strategy = ifelse(n_dams > 1, "Polygynous", "Monogamous"),
    strategy_num = ifelse(strategy == "Polygynous", 1, 0)
  )

summary_df <- summary_df %>%
  mutate(
    n_dams = as.numeric(trimws(as.character(n_dams))),
    n_dams = ifelse(is.na(n_dams), 0, n_dams)
  )

#combine by year and strategy for unique values
summary_agg <- summary_df %>%
  group_by(yrborn, strategy) %>%
  summarise(n_dams = sum(n_dams), .groups = "drop")

#Pivot wider (had to look this up because code wasn't working)
table_data <- summary_agg %>%
  pivot_wider(
    names_from = strategy,
    values_from = n_dams,
    values_fill = 0
  )

#Convert to numeric matrix (all values numeric, no lists) 
table_data_matrix <- table_data %>%
  select(-yrborn) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

#chi-squared test
chisq.test(table_data_matrix)
#Results: X-squared = 28.191, df = 17, p-value = 0.04276 #Means no significance
#Even if some years “look different in a bar plot 
#the variation is likely due to sampling noise, not a true biological pattern

#visualize proportions
ggplot(summary_agg, aes(x = yrborn, y = n_dams, fill = strategy)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    x = "Year",
    y = "Proportion of Sires",
    fill = "Mating Strategy",
    title = "Proportion of Monogamous vs Polygynous Sires by Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.csv(merged_ped, file = "master_ped2.csv", row.names = FALSE)

#For colony area count

#make summary_valley into a matrix
table_valley <- summary_valley %>%
  pivot_wider(
    names_from = strategy,
    values_from = n_individuals,
    values_fill = 0
  )

#matrix
valley_matrix <- as.matrix(table_valley[, c("Polygynous", "Monogamous")])
rownames(valley_matrix) <- table_valley$valley

#chi-square test
chi_valley <- chisq.test(valley_matrix)
chi_valley

#standardized residuals
chi_valley$stdres

#Results: 
#p-value ≈ 0.06 shows the difference between 
#valleys is marginally non-significant, but the residuals suggest a trend:
#Lower Valley has more polygynous sires
#Upper Valley has more monogamous sires

#For valley mating strat by year

table_year_valley <- colony_types %>%
  group_by(yrborn, valley, strategy) %>%
  summarise(n = n(), .groups = "drop")

table_year_valley_wide <- table_year_valley %>%
  pivot_wider(
    names_from = strategy,
    values_from = n,
    values_fill = 0
  )

valid_chi_results <- chi_results %>%
  filter(!is.na(p_value) & !is.nan(p_value))
valid_chi_results

chi_results_fisher <- data.frame()

for (yr in years) {
  df_year <- table_year_valley_wide[table_year_valley_wide$yrborn == yr, ]
  #skip if only 1 valley
  if (nrow(df_year) > 1) {
    mat <- as.matrix(df_year[, c("Polygynous","Monogamous")])
    rownames(mat) <- df_year$valley
    
    test <- fisher.test(mat)
    chi_results_fisher <- rbind(chi_results_fisher, data.frame(
      yrborn = yr,
      p_value = test$p.value
    ))
  } else {
    chi_results_fisher <- rbind(chi_results_fisher, data.frame(
      yrborn = yr,
      p_value = NA
    ))
  }
}

chi_results_fisher

#Results: 
#Significant difference (p < 0.05): 
#Only 2025 (p = 0.024) shows a statistically significant difference in mating 
#strategy between valleys.
#Not significant (p ≥ 0.05):
#Most years (e.g., 2018, 2023) show no evidence that monogamous vs polygynous 
#sires differ between valleys.

#Table for this
#dominant strategy per valley/year
table_year_valley_wide <- table_year_valley_wide %>%
  mutate(
    dominant_strategy = ifelse(Polygynous > Monogamous, "Polygynous",
    ifelse(Monogamous > Polygynous, "Monogamous", "Tie"))
  )

#Merge Fisher test results
final_table <- table_year_valley_wide %>%
  left_join(chi_results_fisher, by = "yrborn") %>%
  mutate(
    significant = case_when(
      is.na(p_value) ~ NA_character_,
      p_value < 0.05 ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  arrange(yrborn, valley)

final_table

#################### Marmot Master Data Set ########################

#load data
master_ped <- read.csv("sophie_sires_col.csv", header = TRUE)
View(master_ped)

#polygynous data set
master_polygynous <- master_ped %>%
  group_by(sire, yob) %>%
  filter(!is.na(sire), !is.na(yob)) %>%
  filter(n_distinct(dam) > 1) %>% #keeps only those with same sire but different dam
  filter(n() > 1) %>% #keeps only sires showing up more than once 
  arrange(sire, yob)
View(master_polygynous)

#monogamous data set
master_monogamous <- master_ped %>%
  group_by(sire, yob) %>%
  filter(!is.na(sire), !is.na(yob)) %>%
  filter(n_distinct(dam) == 1) %>%
  arrange(sire, yob)
View(master_monogamous)

#organize for plotting
#count individuals per year
poly_counts <- master_polygynous %>%
  group_by(yob) %>%
  summarise(count = n()) %>%
  mutate(type = "Polygynous")

mono_counts <- master_monogamous %>%
  group_by(yob) %>%
  summarise(count = n()) %>%
  mutate(type = "Monogamous")

#combine data sets
combined_counts_master <- bind_rows(poly_counts, mono_counts)
View(combined_counts_master)

#plot poly and mono individuals per year
ggplot(combined_counts_master, aes(x = yob, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Sires by Mating Strategy Across Years",
    x = "Year of Birth (yob)",
    y = "Number of Individuals",
    fill = "Mating System"
  ) +
  theme_minimal()

#####stats#####

#pivot so columns= strategy, rows= year
table_data <- combined_counts %>%
  pivot_wider(names_from = type, values_from = count, values_fill = 0)

#matrix
table_data_matrix <- as.matrix(table_data[, -1])
rownames(table_data_matrix) <- table_data$yob

#run test
chi_res <- chisq.test(table_data_matrix)
chi_res

#Results= X-squared = 177.77, df = 25, p-value < 2.2e-16
#Means There is a significant difference in the number of 
#monogamous vs polygynous sires across years

#find significant years
std_res <- chi_res$stdres
std_res


#visualize
year_props <- combined_counts %>%
  group_by(yob) %>%
  mutate(prop = count / sum(count))

ggplot(year_props, aes(x = yob, y = prop, fill = type)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Year",
    y = "Proportion of Sires",
    fill = "Mating Strategy",
    title = "Proportion of Monogamous vs Polygynous Sires by Year (Master Data)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


fixed_master_ped <- master_ped %>%
  group_by(sire, yob, col_area) %>%
  filter(!is.na(sire), !is.na(yob), !is.na(dam))
View(fixed_master_ped)

###Colony Analysis###

#find distinct colony names
col_counts <- fixed_master_ped %>%
  mutate(col_area = trimws(tolower(col_area))) %>%
  distinct(col_area) %>%
  arrange(col_area)
View(col_counts)

#need to exclude all the col_area's that dont fit into upper/lower valley

#assign colonies to valley groups
allowed_colonies <- c(
  "northpk", "mm_aspen", "picnic_upper", "picnic_middle", "picnic_lower", 
  "boulder", "avalanche", "mm_maintalus",
  "gothictown", "river_rivermound", "horsemound", "bench", 
  "river_southmound", "river_sagemound", "river"
)

colony_types_master <- fixed_master_ped %>%
  distinct(dam, sire, yob, col_area) %>%
  filter(col_area %in% allowed_colonies) %>%
  mutate(strategy = case_when(
    sire %in% master_polygynous$sire & yob %in% master_polygynous$yob ~ "Polygynous",
    sire %in% master_monogamous$sire & yob %in% master_monogamous$yob ~ "Monogamous",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(strategy)) %>%
  mutate(valley = case_when(
    col_area %in% c("northpk", "mm_aspen", "picnic_upper", "picnic_middle", "picnic_lower", 
                    "boulder", "avalanche", "mm_maintalus") ~ "Upper Valley",
    col_area %in% c("gothictown", "river_rivermound", "horsemound", "bench", 
                    "river_southmound", "river_sagemound", "river") ~ "Lower Valley"
  ))

#for "by colony count"
combined_colony_counts_master <- fixed_master_ped %>%
  filter(!is.na(sire), !is.na(yob)) %>%
  group_by(sire, yob) %>%
  mutate(strategy = ifelse(n_distinct(dam) > 1, "Polygynous", "Monogamous")) %>%
  ungroup() %>%
  group_by(yob, col_area, strategy) %>%
  summarise(count = n(), .groups = "drop")

summary_colony_master <- combined_colony_counts_master %>%
  group_by(col_area, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

ggplot(summary_colony_master, aes(x = col_area, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Colony",
    y = "Number of Individuals",
    fill = "Mating Strategy",
    title = "Offspring Born to Monogamous vs Polygynous Sires by Colony (Master Data)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#summ by valley instead of individual colony
summary_valley_master <- colony_types_master %>%
  group_by(valley, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

ggplot(summary_valley_master, aes(x = valley, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Valley",
    y = "Number of Individuals",
    fill = "Mating Strategy",
    title = "Offspring Born to Monogamous vs Polygynous Sires by Valley (Master Data)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

#adding factor of per year instead of over all years
summary_valley_year_master <- colony_types_master %>%
  group_by(valley, yob, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

ggplot(summary_valley_year_master, 
       aes(x = yob, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ valley) +
  labs(
    x = "Year",
    y = "Proportion of Individuals",
    fill = "Mating Strategy",
    title = "Proportion of Offspring by Mating Strategy Across Years and Valleys (Master Data)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

table_data_master %>%
  select(-yob) %>%
  summarise(across(everything(), ~ sum(!grepl("^[0-9]+$", as.character(.)))))

###stats###
library(purrr)
library(stats)
library(tibble)

table_fisher <- summary_colony_master %>%
  pivot_wider(
    names_from = strategy,
    values_from = n_individuals,
    values_fill = 0
  )

fisher_matrix <- as.matrix(table_fisher[, setdiff(names(table_fisher), "col_area")])
rownames(fisher_matrix) <- table_fisher$col_area

fisher_result <- fisher.test(fisher_matrix, simulate.p.value = TRUE, B = 1e5) #B =number of sims

fisher_result
#Result: p-value = 0.01246

##for valley##
summary_valley_year <- colony_types_master %>%
  group_by(yob, valley, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

years <- sort(unique(summary_valley_year$yob))

#Fisher's exact test per year
fisher_per_year <- map_df(years, function(yr) {
  df_year <- summary_valley_year %>% filter(yob == yr)
  table_year <- df_year %>%
    pivot_wider(
      names_from = strategy,
      values_from = n_individuals,
      values_fill = 0
    )
  
  #make sure both strategies exist
  for (col in c("Monogamous", "Polygynous")) {
    if (!(col %in% colnames(table_year))) table_year[[col]] <- 0
  }

  #convert strat columns to numeric
  table_year <- table_year %>%
    mutate(
      Monogamous = as.numeric(Monogamous),
      Polygynous = as.numeric(Polygynous)
    )
  
  #only run test if at least 2 valleys have nonzero counts
  if (nrow(table_year) > 1 && sum(rowSums(table_year[, c("Monogamous","Polygynous")]) > 0) > 1) {
    mat <- as.matrix(table_year[, c("Monogamous", "Polygynous")])
    rownames(mat) <- table_year$valley
    
    test <- fisher.test(mat)
    
    data.frame(
      year = yr,
      p_value = test$p.value,
      Significant = ifelse(test$p.value < 0.05, "Yes", "No")
    )
    
  } else {
    data.frame(
      year = yr,
      p_value = NA,
      Significant = "NA"
    )
  }
})

fisher_per_year

#Results: 
#basically there is no significance between mating strategy and valley in any year

#visualization
#proportion per valley
colony_types_master %>%
  group_by(valley, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop") %>%
  ggplot(aes(x = valley, y = n_individuals, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Valley",
    y = "Number of Individuals",
    fill = "Mating Strategy",
    title = "Offspring Prop by Mating Strategy Across Valleys ("
  ) +
  theme_minimal()


#####
#convert counts to long format
data_long <- table_data_master %>%
  pivot_longer(
    cols = c(Monogamous, Polygynous),
    names_to = "strategy",
    values_to = "count"
  ) %>%
  uncount(count) %>% #expand so each offspring is one row
  mutate(strategy_bin = ifelse(strategy == "Polygynous", 1, 0))

install.packages("Matrix", type = "binary")
library(lme4)

glmm_model <- glmer(
  strategy_bin ~ valley + (1 | yob),
  data = data_long,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

summary(glmm_model)


######## Mixed data set plotting #######

#add source
df1 <- summary_df %>%
  rename(year = yrborn,
         count = n_sires,
         strategy_type = strategy) %>%
  mutate(source = "summary_df")

df2 <- combined_counts %>%
  rename(year = yob,
         count = count,
         strategy_type = type) %>%
  mutate(source = "combined_counts")

#combine
combined_df <- bind_rows(df1, df2)

#plot
ggplot(combined_df, aes(x = as.factor(year), y = count, fill = strategy_type)) +
  geom_bar(aes(alpha = source), stat = "identity", position = position_dodge(width = 0.9)) +
  scale_alpha_manual(values = c(1, 0.5), name = "Dataset Source") +
  labs(
    title = "Number of Sires by Mating Strategy Across Years",
    x = "Year of Birth",
    y = "Count",
    fill = "Mating Strategy"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


###Match data to merge graphs###

#reg counts per year
match_to_small_summary_df <- combined_counts %>%
  rename(n_sires = count, strategy = type, yrborn = yob)
View(match_to_small_summary_df )
View(summary_df)

match_to_small_summary_df <- match_to_small_summary_df %>%
  mutate(yrborn = as.integer(yrborn),
         dataset = "master")

summary_df_new <- summary_df %>%
  mutate(yrborn = as.integer(yrborn),
         dataset = "small")

df <- bind_rows(match_to_small_summary_df, summary_df)

df <- df %>%
  mutate(fill_group = paste(dataset, strategy, sep = "_"))

df$yrborn <- as.factor(df$yrborn)

ggplot(df, aes(x = yrborn, y = n_sires, fill = fill_group)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~dataset) +
  scale_fill_manual(values = c(
    "small_Monogamous" = "#1f77b4",
    "small_Polygynous" = "#6baed6",
    "master_Monogamous" = "#d62728",
    "master_Polygynous" = "#fcae91"
  )) +
  labs(x = "Year", y = "Number of Individuals", fill = "Group", title = "Number of Sires by Mating Strategy Across Years") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size = 7))

#prop of mono/poly sires by year


#mix kin born to mono/poly sires by valley#
summary_valley$dataset <- "small"
summary_valley_master$dataset <- "master"

df <- bind_rows(summary_valley, summary_valley_master)

df <- df %>%
  mutate(fill_group = paste(dataset, strategy, sep = "_"))

ggplot(df, aes(x = valley, y = n_individuals, fill = fill_group)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~dataset) +
  scale_fill_manual(values = c(
    "small_Monogamous" = "#1f77b4",
    "small_Polygynous" = "#6baed6",
    "master_Monogamous" = "#d62728",
    "master_Polygynous" = "#fcae91"
  )) +
  labs(x = "Valley", y = "Number of Individuals", fill = "Group", title = "Sire Prop by Mating Strategy Across Valleys") +
  theme_minimal()


#mix kin born to mono/poly sires by colony
summary_colony_combined <- summary_colony_combined %>%
  mutate(strategy_fill = paste(source, strategy, sep = "_")) %>%
  mutate(strategy_fill = factor(strategy_fill, levels = c(
    "Small_Monogamous", "Small_Polygynous",
    "Master_Monogamous", "Master_Polygynous"
  )))

#plot as stacked bars
ggplot(summary_colony_combined, aes(x = col_area, y = n_individuals, fill = strategy_fill)) +
  geom_bar(stat = "identity") +
  facet_wrap(~source) + 
  scale_fill_manual(values = c(
    "Small_Monogamous"   = "#1f77b4",
    "Small_Polygynous"   = "#6baed6",
    "Master_Monogamous"  = "#d62728",
    "Master_Polygynous"  = "#fcae91"
  )) +
  labs(
    x = "Colony",
    y = "Number of Individuals",
    fill = "Group",
    title = "Monogamous vs Polygynous Sires by Colony"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  

