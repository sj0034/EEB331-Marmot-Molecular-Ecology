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
    yrborn = na_if(yrborn, "N/A"),
    yrborn = as.numeric(yrborn)
  ) %>%
  filter(!is.na(yrborn), !is.na(sire.x), !is.na(dam.x)) %>%
  distinct(sire.x, dam.x, yrborn) %>%    
  group_by(sire.x, yrborn) %>%
  summarise(
    n_dams = n_distinct(dam.x),        
    .groups = "drop"
  ) %>%
  mutate(
    strategy = ifelse(n_dams > 1, "Polygynous", "Monogamous"),
    strategy_bin = ifelse(strategy == "Polygynous", 1, 0)
  )

#Logistic regression: year as predictor, strategy as response
anova_model <- glm(strategy_bin ~ yrborn,
                   data = sire_level,
                   family = binomial)

anova(anova_model, test = "Chisq")
summary(anova_model)
#Results: yrborn -0.03490    0.04948  -0.705    0.481
#Year cannot significantly predict mating strat (data set too small)


#For valley mating strat by year: Mann–Whitney (Wilcoxon rank-sum) test 
#comparing mating strat within each valley. 

summary_valley_year_small <- colony_types %>%
  filter(!is.na(strategy), !is.na(valley), valley != "Other") %>%
  filter(!is.na(strategy), !is.na(valley)) %>%
  mutate(yrborn = as.numeric(yrborn)) %>%
  filter(!is.na(yrborn)) %>%
  group_by(valley, yrborn, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

valley_mw_small <- summary_valley_year_small %>%
  group_by(valley) %>%
  summarise(
    p_value = if (n_distinct(strategy) > 1) {
      wilcox.test(n_individuals ~ strategy, exact = FALSE)$p.value
    } else {
      NA_real_
    },
    W = if (n_distinct(strategy) > 1) {
      wilcox.test(n_individuals ~ strategy, exact = FALSE)$statistic
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(Significant = ifelse(!is.na(p_value) & p_value < 0.05, "Yes", "No"))

valley_mw_small
#Results: 1 Lower Valley  0.211  2 Upper Valley  0.0485
#significantly more polygynous in both valleys

#################### Marmot Master Data Set ########################

#load data
master_ped <- read.csv("sophie_sires_col.csv", header = TRUE)


#polygynous data set
master_polygynous <- master_ped %>%
  group_by(sire, yob) %>%
  filter(!is.na(sire), !is.na(yob)) %>%
  filter(n_distinct(dam) > 1) %>% #keeps only those with same sire but different dam
  filter(n() > 1) %>% #keeps only sires showing up more than once 
  arrange(sire, yob)


#monogamous data set
master_monogamous <- master_ped %>%
  group_by(sire, yob) %>%
  filter(!is.na(sire), !is.na(yob)) %>%
  filter(n_distinct(dam) == 1) %>%
  arrange(sire, yob)


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
combined_counts <- bind_rows(poly_counts, mono_counts)


#####stats#####

#Master dataset: does year predict mating strategy?
master_level <- master_ped %>%
  filter(!is.na(sire), !is.na(yob), !is.na(dam)) %>%
  distinct(sire, dam, yob) %>%        
  group_by(sire, yob) %>%
  summarise(
    n_dams = n_distinct(dam),
    .groups = "drop"
  ) %>%
  mutate(
    strategy = ifelse(n_dams > 1, "Polygynous", "Monogamous"),
    strategy_bin = ifelse(strategy == "Polygynous", 1, 0)
  )

#Logistic regression
anova_model_master <- glm(strategy_bin ~ yob, data = master_level, family = binomial)

anova(anova_model_master, test = "Chisq")
summary(anova_model_master)
#Results: yob 0.01295    0.01989   0.651    0.515
#Year has no significant effect on mating strategy

###Colony Analysis###
fixed_master_ped <- master_ped %>%
  mutate(col_area = trimws(tolower(col_area)))

#find distinct colony names
col_counts <- fixed_master_ped %>%
  mutate(col_area = trimws(tolower(col_area))) %>%
  distinct(col_area) %>%
  arrange(col_area)

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

#summ by valley instead of individual colony
summary_valley_master <- colony_types_master %>%
  group_by(valley, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

#adding factor of per year instead of over all years
summary_valley_year_master <- colony_types_master %>%
  group_by(valley, yob, strategy) %>%
  summarise(n_individuals = n(), .groups = "drop")

###stats###
library(purrr)
library(stats)
library(tibble)

#Per valley test: in each valley, do strategies differ across years?

valley_mw_results <- summary_valley_year_master %>%
  group_by(valley) %>%
  summarise(
    p_value = if (n_distinct(strategy) > 1) {
      wilcox.test(n_individuals ~ strategy, exact = FALSE)$p.value
    } else {
      NA_real_
    },
    W = if (n_distinct(strategy) > 1) {
      wilcox.test(n_individuals ~ strategy, exact = FALSE)$statistic
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(Significant = ifelse(!is.na(p_value) & p_value < 0.05, "Yes", "No"))

valley_mw_results
#Results: 1 Lower Valley 0.00000355   2 Upper Valley 0.000000779
#Significantly more polygynous in both valleys


######## Mixed data set plotting #######

#add source
df1 <- summary_df %>%
  rename(
    year = yrborn,
    count = n_dams,
    strategy_type = strategy
  ) %>%
  mutate(
    year = as.integer(year), 
    source = "summary_df"
  )

df2 <- combined_counts %>%
  rename(
    year = yob,
    count = count,
    strategy_type = type
  ) %>%
  mutate(
    year = as.integer(year), 
    source = "combined_counts"
  )

combined_df <- bind_rows(df1, df2)

combined_df <- combined_df %>%
  mutate(
    year = as.factor(year),
    count = as.numeric(count),
    strategy_type = as.character(strategy_type)
  )

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

#count unique sires per strategy per year
small_sire_counts <- sire_level %>%
  group_by(yrborn, strategy) %>%
  summarise(n_sires = n_distinct(sire.x), .groups = "drop") %>%
  mutate(
    yrborn = as.integer(yrborn),
    dataset = "genotype_data"
  )

#count unique sires per strategy per year
master_sire_counts <- master_level %>%
  group_by(yob, strategy) %>%
  summarise(n_sires = n_distinct(sire), .groups = "drop") %>%
  rename(yrborn = yob) %>%
  mutate(
    yrborn = as.integer(yrborn),
    dataset = "master_data"
  )

#Merge
df <- bind_rows(small_sire_counts, master_sire_counts) %>%
  mutate(
    fill_group = paste(dataset, strategy, sep = "_"),
    yrborn = as.factor(yrborn)
  )

#Plot
ggplot(df, aes(x = yrborn, y = n_sires, fill = fill_group)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~dataset) +
  scale_fill_manual(values = c(
    "genotype_data_Monogamous" = "#1f77b4",
    "genotype_data_Polygynous" = "#6baed6",
    "master_data_Monogamous" = "#d62728",
    "master_data_Polygynous" = "#fcae91"
  )) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  labs(
    x = "Year",
    y = "Number of Sires by Mating Strategy",
    fill = "Group",
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size = 7))


#mix kin born to mono/poly sires by valley#
summary_valley$dataset <- "genotype_data"
summary_valley_master$dataset <- "master_data"

df <- bind_rows(summary_valley, summary_valley_master)

df <- df %>%
  mutate(fill_group = paste(dataset, strategy, sep = "_"))

ggplot(df, aes(x = valley, y = n_individuals, fill = fill_group)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~dataset) +
  scale_fill_manual(values = c(
    "genotype_data_Monogamous" = "#1f77b4",
    "genotype_data_Polygynous" = "#6baed6",
    "master_data_Monogamous" = "#d62728",
    "master_data_Polygynous" = "#fcae91"
  )) +
  labs(x = "Valley", y = "Number of Individuals by Mating Strategy", fill = "Group")+
  theme_minimal()
