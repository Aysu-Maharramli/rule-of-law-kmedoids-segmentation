
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)      # For daisy (Gower) and pam (K-Medoids)
library(randomForest)
library(viridis)

# Load data
data <- readxl::read_excel("/Users/mehmet/Desktop/427/OmnibusW1_November2025_UOFI.STAT427.xlsx")

# Define Variable Groups
survey_qs <- c("CaseId","NoOneAbove","Obey","Enforce","EveryoneObey","WayToFix","Prospective",
               "Clear","Consistent","Possible","Stable","AsWritten","Knowable",
               "SeparationPowers","RightsOfAccused","AccessToLawyers","JudicialIndependence",
               "AccessToCourts","Respect","FairProcedures","Voice","NoProtest","Property",
               "WithoutCourt","CrimeControl","NoSelfDealing","EconGrowth","Contracts",
               "HumanRights","Majority","InnocentUntilProve","TreatsEqual","DisputesInCourt",
               "TrustAuthorities","Military","RightToVote","WealthGap")

demo_vars <- c(
  "Sued","BeenSued","CrimDef","Juror","Divorce","Bankruptcy","Lawyer","LawStudent",
  "NoContact","Contact_DK","Contact_SKP","Contact_REF",
  "PartyID7","IDEO","NEWSCONS","NewsFreq","duration_UOFI",
  "SURV_MODE","SURV_LANG","Device","SEX","AGE7","RACETHNICITY","EDUC5",
  "MARITAL","EMPLOY","INCOME4","REGION4","METRO",
  "INTERNET","HOUSING","HOME_TYPE","PHONESERVICE","HHSIZE", "HeardOf"
)


# preprocessing
analysis_df <- data %>%
  select(all_of(c(survey_qs, demo_vars))) %>%
  mutate(duration_UOFI = as.numeric(duration_UOFI)) %>%
  filter(!is.na(duration_UOFI) & duration_UOFI >= 2.0)

cat("Respondents after duration filtering (Total Sample):", nrow(analysis_df), "\n")

survey_cols_only <- setdiff(survey_qs, "CaseId")

clustering_data_mapped <- analysis_df %>%
  select(all_of(survey_cols_only)) %>%
  mutate(across(everything(), ~ case_when(
    . == 1 ~ 0.0,
    . == 2 ~ 1.0,
    . == 4 ~ 2.0,
    . == 5 ~ 3.0,
    TRUE   ~ NA_real_ 
  )))


keep_idx <- rowSums(!is.na(clustering_data_mapped)) > 0
clustering_data_mapped <- clustering_data_mapped[keep_idx, ]
analysis_df <- analysis_df[keep_idx, ] 

dist_matrix <- daisy(clustering_data_mapped, metric = "gower")

# Step D: Fill NA distances with 1.0 (Teammate's Logic)
dist_matrix_mat <- as.matrix(dist_matrix)
dist_matrix_mat[is.na(dist_matrix_mat)] <- 1.0
dist_matrix <- as.dist(dist_matrix_mat)


# K-MEDOIDS MODELING
set.seed(10) 
diss_scores <- sapply(1:10, function(k) {
  if (k == 1) return(sum(as.matrix(dist_matrix)) / (2 * nrow(clustering_data_mapped)))
  pam(dist_matrix, k = k, diss = TRUE)$objective[1]
})

plot(1:10, diss_scores, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Dissimilarity",
     main = "K-Medoids Elbow Plot (Full Sample)")


K_CHOSEN <- 3 #from elbow
pam_res <- pam(dist_matrix, k = K_CHOSEN, diss = TRUE)

respondent_clustered_full <- analysis_df %>%
  mutate(Cluster = factor(pam_res$clustering))


# cluster summary and gap analysis

# identify questions with the highest variance between clusters
cluster_comparison <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(across(all_of(survey_cols_only), mean, na.rm = TRUE)) %>%
  pivot_longer(-Cluster, names_to = "Question", values_to = "MeanScore") %>%
  pivot_wider(names_from = Cluster, names_prefix = "C", values_from = MeanScore) %>%
  mutate(Max_Diff = pmax(abs(C1-C2), abs(C1-C3), abs(C2-C3))) %>%
  arrange(desc(Max_Diff))

print("Top distinguishing questions (Full Sample):")
print(as.data.frame(cluster_comparison))

####

# ccalculate the mean for every survey question by cluster
cluster_comparison_c2_c3 <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(across(all_of(survey_qs[-1]), mean, na.rm = TRUE)) %>%
  
  # reshape the data so clusters are columns (C1, C2, C3)
  pivot_longer(-Cluster, names_to = "Question", values_to = "MeanScore") %>%
  pivot_wider(names_from = Cluster, names_prefix = "C", values_from = MeanScore) %>%
  
  # calculate the absolute difference specifically between Cluster 2 and Cluster 3
  mutate(Diff_C2_C3 = abs(C2 - C3)) %>% 
  
  # rank in descending order to see the biggest disagreements at the top
  arrange(desc(Diff_C2_C3))

print("Questions where Cluster 2 and Cluster 3 diverge the most:")
print(as.data.frame(cluster_comparison_c2_c3))

# View(cluster_comparison_c2_c3)
###


# RF (TOP 15 VARS) this is from the old workflow which is decided to not include in final pipeline


top_15_vars <- c("duration_UOFI", "EDUC5", "NewsFreq", "INCOME4", "EMPLOY", 
                 "PHONESERVICE", "PartyID7", "Device", "IDEO", "RACETHNICITY", 
                 "SURV_MODE", "REGION4", "SEX", "NEWSCONS", "AGE7")

train_df <- respondent_clustered_full %>%
  select(all_of(top_15_vars), Cluster) %>%
  mutate(across(where(is.character), as.factor))

set.seed(42)
rf_model <- randomForest(Cluster ~ ., data = train_df, ntree = 500, importance = TRUE)
varImpPlot(rf_model)


# PROFILING HEATMAP

survey_profile <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(across(all_of(survey_cols_only), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = -Cluster, names_to = "Question", values_to = "Mean_Score")

ggplot(survey_profile, aes(x = Cluster, y = Question, fill = Mean_Score)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma", name = "Avg Score") +
  labs(title = "Survey Patterns (Full Sample - (people who heard of and not heard of)") +
  theme_minimal() + theme(axis.text.y = element_text(size = 7))

# persona table: see if anything interesting can be found in characteristics

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

get_dominance <- function(x) {
  tab <- table(x)
  if(length(tab) == 0) return(NA)
  max(tab) / sum(tab) * 100
}

num_vars <- c("IDEO", "PartyID7", "INCOME4", "AGE7", "EDUC5")
cat_vars <- c("SEX", "RACETHNICITY", "EMPLOY", "REGION4", "HeardOf")

cluster_personas_final <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(
    across(all_of(cat_vars), list(Val = ~as.character(get_mode(.x)), Str = ~get_dominance(.x)), .names = "{.col}_{.fn}"),
    across(all_of(num_vars), list(Val = ~as.character(round(mean(.x, na.rm=T),2)), Str = ~round(sd(.x, na.rm=T),2)), .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -Cluster, names_to = "Variable_Metric", values_to = "Value") %>%
  separate(Variable_Metric, into = c("Variable", "Metric"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Cluster, values_from = Value, names_prefix = "Cluster_") %>%
  arrange(Variable)

print(cluster_personas_final)

