
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)      # For daisy (Gower) and pam (K-Medoids)
library(randomForest)
library(viridis)


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

# filter: HeardOf == 1 AND Duration >= 2.0 minutes
analysis_df <- data %>%
  select(all_of(c(survey_qs, demo_vars))) %>%
  mutate(HeardOf = as.numeric(HeardOf),
         duration_UOFI = as.numeric(duration_UOFI)) %>%
  filter(!is.na(HeardOf) & HeardOf == 1) %>%
  filter(!is.na(duration_UOFI) & duration_UOFI >= 2.0)

cat("Respondents after filtering:", nrow(analysis_df), "\n")

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

dist_matrix_mat <- as.matrix(dist_matrix)
dist_matrix_mat[is.na(dist_matrix_mat)] <- 1.0
dist_matrix <- as.dist(dist_matrix_mat)

# K-MEDOIDS MODELING

K_CHOSEN <- 3 # from elbow
pam_res <- pam(dist_matrix, k = K_CHOSEN, diss = TRUE)

# Add clusters back to data
respondent_clustered_full <- analysis_df %>%
  mutate(Cluster = factor(pam_res$clustering))


# CLUSTER ANALYSIS & VISUALIZATION


# participant-level averages
respondent_clustered_full <- respondent_clustered_full %>%
  mutate(avg_support = rowMeans(select(., all_of(survey_cols_only)), na.rm = TRUE))

# cluster Summary Table
cluster_summary_wide <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(
    mean = mean(avg_support, na.rm = TRUE),
    sd   = sd(avg_support, na.rm = TRUE),
    n    = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -Cluster, names_to = "Metric", values_to = "Value") %>%
  mutate(Cluster = paste0("Cluster_", Cluster)) %>%
  pivot_wider(names_from = Cluster, values_from = Value)

print(cluster_summary_wide)

pca_data <- clustering_data_mapped %>% drop_na()
if(nrow(pca_data) > 0) {
  pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)
  pca_df <- data.frame(
    PC1 = pca$x[,1],
    PC2 = pca$x[,2],
    Cluster = factor(pam_res$clustering[complete.cases(clustering_data_mapped)])
  )
  ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "PCA Visualization of K-Medoids Clusters") +
    theme_minimal() + scale_color_brewer(palette = "Set1")
}

# Rrf model (from old pieline) we decided to not include it in the final piepline

top_15_vars <- c("duration_UOFI", "EDUC5", "NewsFreq", "INCOME4", "EMPLOY", 
                 "PHONESERVICE", "PartyID7", "Device", "IDEO", "RACETHNICITY", 
                 "SURV_MODE", "REGION4", "SEX", "NEWSCONS", "AGE7")

train_df <- respondent_clustered_full %>%
  select(all_of(top_15_vars), Cluster) %>%
  mutate(across(where(is.character), as.factor))

set.seed(42)
rf_model <- randomForest(Cluster ~ ., data = train_df, ntree = 500, importance = TRUE)
varImpPlot(rf_model)

# PROFILING & HEATMAP

survey_profile <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(across(all_of(survey_cols_only), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(cols = -Cluster, names_to = "Question", values_to = "Mean_Score")

ggplot(survey_profile, aes(x = Cluster, y = Question, fill = Mean_Score)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma", name = "Avg Score (1-5)") +
  labs(title = "Survey Patterns by Cluster", x = "Cluster", y = "Question") +
  theme_minimal() + theme(axis.text.y = element_text(size = 7))

# GAP ANALYSIS (DISAGREEMENT BETWEEN CLUSTERS)

# Biggest disagreements between any clusters
cluster_comparison <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(across(all_of(survey_cols_only), mean, na.rm = TRUE)) %>%
  pivot_longer(-Cluster, names_to = "Question", values_to = "MeanScore") %>%
  pivot_wider(names_from = Cluster, names_prefix = "C", values_from = MeanScore) %>%
  mutate(Max_Diff = pmax(abs(C1-C2), abs(C1-C3), abs(C2-C3))) %>%
  arrange(desc(Max_Diff))

print("Top distinguishing questions (All Clusters):")
print(as.data.frame(cluster_comparison))

# specific gap: Cluster 2 vs Cluster 3
cluster_comparison_c2_c3 <- respondent_clustered_full %>%
  group_by(Cluster) %>%
  summarise(across(all_of(survey_cols_only), mean, na.rm = TRUE)) %>%
  pivot_longer(-Cluster, names_to = "Question", values_to = "MeanScore") %>%
  pivot_wider(names_from = Cluster, names_prefix = "C", values_from = MeanScore) %>%
  mutate(Diff_C2_C3 = abs(C2 - C3)) %>% 
  arrange(desc(Diff_C2_C3))

print("Top distinguishing questions (C2 vs C3):")
print(as.data.frame(cluster_comparison_c2_c3))

# short demographic screening: look only for the two talked about variables
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

num_vars <- c("IDEO", "PartyID7", "INCOME4", "AGE7", "EDUC5", "duration_UOFI")
cat_vars <- c("SEX", "RACETHNICITY", "EMPLOY", "REGION4", "SURV_MODE")

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
