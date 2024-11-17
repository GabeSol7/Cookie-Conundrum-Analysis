################################################################################
# Cookie Privacy Research Analysis
# 
# Author: Gabriel Solis (solisgab@usc.edu)
# Created: 2023-07-18
# 
# Description:
#   This script analyzes survey data on consumer behavior regarding cookie consent
#   and online privacy. It investigates how firm size affects user trust and 
#   cookie acceptance decisions, while controlling for privacy regulation awareness
#   (GDPR/CCPA) and demographic factors.
#
# Dependencies:
#   - tidyverse (data manipulation and visualization)
#   - GGally (enhanced visualization)
#   - reshape2 (data reshaping)
#   - cowplot (plot arrangement)
#   - infer (statistical inference)
#   - lmtest (regression diagnostics)
#
# Input Data:
#   - cookies_survey.csv: Raw survey responses
#
# Output:
#   - Statistical tests of cookie acceptance by firm size
#   - Visualizations of privacy regulation awareness
#   - Analysis of trust differences between small and large firms
################################################################################

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(GGally)
  library(reshape2)
  library(cowplot)
  library(infer)
  library(lmtest)
})

################################################################################
# Data Import and Cleaning
################################################################################

# Import survey data
cookies <- read.csv("cookies_survey.csv")

# Retain rows passing two attention checks related to survey quality
cookies_clean <- cookies %>%
  filter(
    sec3_attention.check == "A large number of ‘friends’ or contacts (200+ people)" &
      Attention.Check == "Zenith Computing"
  )
# Note: Removed 52 entries that failed quality checks (from 153 to 101).


################################################################################
# Hypothesis Testing: Cookie Acceptance by Firm Size
################################################################################

# Prepare data for firm size comparison
small_large_table <- cookies_clean %>%
  filter(stimulus.id %in% c("small-old-ads", "large-old-ads")) %>%
  mutate(stimulus.id = factor(stimulus.id, 
                              levels = c("large-old-ads", "small-old-ads"))) %>%
  select(stimulus.id, DV_accept.3)

# Calculate observed difference in means
obs_stat <- small_large_table %>%
  specify(DV_accept.3 ~ stimulus.id) %>%
  calculate(stat = "diff in means", 
            order = c("large-old-ads", "small-old-ads"))

# Generate null distribution (5000 permutations)
null_dist <- small_large_table %>%
  specify(DV_accept.3 ~ stimulus.id) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000) %>%
  calculate(stat = "diff in means", 
            order = c("large-old-ads", "small-old-ads"))

# Calculate and display p-value
p_value <- null_dist %>%
  get_p_value(obs_stat = obs_stat, direction = "two-sided")

################################################################################
# Visualization: Privacy Regulation Awareness
################################################################################

# Prepare data frame for GDPR and CCPA knowledge comparison
GDPR_CCPA_knowledge <- data.frame(
  Regulation = c(rep("GDPR", length(cookies_clean$GDPR.Knowledge)), 
                 rep("CCPA", length(cookies_clean$CCPA.Knowledge))),
  Response = c(cookies_clean$GDPR.Knowledge, 
               cookies_clean$CCPA.Knowledge)
) %>%
  count(Regulation, Response, name = "Total")

# Create visualization
regulation_plot <- GDPR_CCPA_knowledge %>%
  ggplot(aes(x = reorder(Response, -Total / 101), 
             y = Total / 101, 
             fill = Regulation)) +
  geom_bar(color = "white", position = "dodge", stat = "identity") +
  labs(
    x = "Knowledge Level",
    y = "Proportion of Responses",
    title = "Knowledge of Privacy Regulations",
    subtitle = "Comparison of GDPR and CCPA Awareness"
  ) +
  scale_fill_manual(values = c("#bd0026", "#0868ac")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50")
  )

################################################################################
# Analysis: Trust in Large vs Small Firms
################################################################################

# Calculate trust scores and standard errors
trust_scores <- data.frame(
  Company = c("Amazon", "Google", "Apple", "Meta", "TikTok"),
  Mean = sapply(c("Amazon", "Google", "Apple", "Meta", "TikTok"), 
                function(x) mean(cookies_clean[[x]], na.rm = TRUE)),
  SE = sapply(c("Amazon", "Google", "Apple", "Meta", "TikTok"),
              function(x) sd(cookies_clean[[x]], na.rm = TRUE) / 
                sqrt(sum(!is.na(cookies_clean[[x]]))))
)

# Create visualization
trust_plot <- trust_scores %>%
  ggplot(aes(x = reorder(Company, -Mean), y = Mean, fill = Company)) +
  geom_bar(stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(
    x = NULL,
    y = "Average Trust Score",
    title = "Consumer Trust Across Tech Companies",
    subtitle = "Error bars represent ±1 standard error"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

################################################################################
# Save processed data and results
################################################################################

# Save cleaned dataset
write.csv(cookies_clean, "cookies_clean.csv", row.names = FALSE)

# Save plots
ggsave("regulation_awareness.pdf", regulation_plot, width = 10, height = 6)
ggsave("trust_scores.pdf", trust_plot, width = 10, height = 6)

# Print session info for reproducibility
################################################################################
# Composite Score Analysis
################################################################################

# Create composite dependent variable
cookies_clean <- cookies_clean %>%
  mutate(
    # Average of comfort-related measures, handling NAs appropriately
    Composite_DV = rowMeans(select(., starts_with("DV_comfort")), na.rm = TRUE)
  )

# Hypothesis test for composite scores
composite_test <- cookies_clean %>%
  filter(stimulus.id %in% c("small-old-ads", "large-old-ads")) %>%
  {
    # Capture observed statistic
    obs_stat <- specify(., Composite_DV ~ stimulus.id) %>%
      calculate(stat = "diff in means", 
                order = c("large-old-ads", "small-old-ads"))
    
    # Generate null distribution
    null_dist <- specify(., Composite_DV ~ stimulus.id) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 5000) %>%
      calculate(stat = "diff in means", 
                order = c("large-old-ads", "small-old-ads"))
    
    # Return results as list
    list(
      observed = obs_stat,
      null_distribution = null_dist,
      p_value = get_p_value(null_dist, obs_stat, direction = "two-sided")
    )
  }

################################################################################
# Cookie Knowledge Analysis
################################################################################

# Analyze True/False responses about cookie knowledge
knowledge_analysis <- list(
  q1 = sum(cookies_clean$sec2_T.F.cookies == "False", na.rm = TRUE),
  q2 = sum(cookies_clean$sec2_T.F.cookies_2 == "True", na.rm = TRUE),
  q3 = sum(cookies_clean$sec2_T.F.cookies.3 == "False", na.rm = TRUE),
  q4 = sum(cookies_clean$sec2_T.F.cookies_4 == "True", na.rm = TRUE),
  q5 = sum(cookies_clean$sec2_T.F.cookies_5 == "False", na.rm = TRUE),
  q6 = sum(cookies_clean$sec2_T.F.cookies_6 == "True", na.rm = TRUE)
)

# Calculate percentage of correct responses
knowledge_summary <- data.frame(
  Question = paste0("Q", 1:6),
  Incorrect = unlist(knowledge_analysis),
  Total = nrow(cookies_clean),
  Percent_Correct = 100 * (1 - unlist(knowledge_analysis) / nrow(cookies_clean))
)

################################################################################
# Trust Analysis by Firm Type
################################################################################

# Prepare data for trust analysis
trust_analysis <- data.frame(
  Firm_Type = rep(c("Small", "Large"), each = nrow(cookies_clean)),
  Trust_Score = c(cookies_clean$Start_up, cookies_clean$Large_Established_Firm)
) %>%
  # Remove any NA values
  filter(!is.na(Trust_Score))

# Fit linear model
trust_model <- lm(Trust_Score ~ Firm_Type, data = trust_analysis)

# Create summary statistics
trust_summary <- trust_analysis %>%
  group_by(Firm_Type) %>%
  summarise(
    Mean = mean(Trust_Score, na.rm = TRUE),
    SD = sd(Trust_Score, na.rm = TRUE),
    SE = SD / sqrt(n()),
    CI_Lower = Mean - 1.96 * SE,
    CI_Upper = Mean + 1.96 * SE,
    .groups = "drop"
  )

# Visualization of trust scores
trust_comparison_plot <- ggplot(trust_summary, 
                                aes(x = Firm_Type, y = Mean, fill = Firm_Type)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                width = 0.2, color = "black") +
  labs(
    title = "Trust Scores by Firm Type",
    subtitle = "Error bars represent 95% confidence intervals",
    x = "Firm Type",
    y = "Average Trust Score"
  ) +
  scale_fill_manual(values = c("Small" = "#bd0026", "Large" = "#0868ac")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

################################################################################
# Demographic Analysis
################################################################################

# Function to create demographic summary
create_demographic_summary <- function(data, variable) {
  data %>%
    group_by(!!sym(variable)) %>%
    summarise(
      Count = n(),
      Percentage = n() / nrow(data) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(Percentage))
}

# Create summaries for each demographic variable
demographic_summaries <- list(
  Education = create_demographic_summary(cookies_clean, "Education"),
  Income = create_demographic_summary(cookies_clean, "Income"),
  Employment = create_demographic_summary(cookies_clean, "Employment"),
  Political = create_demographic_summary(cookies_clean, "Political")
)

# Age summary statistics
age_summary <- cookies_clean %>%
  summarise(
    Mean_Age = mean(as.numeric(Age), na.rm = TRUE),
    SD_Age = sd(as.numeric(Age), na.rm = TRUE),
    Median_Age = median(as.numeric(Age), na.rm = TRUE),
    Q1_Age = quantile(as.numeric(Age), 0.25, na.rm = TRUE),
    Q3_Age = quantile(as.numeric(Age), 0.75, na.rm = TRUE)
  )

################################################################################
# Save Results and Generate Report
################################################################################

# Save all plots
ggsave("trust_comparison.pdf", trust_comparison_plot, width = 10, height = 6)

# Save summary statistics
write.csv(knowledge_summary, "cookie_knowledge_summary.csv", row.names = FALSE)
write.csv(trust_summary, "trust_analysis_summary.csv", row.names = FALSE)

# Save demographic summaries
lapply(names(demographic_summaries), function(name) {
  write.csv(demographic_summaries[[name]], 
            paste0("demographic_", tolower(name), ".csv"),
            row.names = FALSE)
})

# Print analysis summaries
cat("\n\nTrust Model Summary:\n")
print(summary(trust_model))
cat("\n\nDemographic Summary:\n")
print(age_summary)

# Print session info for reproducibility
################################################################################
# Privacy Notice and Engagement Analysis 
################################################################################

# Analyze privacy notice responses and engagement time
privacy_analysis <- tibble(
  Notice = cookies_clean$sec2_privacy.notice,
  Time = cookies_clean$sec2_cookie.time
) %>%
  count(Notice, Time, name = "Total") %>%
  mutate(
    Notice = reorder(Notice, -Total),
    Time = factor(Time, levels = c(
      "Less than 10 seconds",
      "10-30 seconds",
      "20-30 seconds",
      "30 seconds to 1 minute",
      "1-2 minutes",
      "More than 2 minutes",
      "Not applicable"
    ))
  )

# Create visualization for privacy notice engagement
privacy_engagement_plot <- privacy_analysis %>%
  ggplot(aes(x = Notice, y = Total, fill = Time)) +
  geom_bar(color = "white", position = "dodge", stat = "identity") +
  labs(
    title = "Privacy Notice Engagement Patterns",
    subtitle = "Analysis of user interaction time with privacy notices",
    x = "Privacy Notice Type",
    y = "Number of Responses",
    fill = "Time Spent"
  ) +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

################################################################################
# Tech Company Trust Analysis
################################################################################

# Analyze trust levels for major tech companies
tech_trust <- data.frame(
  Company = c("Amazon", "Google", "Apple", "Meta", "TikTok"),
  Mean = c(
    mean(cookies_clean$Amazon, na.rm = TRUE),
    mean(cookies_clean$Google, na.rm = TRUE),
    mean(cookies_clean$Apple, na.rm = TRUE),
    mean(cookies_clean$Meta, na.rm = TRUE),
    mean(cookies_clean$TikTok, na.rm = TRUE)
  ),
  SE = c(
    sd(cookies_clean$Amazon, na.rm = TRUE) / sqrt(sum(!is.na(cookies_clean$Amazon))),
    sd(cookies_clean$Google, na.rm = TRUE) / sqrt(sum(!is.na(cookies_clean$Google))),
    sd(cookies_clean$Apple, na.rm = TRUE) / sqrt(sum(!is.na(cookies_clean$Apple))),
    sd(cookies_clean$Meta, na.rm = TRUE) / sqrt(sum(!is.na(cookies_clean$Meta))),
    sd(cookies_clean$TikTok, na.rm = TRUE) / sqrt(sum(!is.na(cookies_clean$TikTok)))
  )
)

# Create visualization for tech company trust
tech_trust_plot <- tech_trust %>%
  ggplot(aes(x = reorder(Company, -Mean), y = Mean, fill = Company)) +
  geom_bar(stat = "identity", color = "white", position = "dodge") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(
    title = "Trust Levels Across Major Tech Companies",
    subtitle = "Error bars represent ±1 standard error",
    x = NULL,
    y = "Average Trust Score"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold")
  )

################################################################################
# Additional Hypothesis Testing
################################################################################

# Test for differences in composite privacy scores
composite_privacy_test <- cookies_clean %>%
  filter(stimulus.id %in% c("small-old-ads", "large-old-ads")) %>%
  {
    obs_stat <- specify(., Composite_DV ~ stimulus.id) %>%
      calculate(stat = "diff in means", order = c("large-old-ads", "small-old-ads"))
    
    null_dist <- specify(., Composite_DV ~ stimulus.id) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 5000) %>%
      calculate(stat = "diff in means", order = c("large-old-ads", "small-old-ads"))
    
    list(
      observed = obs_stat,
      null_distribution = null_dist,
      p_value = get_p_value(null_dist, obs_stat, direction = "two-sided")
    )
  }

# Visualize composite privacy score distribution
composite_score_plot <- ggplot(cookies_clean, aes(x = Composite_DV)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  facet_wrap(~stimulus.id) +
  labs(
    title = "Distribution of Composite Privacy Scores",
    subtitle = "Comparison between small and large firms",
    x = "Composite Privacy Score",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

################################################################################
# Save Final Results and Generate Report
################################################################################

# Save all plots
ggsave("privacy_engagement.pdf", privacy_engagement_plot, width = 12, height = 8)
ggsave("tech_trust.pdf", tech_trust_plot, width = 10, height = 6)
ggsave("composite_scores.pdf", composite_score_plot, width = 10, height = 6)

# Save analysis results
write.csv(tech_trust, "tech_trust_analysis.csv", row.names = FALSE)
write.csv(privacy_analysis, "privacy_engagement_analysis.csv", row.names = FALSE)

# Create summary report
cat("\nFinal Analysis Results\n")
cat("\n1. Composite Privacy Score Analysis")
cat("\n\n1. Tech Company Trust Analysis")
print(tech_trust)
cat("\n\n2. Privacy Notice Engagement Summary")
print(table(cookies_clean$sec2_cookie.time))

################################################################################
# Interaction Effects Analysis
################################################################################

# Analyze interaction between privacy awareness and firm size
interaction_analysis <- cookies_clean %>%
  mutate(
    GDPR_Aware = GDPR.Knowledge != "Not at all familiar",
    CCPA_Aware = CCPA.Knowledge != "Not at all familiar",
    Privacy_Aware = GDPR_Aware | CCPA_Aware
  ) %>%
  filter(stimulus.id %in% c("small-old-ads", "large-old-ads"))

# Fit interaction model
interaction_model <- lm(DV_accept.3 ~ stimulus.id * Privacy_Aware, 
                        data = interaction_analysis)

# Create summary statistics for interaction effects
interaction_summary <- interaction_analysis %>%
  group_by(stimulus.id, Privacy_Aware) %>%
  summarise(
    Mean = mean(DV_accept.3, na.rm = TRUE),
    SD = sd(DV_accept.3, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

# Visualize interaction effects
interaction_plot <- ggplot(interaction_summary, 
                           aes(x = stimulus.id, y = Mean, color = Privacy_Aware, group = Privacy_Aware)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(
    title = "Interaction Between Privacy Awareness and Firm Size",
    subtitle = "Effect on Cookie Acceptance",
    x = "Firm Size",
    y = "Average Acceptance Rate",
    color = "Privacy Aware"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

################################################################################
# Robustness Checks
################################################################################

# Check for demographic effects
demographic_models <- list(
  education = lm(DV_accept.3 ~ stimulus.id + Education, data = interaction_analysis),
  income = lm(DV_accept.3 ~ stimulus.id + Income, data = interaction_analysis),
  age = lm(DV_accept.3 ~ stimulus.id + Age, data = interaction_analysis),
  political = lm(DV_accept.3 ~ stimulus.id + Political, data = interaction_analysis)
)

# Create summary of robustness checks
robustness_summary <- lapply(demographic_models, function(model) {
  data.frame(
    R_squared = summary(model)$r.squared,
    F_stat = summary(model)$fstatistic[1],
    P_value = pf(
      summary(model)$fstatistic[1],
      summary(model)$fstatistic[2],
      summary(model)$fstatistic[3],
      lower.tail = FALSE
    )
  )
})

# Combine into single data frame
robustness_df <- bind_rows(robustness_summary, .id = "Model")

################################################################################
# Additional Comfort Scale Analysis
################################################################################

# Analyze comfort scales across different dimensions
comfort_analysis <- cookies_clean %>%
  select(starts_with("DV_comfort_scales_")) %>%
  gather(key = "Scale", value = "Score") %>%
  mutate(
    Scale = factor(Scale, 
                   levels = paste0("DV_comfort_scales_", 1:5),
                   labels = c("Personal Info", "Location", "Browsing", 
                              "Third Party", "Advertising"))
  )

# Create summary statistics for comfort scales
comfort_summary <- comfort_analysis %>%
  group_by(Scale) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    N = sum(!is.na(Score)),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

# Visualize comfort scales
comfort_plot <- ggplot(comfort_summary, aes(x = reorder(Scale, -Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(
    title = "User Comfort Levels Across Different Data Types",
    subtitle = "Error bars represent ±1 standard error",
    x = "Data Type",
    y = "Average Comfort Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

################################################################################
# Final Analysis Output
################################################################################

# Create comprehensive results summary
results_summary <- list(
  "Interaction Effects" = list(
    model = summary(interaction_model),
    summary_stats = interaction_summary
  ),
  "Robustness Checks" = robustness_df,
  "Comfort Analysis" = comfort_summary
)

# Save final results
saveRDS(results_summary, "final_analysis_results.rds")

# Save final plots
ggsave("interaction_effects.pdf", interaction_plot, width = 10, height = 6)
ggsave("comfort_analysis.pdf", comfort_plot, width = 10, height = 6)

# Create final summary report
cat("\nFinal Analysis Summary\n")
cat("====================\n\n")

cat("1. Interaction Effects Analysis\n")
cat("   F-statistic:", summary(interaction_model)$fstatistic[1], "\n")
cat("   p-value:", pf(summary(interaction_model)$fstatistic[1],
                      summary(interaction_model)$fstatistic[2],
                      summary(interaction_model)$fstatistic[3],
                      lower.tail = FALSE), "\n\n")

cat("2. Robustness Check Summary\n")
print(robustness_df)
cat("\n")

cat("3. Comfort Scale Analysis\n")
print(comfort_summary)

# Save detailed technical appendix
sink("technical_appendix.txt")
cat("Technical Appendix\n")
cat("=================\n\n")
cat("1. Model Specifications\n")
print(summary(interaction_model))
cat("\n2. Robustness Checks\n")
lapply(demographic_models, summary)
cat("\n3. Session Information\n")
sessionInfo()
sink()

################################################################################
# END OF ANALYSIS
################################################################################