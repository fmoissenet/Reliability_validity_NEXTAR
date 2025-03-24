# ==============================================================================
# NEXTAR study
# Compute reliability and validity parameters
# Florent Moissenet
# Geneva University Hospitals
# 2025
# ==============================================================================

# Clear workspace
rm(list = ls())

# Select working directories
folder_inputs  <- "C:/Users/Florent/OneDrive - Université de Genève/_VALORISATION/articles/1- en cours/Moissenet - NextAR/0- Dataset"
setwd(folder_inputs)

# Load requested libraries
library(ggplot2)
library(dplyr)
library(rstatix)
library(FSA)
library(car)
library(dplyr)
library(boot)
library(VCA)
library(lme4)
library(ggsignif)
library(emmeans)
library(coin)
library(reshape2)

# 0. Load data
# ==============================================================================
df <- read.csv("Prepared_data.csv",header=TRUE,sep=",",dec=".")

# 1. Dataset overview
# ==============================================================================
print(head(df))
print(str(df))
print(summary(df))
retro_summary <- df %>%
  summarise(
    Median = median(Retroversion, na.rm = TRUE),
    Q1 = quantile(Retroversion, 0.25, na.rm = TRUE),
    Q3 = quantile(Retroversion, 0.75, na.rm = TRUE),
    IQR = Q3-Q1,
  )
print(retro_summary)
myvars <- c("Retroversion")
dat.m <- melt(df[, c('Operator', 'Group', 'Measure', 'Technique', myvars)],
              id.vars = c('Operator', 'Group', 'Measure', 'Technique'),
              measure.vars = myvars)
theme_set(theme_bw())
pab1 <- ggplot(dat.m, aes(x = Operator, y = value, color = Technique, shape = factor(Group))) +
  geom_point(size = 3.5) +  # <-- Taille des points ici
  theme(
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "Retroversion measures distribution",
    x = "",
    y = "Retroversion (°)"
  )
print(pab1)

# Ellipse view
p_validity <- ggplot(df, aes(x = Retroversion, y = Inclination, color = Technique)) +
  geom_point(alpha = 0.6, size = 3) +
  stat_ellipse(type = "norm", linetype = "dashed", level = 0.95, size = 1) +
  facet_wrap(~ Group) +
  theme_minimal() +
  labs(
    title = "Measurement Validity and Reliability",
    x = "Retroversion (°)",
    y = "Inclination (°)",
    color = "Technique"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
print(p_validity)

# Do retroversion measurements follow a normal distribution?
# ------------------------------------------------------------------------------
normality_global <- shapiro.test(df$Retroversion)
print(normality_global)
ggplot(df, aes(sample = Retroversion)) +
  stat_qq() +
  stat_qq_line(col = "red") +
  theme_minimal() +
  labs(title = "Retroversion QQ Plot", 
       x = "Theorical quantiles", 
       y = "Measured quantiles")

# # Do retroversion measurements significantly differ between groups?
# # ------------------------------------------------------------------------------
# # Comparison of the group-related measurements
# ggplot(df, aes(x = Group, y = Retroversion, color = Group)) +
#   geom_boxplot(alpha = 0.5, outlier.shape = NA) +
#   geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
#   geom_signif(comparisons = list(c("G1", "G2"), c("G1", "G3"), c("G2", "G3")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               step_increase = 0.1) +
#   theme_minimal() +
#   labs(title = "Retroversion measures per group",
#        x = "Group", y = "Retroversion (°)") +
#   scale_color_brewer(palette = "Set1")
# retro_summary_by_group <- df %>%
#   group_by(Group) %>%
#   summarise(
#     Median = median(Retroversion, na.rm = TRUE),
#     Q1 = quantile(Retroversion, 0.25, na.rm = TRUE),
#     Q3 = quantile(Retroversion, 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1
#   )
# print(retro_summary_by_group)
# 
# # Comparison of the group-related measurements (non-parametric tests)
# kruskal_group    <- kruskal.test(Retroversion ~ Group, data = df) # Kruskal-Wallis test
# group_levels     <- unique(df$Group)
# pairwise_results <- data.frame(Group1 = character(),
#                                Group2 = character(),
#                                diff = numeric(),
#                                lwr = numeric(),
#                                upr = numeric(),
#                                p = numeric(),
#                                stringsAsFactors = FALSE)
# print(kruskal_group)
# for(pair in combn(group_levels, 2, simplify = FALSE)) {  # Wilcoxon (Hodges-Lehmann) test
#   d_sub            <- subset(df, Group %in% pair)
#   x                <- d_sub$Retroversion[d_sub$Group == pair[1]]
#   y                <- d_sub$Retroversion[d_sub$Group == pair[2]]
#   res              <- wilcox.test(x, y, conf.int = TRUE, exact = FALSE)
#   diff_est         <- as.numeric(res$estimate)
#   conf_int         <- res$conf.int
#   pairwise_results <- rbind(pairwise_results,
#                             data.frame(Group1 = pair[1],
#                                        Group2 = pair[2],
#                                        diff = diff_est,
#                                        lwr = conf_int[1],
#                                        upr = conf_int[2],
#                                        p = res$p.value,
#                                        stringsAsFactors = FALSE))
# }
# pairwise_results$p.adj <- p.adjust(pairwise_results$p, method = "BH") # Benjamini-Hochberg adjustment
# print(pairwise_results)
# 
# # Do retroversion measurements significantly differ between techniques?
# # ------------------------------------------------------------------------------
# # Comparison of the technique-related measurements
# ggplot(df, aes(x = Technique, y = Retroversion, color = Technique)) +
#   geom_boxplot(alpha = 0.5, outlier.shape = NA) +
#   geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
#   geom_signif(comparisons = list(c("T1", "T2")),
#               map_signif_level = TRUE,
#               test = "wilcox.test",
#               step_increase = 0.1) +
#   theme_minimal() +
#   labs(title = "Retroversion measures per technique",
#        x = "Technique", y = "Retroversion (°)") +
#   scale_color_brewer(palette = "Set1")
# retro_summary_by_tech <- df %>%
#   group_by(Technique) %>%
#   summarise(
#     Median = median(Retroversion, na.rm = TRUE),
#     Q1 = quantile(Retroversion, 0.25, na.rm = TRUE),
#     Q3 = quantile(Retroversion, 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1
#   )
# print(retro_summary_by_tech)
# 
# # Comparison of the technique-related measurements (non-parametric tests)
# kruskal_technique <- kruskal.test(Retroversion ~ Technique, data = df) # Kruskal-Wallis test
# technique_levels  <- unique(df$Technique)
# pairwise_results  <- data.frame(Technique1 = character(),
#                                 Technique2 = character(),
#                                 diff = numeric(),
#                                 lwr = numeric(),
#                                 upr = numeric(),
#                                 p = numeric(),
#                                 stringsAsFactors = FALSE)
# print(kruskal_technique)
# for(pair in combn(technique_levels, 2, simplify = FALSE)) { # Wilcoxon (Hodges-Lehmann) test
#   d_sub            <- subset(df, Technique %in% pair)
#   x                <- d_sub$Retroversion[d_sub$Technique == pair[1]]
#   y                <- d_sub$Retroversion[d_sub$Technique == pair[2]]
#   res              <- wilcox.test(x, y, conf.int = TRUE, exact = FALSE)
#   diff_est         <- as.numeric(res$estimate)
#   conf_int         <- res$conf.int
#   pairwise_results <- rbind(pairwise_results,
#                             data.frame(Technique1 = pair[1],
#                                        Technique2 = pair[2],
#                                        diff = diff_est,
#                                        lwr = conf_int[1],
#                                        upr = conf_int[2],
#                                        p = res$p.value,
#                                        stringsAsFactors = FALSE))
# }
# pairwise_results$p.adj <- p.adjust(pairwise_results$p, method = "BH") # Benjamini-Hochberg adjustment
# print(pairwise_results)

# 2. Effect of the techniques in the groups
# ==============================================================================
# Does the effect of Technique vary according to Group?
# ------------------------------------------------------------------------------
# Boxplot
ggplot(df, aes(x = Group, y = Retroversion, fill = Technique)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.7)) +
  geom_jitter(aes(color = Technique), size = 2, alpha = 0.7,
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7)) +
  labs(title = "Retroversion measures by group and technique",
       x = "Group",
       y = "Retroversion (°)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Inter-operator and intra-operators variances
df$Technique <- as.factor(df$Technique)
variance_summary <- data.frame()
perm_results_inter <- data.frame()
perm_results_intra <- data.frame()
for(g in unique(df$Group)) {
  df_g <- subset(df, Group == g)
  for(t in unique(df$Technique)) {
    df_gt <- subset(df_g, Technique == t)
    mod_gt <- lmer(Retroversion ~ (1 | Operator), data = df_gt)
    vc_gt <- as.data.frame(VarCorr(mod_gt))
    var_ope <- vc_gt$vcov[vc_gt$grp == "Operator"]
    var_res <- attr(VarCorr(mod_gt), "sc")^2
    variance_summary <- rbind(variance_summary,
                              data.frame(
                                Group = g,
                                Technique = t,
                                Inter_Operator_Variance = var_ope,
                                Intra_Operator_Variance = var_res
                              )
    )
  }
  perm_test_inter <- oneway_test( # Permutation test using Levene's Test Statistic (inter-operator variances)
    abs(Retroversion - ave(Retroversion, Operator, FUN = median)) ~ Technique,
    data = df_g,
    distribution = approximate(nresample = 10000)
  )
  perm_test_intra <- oneway_test( # Permutation test using Levene's Test Statistic (intra-operator variances)
    abs(Retroversion - ave(Retroversion, interaction(Operator, Technique), FUN = median)) ~ Technique,
    data = df_g,
    distribution = approximate(nresample = 10000)
  )
  sd_T1_inter <- sqrt(variance_summary$Inter_Operator_Variance[variance_summary$Group == g & variance_summary$Technique == "T1"])
  sd_T2_inter <- sqrt(variance_summary$Inter_Operator_Variance[variance_summary$Group == g & variance_summary$Technique == "T2"])
  sd_T1_intra <- sqrt(variance_summary$Intra_Operator_Variance[variance_summary$Group == g & variance_summary$Technique == "T1"])
  sd_T2_intra <- sqrt(variance_summary$Intra_Operator_Variance[variance_summary$Group == g & variance_summary$Technique == "T2"])
  effect_size_inter <- (sd_T1_inter - sd_T2_inter) / ((sd_T1_inter + sd_T2_inter) / 2)
  effect_size_intra <- (sd_T1_intra - sd_T2_intra) / ((sd_T1_intra + sd_T2_intra) / 2)
  perm_results_inter <- rbind(perm_results_inter,
                              data.frame(Group = g, Permutation_p = pvalue(perm_test_inter), Effect_Size = effect_size_inter)
  )
  perm_results_intra <- rbind(perm_results_intra,
                              data.frame(Group = g, Permutation_p = pvalue(perm_test_intra), Effect_Size = effect_size_intra)
  )
}
cat("\n--- Variance components (inter & intra) par groupe et technique ---\n")
print(variance_summary)
cat("\n--- Comparaison T1 vs T2 (inter-opérateur) ---\n")
print(perm_results_inter)
cat("\n--- Comparaison T1 vs T2 (intra-opérateur) ---\n")
print(perm_results_intra)

# Total variance
results_by_group_T1 <- list()
results_by_group_T2 <- list()
perm_results <- data.frame(Group = character(),
                           Permutation_p = numeric(),
                           Effect_Size = numeric(),
                           stringsAsFactors = FALSE)

for(g in unique(df$Group)) {

  df_g_T1 <- subset(df, Group == g & Technique == "T1")
  mod_g_T1 <- lmer(Retroversion ~ (1 | Operator), data = df_g_T1)
  vc_g_T1 <- as.data.frame(VarCorr(mod_g_T1))
  var_ope_T1 <- vc_g_T1$vcov[vc_g_T1$grp == "Operator"]
  var_res_T1 <- attr(VarCorr(mod_g_T1), "sc")^2
  results_by_group_T1[[g]] <- data.frame(Group = g, var_operator = var_ope_T1, var_res = var_res_T1)

  df_g_T2 <- subset(df, Group == g & Technique == "T2")
  mod_g_T2 <- lmer(Retroversion ~ (1 | Operator), data = df_g_T2)
  vc_g_T2 <- as.data.frame(VarCorr(mod_g_T2))
  var_ope_T2 <- vc_g_T2$vcov[vc_g_T2$grp == "Operator"]
  var_res_T2 <- attr(VarCorr(mod_g_T2), "sc")^2
  results_by_group_T2[[g]] <- data.frame(Group = g, var_operator = var_ope_T2, var_res = var_res_T2)
  
  sd_T1 <- sqrt(var_ope_T1 + var_res_T1)
  sd_T2 <- sqrt(var_ope_T2 + var_res_T2)
  effect_size <- (sd_T1 - sd_T2) / ((sd_T1 + sd_T2) / 2)
  perm_test <- oneway_test(abs(Retroversion - median(Retroversion)) ~ Technique, # Permutation test using Levene's Test Statistic
                           data = subset(df, Group == g),
                           distribution = approximate(nresample = 10000))
  perm_results <- rbind(perm_results,
                        data.frame(Group = g,
                                   Permutation_p = pvalue(perm_test),
                                   Effect_Size = effect_size))
}
results_T1 <- bind_rows(results_by_group_T1)
results_T2 <- bind_rows(results_by_group_T2)
print(results_T1)
print(results_T2)
print(perm_results)

# Validity /retroversion
# ------------------------------------------------------------------------------
# Target value
ref_retro <- 0

# Error
df <- df %>%
  mutate(
    bias_retro = Retroversion - ref_retro
  )

# Validity
retro_validity <- df %>%
  group_by(Group, Technique) %>%
  summarise(
    Median     = median(Retroversion, na.rm = TRUE),
    Q1         = quantile(Retroversion, 0.25, na.rm = TRUE),
    Q3         = quantile(Retroversion, 0.75, na.rm = TRUE),
    IQR        = IQR(Retroversion, na.rm = TRUE),
    Bias_Med   = median(bias_retro, na.rm = TRUE),
    p_value    = wilcox.test(bias_retro, mu = 0)$p.value,
    effect     = wilcox_effsize(data.frame(bias_retro), bias_retro ~ 1)$effsize,
    .groups = "drop"
  )
print(retro_validity)