#Ncpf1Ccpf1
dCT_Ncpf1Ccpf1 <- read.csv("Documents/College/Year 3/FYP/Results/dCT_Ncpf1Ccpf1.csv")
rownames(dCT_Ncpf1Ccpf1) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")

#NVPRCKDM pS2
dCT_gRNA_NVPRCKDM <- read.csv("Documents/College/Year 3/FYP/Results/dCT_gRNA_NVPRCKDM.csv")
dCT_gRNA_NVPRCKDM <- dCT_gRNA_NVPRCKDM[-c(7,8),]
rownames(dCT_gRNA_NVPRCKDM) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")

df_dCT_GapDH_NVPRCKDM <- dCT_gRNA_NVPRCKDM[,c(1:6)]
df_dCT_Rps9_NVPRCKDM <- dCT_gRNA_NVPRCKDM[,c(7:12)]

df_GapDH_NVPRCKDM_pS2 <- df_dCT_GapDH_NVPRCKDM[, c(1:3)]
df_Rps9_NVPRCKDM_pS2 <- df_dCT_Rps9_NVPRCKDM[, c(1:3)]

#NVPRCcpf1
dCT_gRNA_NVPRCCcpf1 <- read.csv("Documents/College/Year 3/FYP/Results/dCT_gRNA_NVPRCcpf1.csv")
dCT_gRNA_NVPRCCcpf1 <- dCT_gRNA_NVPRCCcpf1[-c(7,8),]
rownames(dCT_gRNA_NVPRCCcpf1) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")

df_dCT_GapDH_NVPRCcpf1 <- dCT_gRNA_NVPRCCcpf1[,c(1:6)]
df_dCT_Rps9_NVPRCcpf1 <- dCT_gRNA_NVPRCCcpf1[,c(7:12)]

df_GapDH_NVPRCcpf1_pS2 <- df_dCT_GapDH_NVPRCcpf1[, c(1:3)]
df_Rps9_NVPRCcpf1__pS2 <- df_dCT_Rps9_NVPRCcpf1[, c(1:3)]

#Grouping
Group1_NVPRCKDM_GapDH <- as.data.frame(t(df_GapDH_NVPRCKDM_pS2))
Group2_NVPRCKDM_Rps9 <- as.data.frame(t(df_Rps9_NVPRCKDM_pS2))
Group3_NVPRCcpf1_GapDH <- as.data.frame(t(df_GapDH_NVPRCcpf1_pS2))
Group4_NVPRCcpf1_Rps9 <- as.data.frame(t(df_Rps9_NVPRCcpf1__pS2))
Group5_Ncpf1Ccpf1_GapDH <- as.data.frame(t(dCT_Ncpf1Ccpf1[,c(1:3)]))
Group6_Ncpf1Ccpf1_Rps9 <- as.data.frame(t(dCT_Ncpf1Ccpf1[,c(4:6)]))
genes <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")

##T-Tests
#NVPRCKDM_GapDH vs NoEffector_GapDH (Group1&5)
p_values_1 <- sapply(genes, function(gene) {
  t.test(Group1_NVPRCKDM_GapDH[[gene]], Group5_Ncpf1Ccpf1_GapDH[[gene]], paired = FALSE, var.equal = FALSE)$p.value
})

show(p_values_1)

#NVPRCKDM_Rps9 vs NoEffector_Rps9 (Group2&6)
p_values_2 <- sapply(genes, function(gene) {
  t.test(Group2_NVPRCKDM_Rps9[[gene]], Group6_Ncpf1Ccpf1_Rps9[[gene]], paired = FALSE, var.equal = FALSE)$p.value
})

show(p_values_2)

#NVPRCcpf1_GapDH vs NoEffector_GapDH (Group3&5)
p_values_3 <- sapply(genes, function(gene) {
  t.test(Group3_NVPRCcpf1_GapDH[[gene]], Group5_Ncpf1Ccpf1_GapDH[[gene]], paired = FALSE, var.equal = FALSE)$p.value
})

show(p_values_3)

#NVPRCcpf1_Rps9 vs NoEffector_Rps9 (Group4&6)
p_values_4 <- sapply(genes, function(gene) {
  t.test(Group4_NVPRCcpf1_Rps9[[gene]], Group6_Ncpf1Ccpf1_Rps9[[gene]], paired = FALSE, var.equal = FALSE)$p.value
})

show(p_values_4)

pval_table <- data.frame(
  `NVPRCKDM_GapDH` = round(p_values_1, 4),
  `NVPRCKDM_Rps9`  = round(p_values_2, 4),
  `NVPRCcpf1_GapDH` = round(p_values_3, 4),
  `NVPRCcpf1_Rps9`  = round(p_values_4, 4)
)

#Plotting Barplots
library(ggplot2)
library(ggpubr)

Group1_mean <- apply(Group1_NVPRCKDM_GapDH[, 1:6], 2, mean, na.rm = TRUE)
Group2_mean <- apply(Group2_NVPRCKDM_Rps9[, 1:6], 2, mean, na.rm = TRUE)
Group3_mean <- apply(Group3_NVPRCcpf1_GapDH[, 1:6], 2, mean, na.rm = TRUE)
Group4_mean <- apply(Group4_NVPRCcpf1_Rps9[, 1:6], 2, mean, na.rm = TRUE)
Group5_mean <- apply(Group5_Ncpf1Ccpf1_GapDH[, 1:6], 2, mean, na.rm = TRUE)
Group6_mean <- apply(Group6_Ncpf1Ccpf1_Rps9[, 1:6], 2, mean, na.rm = TRUE)


bar_matrix_1 <- rbind(Group1_mean, Group5_mean)

barplot(bar_matrix_1,
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),  
        legend.text = c("NVPRCKDM", "No effector group"),
        ylim = range(as.vector(bar_matrix_1), na.rm = TRUE) + c(-1, 1),
        args.legend = list(x = "topright", inset = c(0, -0.05), fill = c("skyblue", "salmon")),
        names.arg = colnames(bar_matrix_1),
        las = 2,
        ylab = "ΔCT",
        main = "Target Gene Expression for NVPRCKDM and Ncpf1Ccpf1")


###
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

##
df1 <- Group1_NVPRCKDM_GapDH %>%
  mutate(rep = row_number()) %>%
  pivot_longer(-rep, names_to = "gene", values_to = "value") %>%
  mutate(group = "With_Effectors")

df5 <- Group5_Ncpf1Ccpf1_GapDH %>%
  mutate(rep = row_number()) %>%
  pivot_longer(-rep, names_to = "gene", values_to = "value") %>%
  mutate(group = "No_Effectors")

df_plot_1 <- bind_rows(df1, df5)

p1 <- ggbarplot(
  df_plot_1, x = "group", y = "value",
  fill = "group", color = "group",
  facet.by = "gene",
  position = position_dodge(0.8),
  palette = c("skyblue", "salmon"),
  ylab = "ΔCT",
  ylim = c(-5, 15),
  title = "The Target Gene Expression for NVPRCKDM (With_Effector) and\nNcpf1Ccpf1(No_Effector) Relative to GapDH"
) +
  geom_jitter(
    aes(fill = group),  
    shape = 21,         
    color = "black",
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 2,
    alpha = 0.8
  ) +
  stat_compare_means(
    aes(group = group),
    method = "t.test",
    label = "p.format",      
    label.y = max(df_plot_1$value, na.rm = TRUE),
    label.x.npc = 0.4
  ) +
  stat_compare_means(
    method = "t.test",
    aes(label = paste0("p = ", ..p.format.., " (", ..p.signif.., ")")),
    comparisons = list(c("With_Effectors", "No_Effectors")),
    label.y = max(df_plot_1$value, na.rm = TRUE) + 1
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p1

####
df2 <- Group2_NVPRCKDM_Rps9 %>%
  mutate(rep = row_number()) %>%
  pivot_longer(-rep, names_to = "gene", values_to = "value") %>%
  mutate(group = "With_Effectors")

df6 <- Group6_Ncpf1Ccpf1_Rps9 %>%
  mutate(rep = row_number()) %>%
  pivot_longer(-rep, names_to = "gene", values_to = "value") %>%
  mutate(group = "No_Effectors")

df_plot_2 <- bind_rows(df2, df6)

p2 <- ggbarplot(
  df_plot_2, x = "group", y = "value",
  fill = "group", color = "group",
  facet.by = "gene",
  position = position_dodge(0.8),
  palette = c("skyblue", "salmon"),
  ylab = "ΔCT",
  ylim = c(-5, 20),
  title = "The Target Gene Expression for NVPRCKDM (With_Effector) and\nNcpf1Ccpf1(No_Effector) Relative to Rps9"
) +
  geom_jitter(
    aes(fill = group),  
    shape = 21,         
    color = "black",
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 2,
    alpha = 0.8
  ) +
  stat_compare_means(
    aes(group = group),
    method = "t.test",
    label = "p.format",      
    label.y = max(df_plot_2$value, na.rm = TRUE) +1,
    label.x.npc = 0.4
  ) +
  stat_compare_means(
    method = "t.test",
    aes(label = paste0("p = ", ..p.format.., " (", ..p.signif.., ")")),
    comparisons = list(c("With_Effectors", "No_Effectors")),
    label.y = max(df_plot_2$value, na.rm = TRUE) +2
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p2

#####
df3 <- Group3_NVPRCcpf1_GapDH %>%
  mutate(rep = row_number()) %>%
  pivot_longer(-rep, names_to = "gene", values_to = "value") %>%
  mutate(group = "With_Effectors")

df_plot_3 <- bind_rows(df3, df5)

p3 <- ggbarplot(
  df_plot_3, x = "group", y = "value",
  fill = "group", color = "group",
  facet.by = "gene",
  position = position_dodge(0.8),
  palette = c("skyblue", "salmon"),
  ylab = "ΔCT",
  ylim = c(-5, 15),
  title = "The Target Gene Expression for NVPRCcpf1 (With_Effector) and\nNcpf1Ccpf1(No_Effector) Relative to GapDH"
) +
  geom_jitter(
    aes(fill = group),  
    shape = 21,         
    color = "black",
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 2,
    alpha = 0.8
  ) +
  stat_compare_means(
    aes(group = group),
    method = "t.test",
    label = "p.format",      
    label.y = max(df_plot_3$value, na.rm = TRUE) +1,
    label.x.npc = 0.4
  ) +
  stat_compare_means(
    method = "t.test",
    aes(label = paste0("p = ", ..p.format.., " (", ..p.signif.., ")")),
    comparisons = list(c("With_Effectors", "No_Effectors")),
    label.y = max(df_plot_3$value, na.rm = TRUE) +2
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p3

######
df4 <- Group4_NVPRCcpf1_Rps9 %>%
  mutate(rep = row_number()) %>%
  pivot_longer(-rep, names_to = "gene", values_to = "value") %>%
  mutate(group = "With_Effectors")

df_plot_4 <- bind_rows(df4, df6)

p4 <- ggbarplot(
  df_plot_4, x = "group", y = "value",
  fill = "group", color = "group",
  facet.by = "gene",
  position = position_dodge(0.8),
  palette = c("skyblue", "salmon"),
  ylab = "ΔCT",
  ylim = c(-5, 20),
  title = "The Target Gene Expression for NVPRCcpf1 (With_Effector) and\nNcpf1Ccpf1(No_Effector) Relative to Rps9"
) +
  geom_jitter(
    aes(fill = group),  
    shape = 21,         
    color = "black",
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 2,
    alpha = 0.8
  ) +
  stat_compare_means(
    aes(group = group),
    method = "t.test",
    label = "p.format",      
    label.y = max(df_plot_4$value, na.rm = TRUE) +1,
    label.x.npc = 0.4
  ) +
  stat_compare_means(
    method = "t.test",
    aes(label = paste0("p = ", ..p.format.., " (", ..p.signif.., ")")),
    comparisons = list(c("With_Effectors", "No_Effectors")),
    label.y = max(df_plot_4$value, na.rm = TRUE) +2
  ) +
  theme(plot.title = element_text(hjust = 0.5))

p4


y(rstatix)
# library(dplyr)
# 
# anova_1 <- function(gene) {
#   df_GapDH <- data.frame(
#     value = c(
#       Group1_NVPRCKDM_GapDH[[gene]],
#       Group3_NVPRCcpf1_GapDH[[gene]],
#       Group5_Ncpf1Ccpf1_GapDH[[gene]]
#     ),
#     group = factor(rep(c("G1_NVPRCKDM_GapDH", "G3_NVPRCcpf1_GapDH",
#                          "G5_NoEffector_GapDH"), each = 3))
#   )
#   
# 
#   aov_result_1 <- aov(value ~ group, data = df_GapDH)
#   anova_p_1 <- summary(aov_result_1)[[1]][["Pr(>F)"]][1]
# 
#   tukey <- TukeyHSD(aov_result_1)
# 
#   return(list(gene = gene, p_value = anova_p_1, tukey = tukey))
# }
# 
# 
# anova_results_1 <- lapply(genes, anova_1)
# anova_pvals_1 <- sapply(anova_results_1, function(res) res$p_value)
# 
# anova_summary_1 <- data.frame(
#   Gene = genes,
#   ANOVA_P = round(anova_pvals_1, 4)
# )
# 
# print(anova_summary_1)
# 
# #ANOVA_Rps9
# anova_2 <- function(gene) {
#   df_Rps9 <- data.frame(
#     value = c(
#       Group2_NVPRCKDM_Rps9[[gene]],
#       Group4_NVPRCcpf1_Rps9[[gene]],
#       Group6_Ncpf1Ccpf1_Rps9[[gene]]
#     ),
#     group = factor(rep(c("G1_NVPRCKDM_Rps9", "G4_NVPRCcpf1_Rps9",
#                          "G6_NoEffector_GapDH"), each = 3))
#   )
#   
#   
#   aov_result_2 <- aov(value ~ group, data = df_Rps9)
#   anova_p_2 <- summary(aov_result_2)[[1]][["Pr(>F)"]][1]
#   
#   tukey <- TukeyHSD(aov_result_2)
#   
#   return(list(gene = gene, p_value = anova_p_2, tukey = tukey))
# }
# 
# 
# anova_results_2 <- lapply(genes, anova_2)
# anova_pvals_2 <- sapply(anova_results_2, function(res) res$p_value)
# 
# anova_summary_2 <- data.frame(
#   Gene = genes,
#   ANOVA_P = round(anova_pvals_2, 4)
# )
# 
# print(anova_summary_2)











  


