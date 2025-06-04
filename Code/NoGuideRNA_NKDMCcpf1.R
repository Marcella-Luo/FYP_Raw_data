dCT_gRNA_NVPRCCcpf1 <- read.csv("Documents/College/Year 3/FYP/Results/dCT_gRNA_NVPRCcpf1.csv")
dCT_gRNA_NVPRCCcpf1 <- dCT_gRNA_NVPRCCcpf1[-c(7,8),]
dCT_gRNA_NVPRCCcpf1$new_column <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
colnames(dCT_gRNA_NVPRCCcpf1)[13] <- "gene_name"

df_dCT_GapDH <- dCT_gRNA_NVPRCCcpf1[,c(1:6, 13)]
df_dCT_Rps9 <- dCT_gRNA_NVPRCCcpf1[,c(7:13)]

df_GapDH_pS2 <- df_dCT_GapDH[, c(1:3)]
df_GapDH_mS2 <- df_dCT_GapDH[, (4:6)]
bar_matrix <- cbind(Group1 = df_GapDH_pS2, Group2 = df_GapDH_mS2)
rownames(bar_matrix) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
bar_matrix <- t(bar_matrix)


barplot(bar_matrix,
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),  
        legend.text = c("With Guide RNA", "Without Guide RNA"),
        ylim = range(as.vector(bar_matrix), na.rm = TRUE) + c(-1, 1),
        args.legend = list(x = "topright", inset = c(0, -0.05), fill = c("skyblue", "salmon")),
        names.arg = colnames(bar_matrix),
        las = 2,
        ylab = "ΔCT",
        main = "Target Gene Expression for NVPRCCcpf1 with GapDH as a Reference Gene")


rownames(df_GapDH_pS2) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
rownames(df_GapDH_mS2) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
df_GapDH_pS2$sd_pS2 <- apply(df_GapDH_pS2[, 1:3], 1, sd, na.rm = TRUE)
df_GapDH_mS2$sd_mS2 <- apply(df_GapDH_mS2[, 1:3], 1, sd, na.rm = TRUE)

df_Rps9_pS2 <- df_dCT_Rps9[, c(1:3)]
df_Rps9_mS2 <- df_dCT_Rps9[, (4:6)]
bar_matrix_rps9 <- cbind(Group1 = df_Rps9_pS2, Group2 = df_Rps9_mS2)
rownames(bar_matrix_rps9) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
bar_matrix_rps9 <- t(bar_matrix_rps9)


barplot(bar_matrix_rps9,
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),  
        legend.text = c("With Guide RNA", "Without Guide RNA"),
        ylim = range(c(-5, 15)),
        args.legend = list(x = "topright", inset = c(0, -0.05), fill = c("skyblue", "salmon")),
        names.arg = colnames(bar_matrix),
        las = 2,
        ylab = "ΔCT",
        main = "Target Gene Expression for NVPRCCcpf1 with Rps9 as a Reference Gene")

rownames(df_Rps9_pS2) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
rownames(df_Rps9_mS2) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
df_Rps9_pS2$sd_pS2 <- apply(df_Rps9_pS2[, 1:3], 1, sd, na.rm = TRUE)
df_Rps9_mS2$sd_mS2 <- apply(df_Rps9_mS2[, 1:3], 1, sd, na.rm = TRUE)

NVPRCCcpf1_sd <- cbind(df_GapDH_pS2$sd_pS2, df_GapDH_mS2$sd_mS2, df_Rps9_pS2$sd_pS2, df_Rps9_mS2$sd_mS2)
rownames(NVPRCCcpf1_sd) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
barplot(t(NVPRCCcpf1_sd),
        beside = TRUE,
        col = rep(c("cadetblue", "salmon", "skyblue", "coral")),  
        legend.text = c("GapDH +S2", "GapDH -S2", "Rps9 +S2", "Rps9 -S2"),
        ylim = range(as.vector(NVPRCCcpf1_sd), na.rm = TRUE) + c(-1, 1),
        args.legend = list(x = "topright", inset = c(0, -0.05),fill = c("cadetblue", "salmon", "skyblue", "coral")),
        names.arg = colnames(NVPRCCcpf1_sd),
        las = 2,
        ylab = "ΔCT",
        main = "NVPRCCcpf1 Standard Deviation of ΔCT") 

