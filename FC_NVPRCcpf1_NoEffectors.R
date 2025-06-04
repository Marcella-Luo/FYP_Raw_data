FC_NVPRCcpf1_NoEffectors <- read.csv("Documents/College/Year 3/FYP/Results/FC_NVPRCcpf1_NoEffectors.csv")
rownames(FC_NVPRCcpf1_NoEffectors) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
FC_NVPRCcpf1_NoEffectors <- t(FC_NVPRCcpf1_NoEffectors)
FC_NVPRCcpf1_NoEffectors <- apply(FC_NVPRCcpf1_NoEffectors, 2, as.numeric)

y <- colMeans(FC_NVPRCcpf1_NoEffectors)
x <- 1:length(y)
str(y)


p_FC_NVPRCcpf1_R3 <- barplot(log2(FC_NVPRCcpf1_NoEffectors),
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),
        legend.text = c("Reference Gene GapDH", "Reference Gene Rps9"),
        ylim = c(-8, 12),  
        args.legend = list(x = "topright", fill = c("skyblue", "salmon")),
        names.arg = colnames(FC_NVPRCcpf1_NoEffectors),
        las = 2,
        ylab = "log2(Fold Change)",
        main = "log2(Fold Changes) of Target Genes for NVPRCcpf1\nReplicate 1, 2 and 3 against Ncpf1Ccpf1")

### Plotting only replicate one and two
FC_NVPRCcpf1_NoEffectors_R1R2 <- FC_NVPRCcpf1_NoEffectors[-c(3,6), ]

p_FC_NVPRCcpf1_R1R2 <- barplot(log2(FC_NVPRCcpf1_NoEffectors_R1R2),
                              beside = TRUE,
                              col = rep(c("skyblue", "salmon"), each = 2),
                              legend.text = c("Reference Gene GapDH", "Reference Gene Rps9"),
                              ylim = c(-3, 8),  # adjust as needed
                              args.legend = list(x = "topright", fill = c("skyblue", "salmon")),
                              names.arg = colnames(FC_NVPRCKDM_NoEffectors),
                              las = 2,
                              ylab = "log2(Fold Change)",
                              main = "log2(Fold Changes) of Target Genes for NVPRCcpf1\nReplicate one and two against Ncpf1Ccpf1") 

log2_FC_NVPRCcpf1 <- log2(FC_NVPRCcpf1_NoEffectors_R1R2)
mean_FC_NVPRCcpf1 <- colMeans(log2_FC_NVPRCcpf1)




