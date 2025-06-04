FC_NVPRCKDM_NoEffectors <- read.csv("Documents/College/Year 3/FYP/Results/FC_NVPRCKDM_NoEffector.csv")
rownames(FC_NVPRCKDM_NoEffectors) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
FC_NVPRCKDM_NoEffectors <- t(FC_NVPRCKDM_NoEffectors)
FC_NVPRCKDM_NoEffectors <- apply(FC_NVPRCKDM_NoEffectors, 2, as.numeric)

y <- colMeans(FC_NVPRCKDM_NoEffectors)
x <- 1:length(y)
str(y)


p_FC_NVPRCKDM_R3 <- barplot(log2(FC_NVPRCKDM_NoEffectors),
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),
        legend.text = c("Reference Gene GapDH", "Reference Gene Rps9"),
        ylim = c(-3, 8),  # adjust as needed
        args.legend = list(x = "topright", fill = c("skyblue", "salmon")),
        names.arg = colnames(FC_NVPRCKDM_NoEffectors),
        las = 2,
        ylab = "log2(Fold Change)",
        main = "log2(Fold Changes) of Target Genes for NVPRCKDM\nReplicate 1, 2 and 3 against Ncpf1Ccpf1")

## Showing only R1R2
FC_NVPRCKDM_NoEffectors_R1R2 <- FC_NVPRCKDM_NoEffectors[-c(3,6), ]

p_FC_NVPRCKDM_R1R2 <- barplot(log2(FC_NVPRCKDM_NoEffectors_R1R2),
                            beside = TRUE,
                            col = rep(c("skyblue", "salmon"), each = 2),
                            legend.text = c("Reference Gene GapDH", "Reference Gene Rps9"),
                            ylim = c(-3, 8),  # adjust as needed
                            args.legend = list(x = "topright", fill = c("skyblue", "salmon")),
                            names.arg = colnames(FC_NVPRCKDM_NoEffectors),
                            las = 2,
                            ylab = "log2(Fold Change)",
                            main = "log2(Fold Changes) of Target Genes for NVPRCKDM\nReplicate 1 and 2 against Ncpf1Ccpf1")   


log2_FC_NVPRCKDM <- log2(FC_NVPRCKDM_NoEffectors_R1R2)
mean_FC_NVPRCKDM <- colMeans(log2_FC_NVPRCKDM)
