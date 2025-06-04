FC_NVPRCKDM_NoRNA <- read.csv("Documents/College/Year 3/FYP/Results/NVPRCKDM_NoGuideRNA_FC.csv")
rownames(FC_NVPRCKDM_NoRNA) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
FC_NVPRCKDM_NoRNA <- t(FC_NVPRCKDM_NoRNA)
Group1_gRNA_NVPRCKDM <- FC_NVPRCKDM_NoRNA[1:3,]
Group2_noRNA_NVPRCKDM <- FC_NVPRCKDM_NoRNA[4:6,]
FC_NVPRCKDM_NoRNA <- apply(FC_NVPRCKDM_NoRNA, 2, as.numeric)

y <- colMeans(FC_NVPRCKDM_NoRNA)
x <- 1:length(y)
str(y)


install.packages("plotrix")
library(plotrix)

barplot(log2(FC_NVPRCKDM_NoRNA),
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),
        legend.text = c("Reference Gene GapDH", "Reference Gene Rps9"),
        ylim = c(-10, 10),
        args.legend = list(x = "topright", fill = c("skyblue", "salmon")),
        names.arg = colnames(FC_NVPRCKDM_NoRNA),
        las = 2,
        ylab = "log2(Fold Change)",
        main = "log2(Fold Changes) for NVPRCKDM")
