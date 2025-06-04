FC_NVPRCcpf1 <- read.csv("Documents/College/Year 3/FYP/Results/NVPRCcpf1_FC.csv")
rownames(FC_NVPRCcpf1) <- c("PlexB", "Nlg2", "Lamp1", "Wun2", "Trxr2", "Dhc16")
FC_NVPRCcpf1 <- t(FC_NVPRCcpf1)
Group1_gRNA <- FC_NVPRCcpf1[1:3,]
Group2_noRNA <- FC_NVPRCcpf1[4:6,]
FC_NVPRCcpf1 <- apply(FC_NVPRCcpf1, 2, as.numeric)

y <- colMeans(FC_NVPRCcpf1)
x <- 1:length(y)
str(y)


install.packages("plotrix")
library(plotrix)

barplot(log2(FC_NVPRCcpf1),
        beside = TRUE,
        col = rep(c("skyblue", "salmon"), each = 3),
        legend.text = c("Reference Gene GapDH", "Reference Gene Rps9"),
        ylim = c(-3, 8),  # adjust as needed
        args.legend = list(x = "topright", fill = c("skyblue", "salmon")),
        names.arg = colnames(FC_NVPRCcpf1),
        las = 2,
        ylab = "log2(Fold Change)",
        main = "log2(Fold Changes) for NVPRCcpf1")












