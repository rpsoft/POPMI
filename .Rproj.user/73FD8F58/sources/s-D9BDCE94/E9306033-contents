library(tidyverse)
dfEventSeqRisk_cvd_500_14nov20 <- read_csv("data/cvd/dfEventSeqRisk.cvd.500.14nov20.csv")
dfEventSeqRisk_mi_500_12nov20 <- read_csv("data/mi/dfEventSeqRisk.mi.500.12nov20.csv")

joint_probs <- list(
                  cvd = dfEventSeqRisk_cvd_500_14nov20,
                  mi = dfEventSeqRisk_mi_500_12nov20
)
saveRDS(joint_probs,"joint_probs.rds")