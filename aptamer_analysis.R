library(tidyverse)

aptamer_analysis <- function(enrich_tsv) {
  a <- enrich_tsv %>% 
    select(Sequence = "Sequence", RPM_3 = "RPM (x)", RPM_2 = "RPM (y)", RPM_1 = "RPM (z)") %>%
    gather("RPPM", "RPM", 2:4, na.rm = TRUE)
  a$RPM <- as.numeric(a$RPM)
  a_2 <- a %>% 
    group_by(Sequence) %>% 
    summarise(meanRPM = mean(RPM), n = n(), sd = sd(RPM), RSD = sd(RPM)/mean(RPM)*100)
