library(dplyr)
library(ggplot2)
library(broom)
library(tibble)
library(purrr)
library(parallelly)
library(tictoc)
library(sabRmetrics)
library(tidyverse)
library(parallel)

cluster <- makeClusterPSOCK(8)

savant_data_2025 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2025-03-18",
      end_date   = "2025-05-24",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

write_csv(savant_data_2025, "savantData25.csv")
stopCluster(cluster)
