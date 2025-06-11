library(dplyr)
library(ggplot2)
library(broom)
library(tibble)
library(purrr)
library(parallelly)
library(tictoc)
library(sabRmetrics)
library(tidyverse)


cluster <- makeClusterPSOCK(8)

on.exit(stopCluster(cluster), add = TRUE)

savant_data_2021 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2021-04-01",
      end_date   = "2021-10-03",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

savant_data_2022 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2022-04-07",
      end_date   = "2022-10-05",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

savant_data_2023 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2023-03-30",
      end_date   = "2023-10-01",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

savant_data_2024 <- tryCatch(
  {
    sabRmetrics::download_baseballsavant(
      start_date = "2024-03-20",
      end_date   = "2024-09-30",
      cl         = cluster
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    NULL
  }
)

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



savant_data_21_24 <- bind_rows(
  savant_data_2021,
  savant_data_2022,
  savant_data_2023,
  savant_data_2024
)

write_csv(savant_data_21_24, "savantData21_24.csv")