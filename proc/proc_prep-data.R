# ============================================================
# proc_prep-data.R
# Purpose: Load raw data, recode variables, and save
#          the processed dataset for analysis.
# Input:   car::Prestige (built-in dataset)
# Output:  input/data/proc/dfstudy1.RData
#          input/data/proc/table000.html  (codebook)
# ============================================================

rm(list = ls())

# 0. Packages ----
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman", repos = "https://cloud.r-project.org")

pacman::p_load(
  here,    # project-safe file paths
  car,     # Prestige dataset + recode()
  dplyr,   # data manipulation
  sjPlot   # codebook
)

# 1. Load raw data ----
data(Prestige)
data_orig <- Prestige

str(data_orig)   # inspect structure

# 2. Recode variables ----
dfstudy1 <- data_orig %>%
  mutate(
    # Continuous copies
    prestige_cont  = prestige,
    income_cont    = income,
    education_cont = education,
    women_pct      = women,

    # Categorical (3 levels)
    income_cat = car::recode(income,
      "1000:8000 = 'Low'; 8001:12000 = 'Mid'; else = 'High'"),
    education_level = car::recode(education,
      "lo:10.09 = 'Low'; 10.1:13 = 'Mid'; 13.1:hi = 'High'"),
    prestige_tier = car::recode(prestige,
      "0:30.5 = 'Low'; 30.6:60 = 'Mid'; 60.1:100 = 'High'"),

    # Dummies
    blue_collar  = car::recode(type, "'bc'   = 1; else = 0"),
    professional = car::recode(type, "'prof' = 1; else = 0"),
    high_women   = car::recode(women, "50:100 = 1; else = 0")
  ) %>%
  as.data.frame()

str(dfstudy1)
summary(dfstudy1)

# 3. Create output directories ----
dir.create(here("input/data/proc"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("output/images"),    recursive = TRUE, showWarnings = FALSE)
dir.create(here("output/tables"),    recursive = TRUE, showWarnings = FALSE)

# 4. Save processed data ----
save(dfstudy1, file = here("input/data/proc/dfstudy1.RData"))
message("Saved: input/data/proc/dfstudy1.RData")

# 5. Codebook ----

sjPlot::view_df(dfstudy1)
sjPlot::view_df(dfstudy1, file = here("input/data/proc/table000.html"))
message("Saved: input/data/proc/table000.html")
