# ============================================================
# proc_main-analysis.R
# Purpose: Descriptive statistics, visualizations, and
#          regression models on the processed Prestige data.
# Input:   input/data/proc/dfstudy1.RData
# Output:  output/tables/table001.html  (descriptive table)
#          output/tables/table002.html  (regression table)
#          output/images/figure001.png  (histogram)
#          output/images/figure002.png  (bar chart)
#          output/images/figure003.png  (boxplot)
#          output/images/figure004.png  (scatter)
# ============================================================

rm(list = ls())

# 0. Packages ----
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman", repos = "https://cloud.r-project.org")

pacman::p_load(
  here,
  dplyr,
  ggplot2,
  vtable,
  kableExtra,
  broom,
  texreg
)

# 1. Load processed data ----
load(here("input/data/proc/dfstudy1.RData"))
message("Loaded: dfstudy1  (", nrow(dfstudy1), " rows)")

# 2. Descriptive table ----
tab001 <- vtable::st(
  dfstudy1,
  labels       = TRUE,
  out          = "kable",
  summ         = c("notNA(x)", "mean(x)", "median(x)", "sd(x)", "min(x)", "max(x)"),
  summ.names   = c("N", "Mean (%)", "Median", "SD", "Min", "Max"),
  factor.numeric = FALSE,
  digits       = 2,
  title        = "Table 001. Full descriptive table of processed variables"
) %>%
  kable_classic(html_font = "sans", full_width = FALSE) %>%
  footnote(general = "Numeric variables: mean, median, SD, min, max. Categorical: counts and percentages.")
tab001
kableExtra::save_kable(tab001, here("output/tables/table001.html"))
message("Saved: output/tables/table001.html")

# 3. Univariate visualizations ----
p_hist <- ggplot(dfstudy1, aes(x = prestige_cont)) +
  geom_histogram(bins = 15, fill = "steelblue", alpha = 0.7) +
  labs(title = "Figure 001. Distribution of Prestige Score",
       x = "Prestige", y = "Frequency") +
  theme_minimal()
ggsave(here("output/images/figure001.png"), p_hist, width = 8, height = 5, dpi = 600)

p_bar <- ggplot(dfstudy1, aes(x = income_cat, fill = income_cat)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Figure 002. Distribution of Income Categories",
       x = "Income Category", y = "Count") +
  theme_minimal()
ggsave(here("output/images/figure002.png"), p_bar, width = 8, height = 5, dpi = 600)

message("Saved: figures 001-002")

# 4. Bivariate visualizations ----
p_box <- ggplot(dfstudy1, aes(x = income_cat, y = prestige_cont, fill = income_cat)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Figure 003. Prestige Score by Income Category",
       x = "Income Category", y = "Prestige Score") +
  theme_minimal()
ggsave(here("output/images/figure003.png"), p_box, width = 8, height = 5, dpi = 600)

p_scatter <- ggplot(dfstudy1, aes(x = education_cont, y = prestige_cont)) +
  geom_point(alpha = 0.6, size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Figure 004. Prestige vs Education",
       x = "Years of Education", y = "Prestige Score") +
  theme_minimal()
ggsave(here("output/images/figure004.png"), p_scatter, width = 8, height = 5, dpi = 600)

message("Saved: figures 003-004")

# 5. Regression models ----
df_reg <- dfstudy1 %>%
  mutate(
    income_cat      = factor(income_cat,      levels = c("Low", "Mid", "High")),
    education_level = factor(education_level, levels = c("Low", "Mid", "High")),
    high_women      = factor(high_women,  levels = c(0, 1), labels = c("No", "Yes")),
    blue_collar     = factor(blue_collar, levels = c(0, 1), labels = c("No", "Yes"))
  )

model_1 <- lm(prestige_cont ~ income_cont + education_cont, data = df_reg)
model_2 <- lm(prestige_cont ~ income_cont + education_cont + women_pct + blue_collar, data = df_reg)
model_3 <- lm(prestige_cont ~ income_cat + education_level, data = df_reg)
model_4 <- lm(prestige_cont ~ income_cat + education_level + high_women + blue_collar, data = df_reg)

# Model fit comparison
compare_models <- bind_rows(
  glance(model_1) %>% mutate(model = "Continuous M1"),
  glance(model_2) %>% mutate(model = "Continuous M2"),
  glance(model_3) %>% mutate(model = "Categorical M3"),
  glance(model_4) %>% mutate(model = "Categorical M4")
) %>%
  select(model, nobs, r.squared, adj.r.squared, sigma, AIC, BIC)

print(compare_models)

# 6. Regression table ----
coef_map <- list(
  "(Intercept)"          = "Intercept",
  "income_cont"          = "Income (continuous)",
  "income_catMid"        = "Income: Mid (ref. Low)",
  "income_catHigh"       = "Income: High (ref. Low)",
  "education_cont"       = "Education (continuous)",
  "education_levelMid"   = "Education: Mid (ref. Low)",
  "education_levelHigh"  = "Education: High (ref. Low)",
  "women_pct"            = "Women (%)",
  "high_womenYes"        = "High women share: Yes (ref. No)",
  "blue_collarYes"       = "Blue collar: Yes (ref. No)"
)

htmlreg(
  list(model_1, model_2, model_3, model_4),
  custom.coef.map  = coef_map,
  custom.model.names = c("M1 Cont.", "M2 Cont.", "M3 Cat.", "M4 Cat."),
  caption          = "Table 002. OLS regression models predicting occupational prestige",
  caption.above    = TRUE,
  file             = here("output/tables/table002.html")
)
message("Saved: output/tables/table002.html")
