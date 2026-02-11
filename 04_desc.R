# Helper functions -----------------------------------------------------------------

fmt_mean_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf("%.1f (%.1f)", mean(x), sd(x))
}

fmt_median_iqr <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_character_)
  qs <- quantile(x, probs = c(.25, .5, .75), na.rm = TRUE)
  sprintf("%.2f [%.2f, %.2f]", qs[[2]], qs[[1]], qs[[3]])
}

fmt_n_pct <- function(x) {
  n <- sum(x, na.rm = TRUE)
  d <- sum(!is.na(x))
  sprintf("%d (%.1f%%)", n, 100 * n / d)
}

# Table 1A: CKD-free at baseline, by vgdffm --------------------------------
table1a <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>% 
  filter(is.na(n18) | n18 > date_enrol,
         is.na(n07)
) %>%
  mutate(
    vgdffm = case_when(
      is.na(vgdffm) ~ NA_integer_,
      vgdffm == 0   ~ 0,
      vgdffm <= 1   ~ 1,
      vgdffm > 1    ~ 2),
    uacr = if_else(!is.na(uacr), uacr >= 3, NA)
  ) %>%
  select(vgdffm, age, sex, yob, smo, bmi, alc, dep, eth, qua, inc, egfr, uacr) %>%
  mutate(vgdffm = case_match(vgdffm, 
                        0 ~ "Unexposed", 
                        1 ~ "1-10 EU-years", 
                        2 ~ ">10 EU-years")) %>%
  group_by(vgdffm) %>%
  summarise(
    N = as.character(n()),
    `Age at baseline, years, mean (IQR)` = fmt_mean_sd(age),
    `Female, n (%)`     = fmt_n_pct(sex == 0),
    `Year of birth, median (IQR)` = fmt_median_iqr(yob),
    `Never-smokers, n (%) `         = fmt_n_pct(smo == 0),
    `BMI, kg/m^2, mean (SD)`            = fmt_mean_sd(bmi),
    `Alcohol >=1 per week, n (%)`         = fmt_n_pct(alc %in% c(1,2,3)),
    `Townsend deprivation index, mean (SD)` = fmt_mean_sd(dep),
    `Ethnicity, % White` = fmt_n_pct(eth == 1),
    `Education, median (IQR)` = fmt_median_iqr(qua),
    `Income <18 000 GBP, n (%)`    = fmt_n_pct(inc == 1),
    `eGFR, mean (SD)` = fmt_mean_sd(egfr),
    `eGFR < 60, n (%)` = fmt_n_pct(egfr < 60),
    `uACR >= 3, n (%)` = fmt_n_pct(uacr),
    .groups = "drop"
  ) %>%
  relocate(vgdffm) %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname)

writexl::write_xlsx(table1a, path = 'opeck/outputs/table1a.xlsx')

# Table 1B: CKD at baseline, by vgdffm --------------------------------
table1b <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>% 
  filter(n18 <= date_enrol,
         is.na(n07)
  ) %>%
  mutate(
    vgdffm = case_when(
      is.na(vgdffm) ~ NA_integer_,
      vgdffm == 0   ~ 0,
      vgdffm <= 1   ~ 1,
      vgdffm > 1    ~ 2),
    uacr = if_else(!is.na(uacr), uacr >= 3, NA)
  ) %>%
  select(vgdffm, age, sex, yob, smo, bmi, alc, dep, eth, qua, inc, egfr, uacr) %>%
  mutate(vgdffm = case_match(vgdffm, 
                             0 ~ "Unexposed", 
                             1 ~ "1-10 EU-years", 
                             2 ~ ">10 EU-years")) %>%
  group_by(vgdffm) %>%
  summarise(
    N = as.character(n()),
    `Age at baseline, years, mean (IQR)` = fmt_mean_sd(age),
    `Female, n (%)`     = fmt_n_pct(sex == 0),
    `Year of birth, median (IQR)` = fmt_median_iqr(yob),
    `Never-smokers, n (%) `         = fmt_n_pct(smo == 0),
    `BMI, kg/m^2, mean (SD)`            = fmt_mean_sd(bmi),
    `Alcohol >=1 per week, n (%)`         = fmt_n_pct(alc %in% c(1,2,3)),
    `Townsend deprivation index, mean (SD)` = fmt_mean_sd(dep),
    `Ethnicity, % White` = fmt_n_pct(eth == 1),
    `Education, median (IQR)` = fmt_median_iqr(qua),
    `Income <18 000 GBP, n (%)`    = fmt_n_pct(inc == 1),
    `eGFR, mean (SD)` = fmt_mean_sd(egfr),
    `eGFR < 60, n (%)` = fmt_n_pct(egfr < 60),
    `uACR >= 3, n (%)` = fmt_n_pct(uacr),
    .groups = "drop"
  ) %>%
  relocate(vgdffm) %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname)

writexl::write_xlsx(table1b, path = 'opeck/outputs/table1b.xlsx')
