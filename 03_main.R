source('opeck/00_setup.R')
source('opeck/01_expo.R')
source('opeck/02_outc.R')

expo <- c("vapo",
          "gas",
          "dust",
          "dust_bio", 
          "dust_min",
          "fume",
          "dies",
          "fibr",
          "mist",
          "asth",
          "meta",
          "gasf", 
          "vgdf", 
          "vgdffm")

dd <- datadist(opeck_a1 %>% 
                 select(expo, age, sex, year_enrol, smo, bmi, alc, dep, eth, qua, inc))
options(datadist = "dd")

# 03_00 LONGITUDINAL ANALYSES OF INCIDENT CKD ----------------------------------
opeck_a1 <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>%  
  filter(is.na(n18) | n18 > date_enrol,
         is.na(n07)
) %>% 
  mutate(
    time = pmin(
      date_death,
      date_lofup,
      n18,
      as.Date.character('2023-03-31', format = '%Y-%m-%d'),
      na.rm = T) - date_enrol,
    event = if_else(time != n18 - date_enrol | is.na(n18), 0, 1),
    time = as.integer(time),
    year_enrol = year(date_enrol)
  )

surv_a1 <- Surv(
  time = opeck_a1$time,
  event = opeck_a1$event
)

fit_a1_m0 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a1 ~ ', 
                      x)),
    data = opeck_a1
  )
)

fit_a1_m1 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol')),
    data = opeck_a1
  )
)

fit_a1_m2 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc')),
    data = opeck_a1
  )
)

fit_a1_m3 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
    data = opeck_a1
  )
)

fit_a1_m4 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                      ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
    data = opeck_a1
  )
)

opeck_res_a1x <- 
  map2(
    list(
      map(fit_a1_m0, broom::tidy) %>% bind_rows,
      map(fit_a1_m1, broom::tidy) %>% bind_rows,
      map(fit_a1_m2, broom::tidy) %>% bind_rows,
      map(fit_a1_m3, broom::tidy) %>% bind_rows,
      map(fit_a1_m4, broom::tidy) %>% bind_rows,
    ),
    0:4,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows() %>% 
  mutate(term_f = case_match(term,
                             'asth' ~ 'Asthmagens',
                             'dies' ~ 'Diesel exhaust',
                             'dust' ~ 'Dusts',
                             'dust_bio' ~ 'Biological dusts',
                             'dust_min' ~ 'Mineral dusts',
                             'fibr' ~ 'Fibres',
                             'fume' ~ 'Fumes',
                             'gas' ~ 'Gasses',
                             'gasf' ~ 'Gasses and fumes',
                             'meta' ~ 'Metals',
                             'mist' ~ 'Mists',
                             'vapo' ~ 'Vapours',
                             'vgdf' ~ 'VGDF',
                             'vgdffm' ~ 'VGDFFM'))

# 03_00 LONGITUDINAL ANALYSES OF INCIDENT  ESKD --------------------------------
opeck_a2 <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>%  
  filter(n18 < date_enrol,
         is.na(n07)
  ) %>% 
  mutate(
    time = pmin(
      date_death,
      date_lofup,
      eskd,
      as.Date.character('2023-03-31', format = '%Y-%m-%d'),
      na.rm = T) - date_enrol,
    event = if_else(time != eskd - date_enrol | is.na(eskd), 0, 1),
    time = as.integer(time),
    year_enrol = year(date_enrol)
  )

surv_a2 <- Surv(
  time = opeck_a2$time,
  event = opeck_a2$event
)

fit_a2_m0 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a2 ~ ', 
                      x)),
    data = opeck_a2
  )
)

fit_a2_m1 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a2 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol')),
    data = opeck_a2
  )
)

fit_a2_m2 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a2 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc')),
    data = opeck_a2
  )
)

fit_a2_m3 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a2 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
    data = opeck_a2
  )
)

fit_a2_m4 <- map(
  expo,
  \(x) rms::cph(
    as.formula(paste0('surv_a2 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                      ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
    data = opeck_a2
  )
)

opeck_res_a2 <- 
  map2(
    list(
      map(fit_a2_m0, broom::tidy) %>% bind_rows,
      map(fit_a2_m1, broom::tidy) %>% bind_rows,
      map(fit_a2_m2, broom::tidy) %>% bind_rows,
      map(fit_a2_m3, broom::tidy) %>% bind_rows,
      map(fit_a2_m4, broom::tidy) %>% bind_rows
    ),
    0:4,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows() %>% 
  mutate(term_f = case_match(term,
                             'asth' ~ 'Asthmagens',
                             'dies' ~ 'Diesel exhaust',
                             'dust' ~ 'Dusts',
                             'dust_bio' ~ 'Biological dusts',
                             'dust_min' ~ 'Mineral dusts',
                             'fibr' ~ 'Fibres',
                             'fume' ~ 'Fumes',
                             'gas' ~ 'Gases',
                             'gasf' ~ 'Gases and fumes',
                             'meta' ~ 'Metals',
                             'mist' ~ 'Mists',
                             'vapo' ~ 'Vapours',
                             'vgdf' ~ 'VGDF',
                             'vgdffm' ~ 'VGDFFM'))

# 03_02 CROSS-SECTIONAL ANALYSES OF EGFR-cont ----------------------------------
opeck_a3 <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>%  
  filter(is.na(n07))

fit_a3_m0 <- map(
  expo,
  \(x) lm(
    as.formula(paste0('log(egfr) ~ ', 
                      x)),
    data = opeck_a3
  )
)

fit_a3_m1 <- map(
  expo,
  \(x) lm(
    as.formula(paste0('log(egfr) ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex')),
    data = opeck_a3
  )
)

fit_a3_m2 <- map(
  expo,
  \(x) lm(
    as.formula(paste0('log(egfr) ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex',
                      ' + smo + rcs(bmi,3) + alc')),
    data = opeck_a3
  )
)

fit_a3_m3 <- map(
  expo,
  \(x) lm(
    as.formula(paste0('log(egfr) ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
    data = opeck_a3
  )
)

fit_a3_m4 <- map(
  expo,
  \(x) lm(
    as.formula(paste0('log(egfr) ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                      ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
    data = opeck_a3
  )
)

opeck_res_a3 <- 
  map2(
    list(
      map(fit_a3_m0, broom::tidy) %>% bind_rows,
      map(fit_a3_m1, broom::tidy) %>% bind_rows,
      map(fit_a3_m2, broom::tidy) %>% bind_rows,
      map(fit_a3_m3, broom::tidy) %>% bind_rows,
      map(fit_a3_m4, broom::tidy) %>% bind_rows
    ),
    0:4,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows() %>% 
  mutate(term_f = case_match(term,
                             'asth' ~ 'Asthmagens',
                             'dies' ~ 'Diesel exhaust',
                             'dust' ~ 'Dusts',
                             'dust_bio' ~ 'Biological dusts',
                             'dust_min' ~ 'Mineral dusts',
                             'fibr' ~ 'Fibres',
                             'fume' ~ 'Fumes',
                             'gas' ~ 'Gases',
                             'gasf' ~ 'Gases and fumes',
                             'meta' ~ 'Metals',
                             'mist' ~ 'Mists',
                             'vapo' ~ 'Vapours',
                             'vgdf' ~ 'VGDF',
                             'vgdffm' ~ 'VGDFFM'))

# 03_03 CROSS-SECTIONAL ANALYSES OF EGFR-cat -----------------------------------
opeck_a4 <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>%  
  filter(is.na(n07))

fit_a4_m0 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('egfr < 60 ~ ', 
                      x)),
    data = opeck_a4
  )
)

fit_a4_m1 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('egfr < 60 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex')),
    data = opeck_a4
  )
)

fit_a4_m2 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('egfr < 60 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex',
                      ' + smo + rcs(bmi,3) + alc')),
    data = opeck_a4
  )
)

fit_a4_m3 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('egfr < 60 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
    data = opeck_a4
  )
)

fit_a4_m4 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('egfr < 60 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                      ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
    data = opeck_a4
  )
)

opeck_res_a4 <- 
  map2(
    list(
      map(fit_a4_m0, broom::tidy) %>% bind_rows,
      map(fit_a4_m1, broom::tidy) %>% bind_rows,
      map(fit_a4_m2, broom::tidy) %>% bind_rows,
      map(fit_a4_m3, broom::tidy) %>% bind_rows,
      map(fit_a4_m4, broom::tidy) %>% bind_rows
    ),
    0:4,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows() %>% 
  mutate(term_f = case_match(term,
                             'asth' ~ 'Asthmagens',
                             'dies' ~ 'Diesel exhaust',
                             'dust' ~ 'Dusts',
                             'dust_bio' ~ 'Biological dusts',
                             'dust_min' ~ 'Mineral dusts',
                             'fibr' ~ 'Fibres',
                             'fume' ~ 'Fumes',
                             'gas' ~ 'Gases',
                             'gasf' ~ 'Gases and fumes',
                             'meta' ~ 'Metals',
                             'mist' ~ 'Mists',
                             'vapo' ~ 'Vapours',
                             'vgdf' ~ 'VGDF',
                             'vgdffm' ~ 'VGDFFM'))

# 03_04 CROSS-SECTIONAL ANALYSES OF uACR-cont ----------------------------------
opeck_a5 <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>%  
  filter(is.na(n07))

fit_a5_m0 <- map(
  expo,
  \(x) lm(as.formula(paste0('log(uacr) ~ ', 
                            x)),
          data = opeck_a5
  )
)

fit_a5_m1 <- map(
  expo,
  \(x) lm(as.formula(paste0('log(uacr) ~ ', 
                            x, 
                            ' + rcs(age, 3) + sex')),
          data = opeck_a5
  )
)

fit_a5_m2 <- map(
  expo,
  \(x) lm(as.formula(paste0('log(uacr) ~ ', 
                            x, 
                            ' + rcs(age, 3) + sex',
                            ' + smo + rcs(bmi,3) + alc')),
          data = opeck_a5
  )
)

fit_a5_m3 <- map(
  expo,
  \(x) lm(as.formula(paste0('log(uacr) ~ ', 
                            x, 
                            ' + rcs(age, 3) + sex',
                            ' + smo + rcs(bmi,3) + alc',
                            ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
          data = opeck_a5
  )
)

fit_a5_m4 <- map(
  expo,
  \(x) lm(as.formula(paste0('log(uacr) ~ ', 
                            x, 
                            ' + rcs(age, 3) + sex',
                            ' + smo + rcs(bmi,3) + alc',
                            ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                            ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
          data = opeck_a5
  )
)

opeck_res_a5 <- 
  map2(
    list(
      map(fit_a5_m0, broom::tidy) %>% bind_rows,
      map(fit_a5_m1, broom::tidy) %>% bind_rows,
      map(fit_a5_m2, broom::tidy) %>% bind_rows,
      map(fit_a5_m3, broom::tidy) %>% bind_rows,
      map(fit_a5_m4, broom::tidy) %>% bind_rows
    ),
    0:4,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows() %>% 
  mutate(term_f = case_match(term,
                             'asth' ~ 'Asthmagens',
                             'dies' ~ 'Diesel exhaust',
                             'dust' ~ 'Dusts',
                             'dust_bio' ~ 'Biological dusts',
                             'dust_min' ~ 'Mineral dusts',
                             'fibr' ~ 'Fibres',
                             'fume' ~ 'Fumes',
                             'gas' ~ 'Gases',
                             'gasf' ~ 'Gases and fumes',
                             'meta' ~ 'Metals',
                             'mist' ~ 'Mists',
                             'vapo' ~ 'Vapours',
                             'vgdf' ~ 'VGDF',
                             'vgdffm' ~ 'VGDFFM'))

# 03_05 CROSS-SECTIONAL ANALYSES OF uACR-cat -----------------------------------
opeck_a6 <- left_join(
  opeck_o2,
  opeck_e3,
  by = 'opeck_id'
) %>%  
  filter(is.na(n07))

fit_a6_m0 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('uacr > 3 ~ ', 
                             x)),
           data = opeck_a6
  )
)

fit_a6_m1 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('uacr > 3 ~ ', 
                             x, 
                             ' + rcs(age, 3) + sex')),
           data = opeck_a6
  )
)

fit_a6_m2 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('uacr > 3 ~ ', 
                             x, 
                             ' + rcs(age, 3) + sex',
                             ' + smo + rcs(bmi,3) + alc')),
           data = opeck_a6
  )
)

fit_a6_m3 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('uacr > 3 ~ ', 
                             x, 
                             ' + rcs(age, 3) + sex',
                             ' + smo + rcs(bmi,3) + alc',
                             ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
           data = opeck_a6
  )
)

fit_a6_m4 <- map(
  expo,
  \(x) glm(family = 'poisson',
           as.formula(paste0('uacr > 3 ~ ', 
                             x, 
                             ' + rcs(age, 3) + sex',
                             ' + smo + rcs(bmi,3) + alc',
                             ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                             ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
           data = opeck_a6
  )
)

opeck_res_a6 <- 
  map2(
    list(
      map(fit_a6_m0, broom::tidy) %>% bind_rows,
      map(fit_a6_m1, broom::tidy) %>% bind_rows,
      map(fit_a6_m2, broom::tidy) %>% bind_rows,
      map(fit_a6_m3, broom::tidy) %>% bind_rows,
      map(fit_a6_m4, broom::tidy) %>% bind_rows
    ),
    0:4,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows() %>% 
  mutate(term_f = case_match(term,
                             'asth' ~ 'Asthmagens',
                             'dies' ~ 'Diesel exhaust',
                             'dust' ~ 'Dusts',
                             'dust_bio' ~ 'Biological dusts',
                             'dust_min' ~ 'Mineral dusts',
                             'fibr' ~ 'Fibres',
                             'fume' ~ 'Fumes',
                             'gas' ~ 'Gases',
                             'gasf' ~ 'Gases and fumes',
                             'meta' ~ 'Metals',
                             'mist' ~ 'Mists',
                             'vapo' ~ 'Vapours',
                             'vgdf' ~ 'VGDF',
                             'vgdffm' ~ 'VGDFFM'))

