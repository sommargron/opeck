source(here::here('opeck','01_expo.R'))
source(here::here('opeck','02_outc.R'))

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
          "gasf", "vgdf", 
          "vgdffm")

# 03_00 LONGITUDINAL ANALYSES --------------------------------------------------
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
  \(x) coxph(
    as.formula(paste0('surv_a1 ~ ', 
                      x)),
    data = opeck_a1
  )
)

fit_a1_m1 <- map(
  expo,
  \(x) coxph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol')),
    data = opeck_a1
  )
)

fit_a1_m2 <- map(
  expo,
  \(x) coxph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc')),
    data = opeck_a1
  )
)

fit_a1_m3 <- map(
  expo,
  \(x) coxph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + dep + eth + rcs(qua,3) + inc')),
    data = opeck_a1
  )
)

fit_a1_m4 <- map(
  expo,
  \(x) coxph(
    as.formula(paste0('surv_a1 ~ ', 
                      x, 
                      ' + rcs(age, 3) + sex + year_enrol',
                      ' + smo + rcs(bmi,3) + alc',
                      ' + dep + eth + rcs(qua,3) + inc',
                      ' + ldl + gly + dia + hpt')),
    data = opeck_a1
  )
)

opeck_res_a1 <- 
  map2(
    list(
      map(fit_a1_m0, broom::tidy) %>% bind_rows,
      map(fit_a1_m1, broom::tidy) %>% bind_rows,
      map(fit_a1_m2, broom::tidy) %>% bind_rows
    ),
    1:3,
    \(x, y) bind_rows(x) %>% 
      mutate(model = y) %>% 
      filter(term %in% expo)
  ) %>% 
  bind_rows()

p03_00 <- opeck_res_a1 %>%  
  mutate(
    model = factor(model),
    term  = factor(term),
    term_id = as.numeric(term)
  ) %>% 
  ggplot(aes(x = term,
             group = model,
             y = exp(estimate), 
             ymin = exp(estimate - 1.96*std.error), 
             ymax = exp(estimate + 1.96*std.error),
             colour = model)) +
  geom_rect(
    aes(
      xmin = term_id - 0.45,
      xmax = term_id + 0.45,
      ymin = 0.95,
      ymax = 1.25,
      fill = term
    ),
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  geom_hline(aes(yintercept = 1), colour = 'white', size = 2) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = .25) +
  scale_y_continuous(trans = 'log')+ 
  theme_void() +
  theme(
    axis.text.y = element_text(),
    legend.position = 'null'
  )
ggsave(filename = 'opeck/outputs/plots/p03_00.svg', p03_00, width = 10, height = 6)
