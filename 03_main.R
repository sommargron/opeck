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
          "gasf", 
          "vgdf", 
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
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc')),
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
                      ' + rcs(dep,3) + eth + rcs(qua,3) + inc',
                      ' + rcs(ldl,3) + rcs(gly,3) + dia + hpt')),
    data = opeck_a1
  )
)

opeck_res_a1 <- 
  map2(
    list(
      map(fit_a1_m0, broom::tidy) %>% bind_rows,
      map(fit_a1_m1, broom::tidy) %>% bind_rows,
      map(fit_a1_m2, broom::tidy) %>% bind_rows,
      map(fit_a1_m3, broom::tidy) %>% bind_rows,
      map(fit_a1_m4, broom::tidy) %>% bind_rows
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

p03_00 <- opeck_res_a1 %>%  
  filter(!term %in% c('asth', 'gasf'),
         model != 0) %>% 
  mutate(
    model = factor(model) %>% fct_rev(),
    term  = factor(term) %>% fct_rev(),
    term_id = as.numeric(term)
  ) %>% 
  ggplot(aes(y = term,
             group = model,
             x = exp(estimate), 
             xmin = exp(estimate - 1.96*std.error), 
             xmax = exp(estimate + 1.96*std.error),
             colour = model)) +
  geom_rect(
    aes(
      ymin = term_id - 0.44,
      ymax = term_id + 0.44,
      xmin = 0.95,
      xmax = 1.22
    ),
    fill = 'lightgrey',
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  geom_text(aes(y = term, x = 0.9475, label = term_f), hjust = 1, colour = 'black') +
  geom_vline(aes(xintercept = 1), colour = 'white', size = 2) +
  geom_point(aes(shape = model), position = position_dodge(.8)) +
  geom_errorbar(orientation = 'y', position = position_dodge(.8), width = .5) +
  scale_x_continuous(trans = 'log', 
                     breaks = c(.95, 1, 1.05, 1.1, 1.15, 1.2),
                     expand = c(0,0)) +
  scale_colour_grey(start = .1, end = .7, breaks = 0:4) + 
  scale_shape_manual(values = 15:19, breaks = 0:4) +
  coord_cartesian(xlim = c(.91, 1.21)) +
  labs(shape = 'Model', colour = 'Model', x = 'Adjusted HR (95% CI) for incident CKD per 10 EU-year') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1),
    axis.title.x = element_text(size = 12, hjust = 1, vjust = 0),
    legend.position = 'bottom'
  )
ggsave(filename = 'opeck/outputs/plots/p03_00.svg', p03_00, width = 10, height = 6)
