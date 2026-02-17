
p01_00 <- ace_jem %>% 
  mutate(combined = combined %>% 
           factor(levels = c(0,1,2),
                  labels = c('none', 'low', 'high'))) %>% 
  group_by(expo) %>% 
  count(combined) %>% 
  ggplot(aes(x = combined, y = n, fill = combined)) + 
  geom_col() + 
  geom_text(aes(label = n), vjust = 0) +
  labs(x = 'Exposure group', fill = '', y = 'SOC codes (count)') +
  scale_y_continuous(expand = c(0,0), limits = c(0,400)) +
  ggtitle('ACE-JEM exposure overview') +
  facet_wrap(vars(expo)) +
  theme_bw()
ggsave(p01_00, filename = 'opeck/outputs/plots/p01_00.svg', width = 10, height = 6)

p01_01 <- opeck_e3 %>% 
  pivot_longer(cols = -opeck_id, names_to = 'expo', values_to = 'eu_years') %>% 
  ggplot(aes(x = eu_years+1, fill = expo)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(expand = c(0,0), 
                     trans = 'log', 
                     breaks = 10^(0:5), 
                     limits = c(1, 500000)) +
  scale_x_continuous(trans = 'log',
                     breaks = 10^(0:2)) +
  facet_wrap(vars(expo)) +
  labs(x = 'EU-years', y = 'Count (participants)') +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle('Distribution of exposure unit-years (EU-years) in UK Biobank')
ggsave('opeck/outputs/plots/p01_01.svg', width = 10, height = 6)



p03_01 <- opeck_res_a1 %>%  
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
      xmin = 0.93,
      xmax = 1.22
    ),
    fill = 'lightgrey',
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  geom_text(aes(y = term, x = 0.9275, label = term_f), hjust = 1, colour = 'black') +
  geom_vline(aes(xintercept = 1), colour = 'white', size = 2) +
  geom_point(aes(shape = model), position = position_dodge(.8)) +
  geom_errorbar(orientation = 'y', position = position_dodge(.8), width = .5) +
  scale_x_continuous(trans = 'log', 
                     breaks = c(.95, 1, 1.05, 1.1, 1.15, 1.2),
                     expand = c(0,0)) +
  scale_colour_grey(start = .1, end = .7, breaks = 0:4) + 
  scale_shape_manual(values = 15:19, breaks = 0:4) +
  coord_cartesian(xlim = c(.89, 1.16)) +
  labs(shape = 'Model', colour = 'Model', x = 'Adjusted HR (95% CI) for incident CKD per 10 EU-years') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1),
    axis.title.x = element_text(size = 12, hjust = 1, vjust = 0),
    legend.position = 'bottom'
  )
ggsave(filename = 'opeck/outputs/plots/p03_01.svg', p03_01, width = 10, height = 6)

p03_03 <- opeck_res_a3 %>%  
  filter(!term %in% c('asth', 'gasf'),
         model != 0) %>% 
  mutate(
    model = factor(model) %>% fct_rev(),
    term  = factor(term) %>% fct_rev(),
    term_id = as.numeric(term)
  ) %>% 
  ggplot(aes(y = term,
             group = model,
             x = (exp(estimate)-1)*100, 
             xmin = (exp(estimate - 1.96*std.error)-1) * 100, 
             xmax = (exp(estimate + 1.96*std.error)-1) * 100,
             colour = model)) +
  geom_rect(
    aes(
      ymin = term_id - 0.44,
      ymax = term_id + 0.44,
      xmin = -0.47,
      xmax = 0.65
    ),
    fill = 'lightgrey',
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  geom_text(aes(y = term, x = -0.4825, label = term_f), hjust = 1, colour = 'black') +
  geom_vline(aes(xintercept = 0), colour = 'white', size = 2) +
  geom_point(aes(shape = model), position = position_dodge(.8)) +
  geom_errorbar(orientation = 'y', position = position_dodge(.8), width = .5) +
  scale_x_continuous(breaks = c(-.4, -.2, 0, .2, .4, .6),
                     expand = c(0,0)) +
  scale_colour_grey(start = .1, end = .7, breaks = 0:4) + 
  scale_shape_manual(values = 15:19, breaks = 0:4) +
  coord_cartesian(xlim = c(-.66, .65)) +
  labs(shape = 'Model', colour = 'Model', x = 'Adjusted % difference in eGFR (95% CI) per 10 EU-years') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1),
    axis.title.x = element_text(size = 12, hjust = 1, vjust = 0),
    legend.position = 'bottom'
  )
p03_03
ggsave(filename = 'opeck/outputs/plots/p03_03.svg', p03_03, width = 10, height = 6)


p03_04 <- opeck_res_a4 %>%  
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
      xmin = 0.85,
      xmax = 1.12
    ),
    fill = 'lightgrey',
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  geom_text(aes(y = term, x = 0.8475, label = term_f), hjust = 1, colour = 'black') +
  geom_vline(aes(xintercept = 1), colour = 'white', size = 2) +
  geom_point(aes(shape = model), position = position_dodge(.8)) +
  geom_errorbar(orientation = 'y', position = position_dodge(.8), width = .5) +
  scale_x_continuous(trans = 'log', 
                     breaks = c(.85, .9, .95, 1, 1.05, 1.1),
                     expand = c(0,0)) +
  scale_colour_grey(start = .1, end = .7, breaks = 0:4) + 
  scale_shape_manual(values = 15:19, breaks = 0:4) +
  coord_cartesian(xlim = c(.81, 1.11)) +
  labs(shape = 'Model', colour = 'Model', x = 'Adjusted PR (95% CI) for eGFR <60 per 10 EU-year') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1),
    axis.title.x = element_text(size = 12, hjust = 1, vjust = 0),
    legend.position = 'bottom'
  )
ggsave(filename = 'opeck/outputs/plots/p03_04.svg', p03_04, width = 10, height = 6)

p03_05 <- opeck_res_a5 %>%  
  filter(!term %in% c('asth', 'gasf'),
         model != 0) %>% 
  mutate(
    model = factor(model) %>% fct_rev(),
    term  = factor(term) %>% fct_rev(),
    term_id = as.numeric(term)
  ) %>% 
  ggplot(aes(y = term,
             group = model,
             x = (exp(estimate)-1)*100, 
             xmin = (exp(estimate - 1.96*std.error)-1) * 100, 
             xmax = (exp(estimate + 1.96*std.error)-1) * 100,
             colour = model)) +
  geom_rect(
    aes(
      ymin = term_id - 0.44,
      ymax = term_id + 0.44,
      xmin = -2.2,
      xmax = 3.2
    ),
    fill = 'lightgrey',
    inherit.aes = FALSE,
    alpha = 0.05
  ) +
  geom_text(aes(y = term, x = -2.25, label = term_f), hjust = 1, colour = 'black') +
  geom_vline(aes(xintercept = 0), colour = 'white', size = 2) +
  geom_point(aes(shape = model), position = position_dodge(.8)) +
  geom_errorbar(orientation = 'y', position = position_dodge(.8), width = .5) +
  scale_x_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2, 2.5, 3),
                     expand = c(0,0)) +
  scale_colour_grey(start = .1, end = .7, breaks = 0:4) + 
  scale_shape_manual(values = 15:19, breaks = 0:4) +
  coord_cartesian(xlim = c(-3.2, 3.2)) +
  labs(shape = 'Model', colour = 'Model', x = 'Adjusted % difference in uACR (95% CI) per 10 EU-years') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1),
    axis.title.x = element_text(size = 12, hjust = 1, vjust = 0),
    legend.position = 'bottom'
  )
ggsave(filename = 'opeck/outputs/plots/p03_05.svg', p03_05, width = 10, height = 6)


p03_06 <- opeck_res_a6 %>%  
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
      xmax = 1.12
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
                     breaks = c(.95, .975, 1, 1.025, 1.05, 1.075, 1.1),
                     expand = c(0,0)) +
  scale_colour_grey(start = .1, end = .7, breaks = 0:4) + 
  scale_shape_manual(values = 15:19, breaks = 0:4) +
  coord_cartesian(xlim = c(.925, 1.11)) +
  labs(shape = 'Model', colour = 'Model', x = 'Adjusted PR (95% CI) for uACR >3 per 10 EU-year') +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 10, vjust = 1),
    axis.title.x = element_text(size = 12, hjust = 1, vjust = 0),
    legend.position = 'bottom'
  )
ggsave(filename = 'opeck/outputs/plots/p03_06.svg', p03_06, width = 10, height = 6)

cowplot::plot_grid(
  p03_01 + 
    ggtitle('A. Incident CKD') +
    theme(legend.position = 'none'),
  p03_03 +
    ggtitle('B. Baseline eGFR'),
  p03_05 +
    ggtitle('C. Baseline uACR') +
    theme(legend.position = 'none'),
  align = 'h',
  nrow = 1
)
