install.packages('tidyverse')
install.packages('readxl')
library('tidyverse')

opeck_e1 <- read_csv('data.csv', 
                     col_types = paste0(rep('c', 208), 
                                        collapse = '')) %>% 
  mutate(opeck_id = seq(nrow(.))) %>% 
  filter(!is.na(p22599)) %>% 
  select(opeck_id,
         p22599, 
         p22661,
         contains('p22601'), 
         contains('p22602'), 
         contains('p22603'),
         contains('p22663'),
         contains('p22664'))

opeck_e2 <- opeck_e1 %>% 
  pivot_longer(-c(opeck_id, p22599, p22661), 
               names_pattern = '(.*)_(.*)', 
               names_to = c('var', 'no'), 
               values_to = 'value') %>% 
  mutate(class = if_else(var %in% c('p22663', 'p22664'), 'g', 'j'),
         var = case_match(var, 
                          'p22601' ~ 'job_code', 
                          'p22602' ~ 'y_start', 
                          'p22603' ~ 'y_end', 
                          'p20121' ~ 'cascot', 
                          'p22663' ~ 'y_start', 
                          'p22664' ~ 'y_end')) %>% 
  pivot_wider(id_cols = c(opeck_id, p22599, p22661, no, class), 
              names_from = var, 
              values_from = 'value') %>% 
  filter(!is.na(y_start)) %>% 
  mutate(across(c(job_code, y_start, y_end), as.integer), 
         job_gr = str_sub(job_code, 1, 4),
         y_end = if_else(y_end == -313, 
                         true = 2016, 
                         false = y_end),
         duration = y_end - y_start)

ace_jem_b <- bind_cols(
  readxl::read_xlsx('ACE-JEM.xlsx', range = 'A2:A355', col_types = 'text'),
  readxl::read_xlsx('ACE-JEM.xlsx', range = 'D2:Q355')) %>% 
  rename('job_gr' = 'Unit   Group',
         'vapo' = 'Vapour',
         'gas' = 'Gas',
         'dust' = 'Dust',
         'dust_bio' = 'Bio-Dust',
         'dust_min' = 'Min.-Dust',
         'fume' = 'Fume',
         'dies' = 'Diesel',
         'fibr' = 'Fibre',
         'mist' = 'Mist',
         'asth' = 'Ast.',
         'meta' = 'Metal',
         'gasf' = 'Gas & fume',
         'vgdf' = 'VGDF',
         'vgdffm' = 'VGDFFM')

ace_jem_p <- bind_cols(
  readxl::read_xlsx('ACE-JEM.xlsx', range = 'A2:A355', col_types = 'text'),
  readxl::read_xlsx('ACE-JEM.xlsx', range = 'R2:AE355')) %>% 
  rename('job_gr' = 'Unit   Group',
         'vapo' = 'Vapour',
         'gas' = 'Gas',
         'dust' = 'Dust',
         'dust_bio' = 'Bio-Dust',
         'dust_min' = 'Min.-Dust',
         'fume' = 'Fume',
         'dies' = 'Diesel',
         'fibr' = 'Fibre',
         'mist' = 'Mist',
         'asth' = 'Ast.',
         'meta' = 'Metal',
         'gasf' = 'Gas & fume',
         'vgdf' = 'VGDF',
         'vgdffm' = 'VGDFFM')

ace_jem_l <- bind_cols(
  readxl::read_xlsx('ACE-JEM.xlsx', range = 'A2:A355', col_types = 'text'),
  readxl::read_xlsx('ACE-JEM.xlsx', range = 'AF2:AS355')) %>% 
  rename('job_gr' = 'Unit   Group',
         'vapo' = 'Vapour',
         'gas' = 'Gas',
         'dust' = 'Dust',
         'dust_bio' = 'Bio-Dust',
         'dust_min' = 'Min-Dust',
         'fume' = 'Fume',
         'dies' = 'Diesel',
         'fibr' = 'Fibre',
         'mist' = 'Mist',
         'asth' = 'Ast.',
         'meta' = 'Metal',
         'gasf' = 'Gas & fume',
         'vgdf' = 'VGDF',
         'vgdffm' = 'VGDFFM')

ace_jem <- ace_jem_b %>% 
  rename_with(~str_c(.x, '_b'), -job_gr) %>% 
  left_join(ace_jem_p %>% rename_with(~str_c(.x, '_p'), -job_gr), 
            by = 'job_gr') %>% 
  left_join(ace_jem_l %>% rename_with(~str_c(.x, '_l'), -job_gr), 
            by = 'job_gr') %>% 
  pivot_longer(-job_gr, 
               names_to = c('expo', 'metric'),
               names_pattern = '(.*)_(.*)') %>% 
  pivot_wider(id_cols = c('job_gr', 'expo'),
              names_from = 'metric') %>%
  mutate(
    combined = case_when(
      p == 0 ~ 0,
      l == 0 ~ 0,
      p == 1 ~ 1,
      p == 2 ~ 1,
      p == 3 & l == 1 ~ 1,
      p == 3 & l >= 2 ~ 2))

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

opeck_e3 <- opeck_e2 %>% 
  left_join(ace_jem %>% 
              select(job_gr, expo, combined) %>% 
              pivot_wider(id_cols = 'job_gr', 
                          names_from = 'expo', 
                          values_from = 'combined')) %>% 
  filter(class == 'j') %>% 
  group_by(opeck_id) %>% 
  summarise(across(c(vapo, gas, dust, dust_bio, dust_min, fume, dies, fibr, mist, 
                     asth, meta, gasf, vgdf, vgdffm),
                   ~ sum(.x * duration)))

options(scipen = 999)

p01_01 <- opeck_e3 %>% 
  pivot_longer(cols = -opeck_id, names_to = 'expo', values_to = 'eu_years') %>% 
  ggplot(aes(x = eu_years, fill = expo)) +
  geom_histogram(binwidth = 10) +
  scale_y_continuous(expand = c(0,0), 
                     trans = 'log', 
                     breaks = 10^(0:5), 
                     limits = c(1, 500000)) +
  facet_wrap(vars(expo)) +
  labs(x = 'EU-years', y = 'Count (participants)') +
  theme_bw() +
  theme(legend.position = 'none') +
  ggtitle('Distribution of exposure unit-years (EU-years) in UK Biobank')
ggsave('opeck/outputs/plots/p01_01.svg', width = 10, height = 6)
