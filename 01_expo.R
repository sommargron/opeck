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
  readxl::read_xlsx('ace-jem.xlsx', range = 'A2:A355'),
  readxl::read_xlsx('ace-jem.xlsx', range = 'D2:Q355')) %>% 
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
  readxl::read_xlsx('ace-jem.xlsx', range = 'A2:A355'),
  readxl::read_xlsx('ace-jem.xlsx', range = 'R2:AE355')) %>% 
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
  readxl::read_xlsx('ace-jem.xlsx', range = 'A2:A355'),
  readxl::read_xlsx('ace-jem.xlsx', range = 'AF2:AS355')) %>% 
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
  mutate(
    vapo = vapo_l * vapo_p,
    gas = gas_l * gas_p,
    dust = dust_l * dust_p,
    dust_bio = dust_bio_l * dust_bio_p,
    dust_min = dust_min_l * dust_min_p,
    fume = fume_l * fume_p,
    dies = dies_l * dies_p,
    fibr = fibr_l * fibr_p,
    mist = mist_l * mist_p,
    asth = asth_l * asth_p,
    meta = meta_l * meta_p,
    gasf = gasf_l * gasf_p,
    vgdf = vgdf_l * vgdf_p,
    vgdffm = vgdffm_l * vgdffm_p
  )

