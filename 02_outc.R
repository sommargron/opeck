opeck_o1 <- read_csv('data.csv', 
                     col_types = paste0(rep('c', 210), 
                                        collapse = '')) %>% 
  mutate(opeck_id = seq(nrow(.))) %>% 
  filter(!is.na(p22599)) %>% 
  select(opeck_id,
         sex = p31,
         age = p21003_i0,
         yob = p34,
         date_enrol = p53_i0,
         date_death = p40000_i0,
         date_lofup = p191,
         eskd = p42026,
         eskd_source = p42027,
         n03 = p132004,
         n07 = p132012,
         n18 = p132032,
         n18_source = p132033,
         e10 = p130706,
         e11 = p130708,
         e12 = p130710,
         e13 = p130712,
         e14 = p130714,
         i10 = p131286,
         ldl = p23404_i0,
         gly = p30750_i0,
         crea = p30700_i0,
         cys = p30720_i0,
         ualb = p30500_i0,
         ucre = p30510_i0,
         smo = p20116_i0,
         bmi = p21001_i0,
         alc = p1558_i0,
         dep = p22189,
         eth = p21000_i0,
         qua = p845_i0,
         inc = p738_i0) %>% 
  mutate(
    across(
      c(smo, alc, eth, inc, qua),
      \(x) na_if(x, '-3')),
    across(
      c(qua),
      \(x) na_if(x, '-2')),
    across(
      c(eth, inc, qua),
      \(x) na_if(x, '-1')),
    eth = str_sub(eth, 1, 1), 
    ) %>% 
  mutate(across(c(sex, age, yob, qua), as.integer),
         across(c(date_enrol, date_death, date_lofup, eskd, n03, n07, n18, e10, e11, e12, e13, e14, i10), 
                \(x) as.Date.character(x, format =  '%Y-%m-%d')),
         across(c(crea, cys, ualb, ucre, bmi, dep), as.double),
         across(c(smo, alc, eth, inc, n18_source, eskd_source), as.factor)) %>% 
  mutate(n18_source = n18_source %>% 
           factor(level = c(20, 21, 30, 31, 40, 41, 50, 51),
                  label = c('Death register only', 
                            'Death register and other source(s)', 
                            'Primary care only', 
                            'Primary care and other source(s)', 
                            'Hospital admissions data only',
                            'Hospital admissions data and other source(s)',
                            'Self-report only',
                            'Self-report and other source(s)')),
         eskd_source = eskd_source %>% 
           factor(level = c(0, 1, 2, 11, 12, 21, 22),
                  label = c('Self-reported only', 
                            'Hospital admission', 
                            'Death only', 
                            'Hospital primary', 
                            'Death primary',
                            'Hospital secondary',
                            'Death contributory')))

opeck_o2 <- opeck_o1 %>% 
  mutate(uacr = ualb / (ucre / 1000),
         egfr = kidney.epi::egfr.ckdepi.cr_cys.2021(
           creatinine = crea,
           cystatin = cys,
           age = age,
           sex = sex,
           creatinine_units = "micromol/l",
           cystatin_units = "mg/L",
           label_sex_male = c("Male", 1),
           label_sex_female = c("Female", 0),
           max_age = 100),
         dia = if_any(c(e10, e11, e12, e13, e14), 
                      \(x) !is.na(x) & x <= date_enrol),
         hpt = !is.na(i10) & i10 < date_enrol)

rm(opeck_o1)
