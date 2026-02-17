dd <- datadist(opeck_a1 %>% 
                 select(vgdffm, age, sex, year_enrol, smo, bmi, alc, dep, eth, qua, inc))
dd$limits['Adjust to', 'vgdffm'] <- 0
dd$limits['Adjust to', 'year_enrol'] <- mean(opeck_a1$year_enrol, na.rm = TRUE)
dd$limits['Adjust to', 'smo'] <- "0"
dd$limits['Adjust to', 'age'] <- mean(opeck_a1$age, na.rm = TRUE)
dd$limits['Adjust to', 'sex'] <- 0
dd$limits['Adjust to', 'bmi'] <- mean(opeck_a1$bmi, na.rm = TRUE)
dd$limits['Adjust to', 'alc'] <- "1"
dd$limits['Adjust to', 'dep'] <- mean(opeck_a1$dep, na.rm = TRUE)
dd$limits['Adjust to', 'eth'] <- "TRUE"
dd$limits['Adjust to', 'qua'] <- mean(opeck_a1$qua, na.rm = TRUE)
dd$limits['Adjust to', 'inc'] <- "1"
options(datadist = "dd")

fit_er_vgdffm_ckd <- rms::cph(
  surv_a1 ~ rcs(vgdffm, 3) + rcs(age, 3) + sex + year_enrol +
    smo + rcs(bmi, 3) + alc + rcs(dep, 3) + eth + rcs(qua, 3) + inc,
  data = opeck_a1,
  x = TRUE, y = TRUE, surv = TRUE
)

fit_er_vgdffm_egfr <- rms::ols(
  log(egfr) ~ rcs(vgdffm, 3) + rcs(age, 3) + sex + 
    smo + rcs(bmi,3) + alc + rcs(dep,3) + eth + rcs(qua,3) + inc,
  data = opeck_a3,
  x = TRUE, y = TRUE
)

fit_er_vgdffm_uacr <- rms::ols(
  log(uacr) ~ rcs(vgdffm, 3) + rcs(age, 3) + sex + 
    smo + rcs(bmi,3) + alc + rcs(dep,3) + eth + rcs(qua,3) + inc,
  data = opeck_a3,
  x = TRUE, y = TRUE
)


pred_01 <- Predict(
  fit_er_vgdffm_ckd,
  vgdffm = 0:100*.1,
  ref.zero = TRUE
) %>% 
  as.data.frame() %>% 
  transmute(vgdffm,
            across(c(yhat, lower, upper), exp))

pred_02 <- Predict(
  fit_er_vgdffm_egfr,
  vgdffm = 0:100*.1,
  ref.zero = TRUE
) %>% 
  as.data.frame() %>% 
  transmute(vgdffm,
            across(c(yhat, lower, upper), \(x) exp(x)-1))

pred_03 <- Predict(
  fit_er_vgdffm_uacr,
  vgdffm = 0:100*.1,
  ref.zero = TRUE
) %>% 
  as.data.frame() %>% 
  transmute(vgdffm,
            across(c(yhat, lower, upper), \(x) exp(x)-1))

p06_00 <- cowplot::plot_grid(
  ggplot(pred_01, aes(vgdffm*10, yhat, ymin = lower, ymax = upper)) +
    annotate(
      'rect',
      ymin = 0.8,
      ymax = 1.25,
      xmin = -5,
      xmax = 100,
      fill = 'lightgrey',
      alpha = 0.18
    ) +
    geom_hline(aes(yintercept = 1), colour = 'white', size = 2) +
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.05, colour = 'black') +
    scale_y_continuous(trans = 'log', limits = c(.8, 1.25), breaks = c(.8, .9, 1, 1.1, 1.2)) +
    scale_x_continuous(expand = c(0,0), limits = c(-5, 100)) +
    labs(y = 'Adjusted HR (95% CI)\nfor incident CKD') +
    theme_void() +
    theme(
      axis.text.y = element_text(size = 10, hjust = 0, margin = margin(5, 5, 5, 5)),
      axis.title.y = element_text(size = 12, angle = 90),
      legend.position = 'none'
    ), 
  ggplot(pred_02, aes(vgdffm*10, yhat, ymin = lower, ymax = upper)) +
    annotate(
      'rect',
      ymin = -.01,
      ymax = .03,
      xmin = -5,
      xmax = 100,
      fill = 'lightgrey',
      alpha = 0.18
    ) +
    geom_hline(aes(yintercept = 0), colour = 'white', size = 2) +
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.05, colour = 'black') +
    scale_y_continuous(limits = c(-.01, .03),
                       breaks = -1:4*.01, 
                       labels = scales::percent) +
    scale_x_continuous(expand = c(0,0), limits = c(-5, 100)) +
    labs(y = 'Adjusted % difference (95% CI)\nin baseline eGFR') +
    theme_void() +
    theme(
      axis.text.y = element_text(size = 10, hjust = 0, margin = margin(5, 5, 5, 5)),
      axis.title.y = element_text(size = 12, angle = 90),
      legend.position = 'none'
    ), 
  ggplot(pred_03, aes(vgdffm*10, yhat, ymin = lower, ymax = upper)) +
    annotate(
      'rect',
      ymin = -.08,
      ymax = .13,
      xmin = -5,
      xmax = 100,
      fill = 'lightgrey',
      alpha = 0.18
    ) +
    geom_hline(aes(yintercept = 0), colour = 'white', size = 2) +
    geom_line(linewidth = 2) +
    geom_ribbon(alpha = 0.07, colour = 'black') +
    scale_y_continuous(limits = c(-.08, .13),
                       breaks = c(-.04, 0, .04, .08, .12), 
                       labels = scales::percent) +
    scale_x_continuous(expand = c(0,0), limits = c(-5, 100)) +
    labs(y = 'Adjusted % difference (95% CI)\nin baseline uACR') +
    theme_void() +
    theme(
      axis.text.y = element_text(size = 10, hjust = 0, margin = margin(5, 5, 5, 5)),
      axis.title.y = element_text(size = 12, angle = 90),
      legend.position = 'none'
    ), 
  NULL,
  ggplot(opeck_a1) +
    geom_histogram(aes(x = vgdffm * 10), 
                   binwidth = 10, 
                   center = 0,
                   fill = 'black',
                   alpha = .05,
                   colour = 'black') +
    labs(x = 'VGDFFM (EU-years)') +
    scale_y_continuous(expand = c(0,0), limits = c(0, 90000)) +
    scale_x_continuous(expand = c(0,0), breaks = 0:4*20) +
    coord_cartesian(xlim = c(-5, 100)) +
    theme_void() +
    theme(
      axis.text.x = element_text(size = 10, vjust = 1, margin = margin(5, 5, 5, 5)),
      axis.title.x = element_text(size = 12, hjust = 1, vjust = 0, margin = margin(0, 0, 5, 0)),
      legend.position = 'none'
    ),
  ncol = 1,
  rel_heights = c(30, 30, 30, 0, 15), 
  align = 'v')
ggsave(filename = 'opeck/outputs/plots/p06_00.svg', p06_00, width = 10, height = 10)
