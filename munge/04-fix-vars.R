rsdata <- rsdata %>%
  mutate(
    shf_primaryetiology_cat = factor(case_when(
      shf_primaryetiology == "IHD" ~ 1,
      shf_primaryetiology == "Heart valve disease" ~ 2,
      shf_primaryetiology == "Hypertension" ~ 3,
      TRUE ~ 4
    ), levels = 1:4, labels = c("Ishemic", "Valvular", "Hypertensive", "Other")),
    shf_primaryetiology2 = factor(case_when(
      shf_primaryetiology == "IHD" ~ 1,
      shf_primaryetiology == "Heart valve disease" ~ 2,
      shf_primaryetiology == "Hypertension" ~ 3,
      shf_primaryetiology == "DCM" ~ 4,
      shf_primaryetiology == "Known alcoholic cardiomyopathy" ~ 5,
      TRUE ~ 6
    ), levels = 1:6, labels = c("Ishemic", "Valvular", "Hypertensive", "DCM", "Alcoholic cardiomyopathy", "Other")),
    shf_ef_cat = factor(shf_ef_cat, levels = c("HFpEF", "HFmrEF", "HFrEF")),
    shf_indexyear_cat = factor(case_when(
      shf_indexyear <= 2015 ~ "2010-2015",
      shf_indexyear <= 2020 ~ "2016-2020",
      shf_indexyear <= 2023 ~ "2021-2023"
    )),
    shf_rasi = case_when(
      is.na(shf_arb) | is.na(shf_acei) ~ NA_character_,
      shf_arb == "Yes" | shf_acei == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    # comp risk outcomes
    sos_out_hosphf_cr = create_crevent(sos_out_hosphf, sos_out_death, eventvalues = c("Yes", "Yes")),
    sos_out_hosphf_cr = factor(sos_out_hosphf_cr, levels = 0:2, labels = c("censor", "hfh", "death"))
  ) %>%
  mutate(
    meds_hypertension = rowSums(across(c("shf_rasi", "shf_bbl", "shf_mra", "shf_diuretic", "sos_lm_ccb", "sos_lm_antiadrenerga")) == "Yes"),
    meds_hf = rowSums(across(c("shf_rasiarni", "shf_bbl", "shf_mra")) == "Yes"),
    meds_both = rowSums(across(c("shf_rasiarni", "shf_bbl", "shf_mra", "shf_diuretic", "sos_lm_ccb", "sos_lm_antiadrenerga")) == "Yes")
  )

rsdata <- create_crvar(rsdata, "shf_primaryetiology_cat")

# income
inc <- rsdata %>%
  reframe(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = shf_indexyear) %>%
  unnest(cols = c(incsum)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < `33%` ~ 1,
        scb_dispincome < `66%` ~ 2,
        scb_dispincome >= `66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile within year", "2nd tertile within year", "3rd tertile within year")
    )
  ) %>%
  select(-`33%`, -`66%`)

# ntprobnp

nt <- rsdata %>%
  reframe(ntmed = list(enframe(quantile(shf_ntprobnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  )))) %>%
  unnest(cols = c(ntmed)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- rsdata %>%
  mutate(
    shf_ntprobnp_cat = factor(
      case_when(
        shf_ntprobnp < nt$`33%` ~ 1,
        shf_ntprobnp < nt$`66%` ~ 2,
        shf_ntprobnp >= nt$`66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile", "2nd tertile", "3rd tertile")
    )
  )

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))
