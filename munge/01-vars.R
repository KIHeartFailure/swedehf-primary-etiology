# Variables for baseline tables -----------------------------------------------

tabvars <- c(
  # demo
  "shf_indexyear_cat",
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_location",
  "shf_followuphfunit",
  "shf_followuplocation_cat",

  # clinical factors and lab measurements
  "shf_ef_cat",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_gfrckdepi_cat",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_ntprobnp",
  "shf_ntprobnp_cat",

  # comorbs
  "shf_smoke_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_stroke",
  "shf_sos_com_af",
  "shf_anemia",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_copd",
  "sos_com_cancer3y",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",

  # treatments
  "shf_rasiarni",
  "shf_bbl",
  "shf_mra",
  "shf_sglt2",
  "shf_diuretic",
  "shf_nitrate",
  "shf_digoxin",
  "shf_anticoagulantia",
  "shf_asaantiplatelet",
  "shf_statin",
  "sos_lm_ccb",
  "sos_lm_antiadrenerga",
  "shf_device_cat",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat",
  "shf_qol",
  "shf_qol_cat"
)

# Variables for models (imputation, log, cox reg) ----------------------------

tabvars_not_in_mod <- c(
  "shf_age",
  "shf_nyha",
  "shf_revasc",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntprobnp",
  "shf_potassium",
  "shf_bmi",
  "sos_com_charlsonci",
  "sos_com_charlsonci_cat",
  "shf_qol",
  "shf_qol_cat",
  "shf_sglt2",
  "sos_lm_ccb",
  "sos_lm_antiadrenerga"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

stratavars <- c("shf_location", "shf_age_cat")

outvars <- tibble(
  var = c("sos_out_deathhosphf", "sos_out_death", "sos_out_hosphf"),
  time = c("sos_outtime_hosphf", "sos_outtime_death", "sos_outtime_hosphf"),
  shortname = c("Death/1st HFH", "Death", "1st HFH"),
  name = c("Composite All-cause death or First HF hospitalization", "All-cause death", "First HF hospitalization"),
  composite = c(T, F, F),
  rep = c(F, F, F),
  primary = c(T, F, F),
  order = c(1, 2, 3)
) %>%
  arrange(order)
