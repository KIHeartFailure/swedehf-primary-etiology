load(here(shfdbpath, "data", datadate, "lmswedehf.RData"))

lmswedehf <- left_join(
  rsdata %>%
    select(lopnr, shf_indexdtm),
  lmswedehf %>% select(lopnr, EDATUM, ATC),
  by = "lopnr"
) %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm))

lm <- lmswedehf %>%
  filter(diff >= -120, diff <= 14)

rsdata <- create_medvar(
  atc = "^C08CA",
  medname = "ccb",
  cohortdata = rsdata,
  meddata = lm,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-14days",
  fromdate = ymd("2005-11-01"),
  indexdate = shf_indexdtm
)

rsdata <- create_medvar(
  atc = "^(C02A|C02CA)",
  medname = "antiadrenerga",
  cohortdata = rsdata,
  meddata = lm,
  id = "lopnr",
  valsclass = "fac",
  metatime = "-120-14days",
  fromdate = ymd("2005-11-01"),
  indexdate = shf_indexdtm
)

rm(lmswedehf)
rm(lm)
gc()
