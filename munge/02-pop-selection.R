# Inclusion/exclusion criteria --------------------------------------------------------

flow <- flow[1:10, 1:2]

names(flow) <- c("Criteria", "N")

flow <- rbind(c("General inclusion/exclusion criteria", ""), flow)

flow <- rbind(flow, c("Project specific inclusion/exclusion criteria", ""))

rsdata <- rsdata411 %>%
  filter(shf_indexdtm >= ymd("2010-04-08"))
flow <- rbind(flow, c("Exclude posts < 2010-04-08 (Primary etiology included in the CRF)", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_primaryetiology))
flow <- rbind(flow, c("Exclude posts with missing Primary etiology", nrow(rsdata)))

rsdata <- rsdata %>%
  filter(!is.na(shf_ef_cat))
flow <- rbind(flow, c("Exclude posts with missing EF", nrow(rsdata)))

rsdata <- rsdata %>%
  group_by(lopnr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(rsdata)))

rm(rsdata411)
gc()
