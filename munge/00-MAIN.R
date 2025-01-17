# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Load data ---------------------------------------------------------------

load(here(shfdbpath, "data/v421/rsdata421.RData"))

# Meta data ect -----------------------------------------------------------

metavars <- read.xlsx(here(shfdbpath, "metadata/meta_variables.xlsx"))
load(here(paste0(shfdbpath, "data/v421/meta_statreport.RData")))

# Munge data --------------------------------------------------------------

source(here("munge/01-vars.R"))
source(here("munge/02-pop-selection.R"))
source(here("munge/03-pdr-meds.R"))
source(here("munge/04-fix-vars.R"))
source(here("munge/05-mi.R"))

# Cache/save data ---------------------------------------------------------

save(
  file = here("data/clean-data/data.RData"),
  list = c(
    "rsdata",
    "imprsdata",
    "imprsdatafemale",
    "imprsdatamale",
    "flow",
    "modvars",
    "stratavars",
    "tabvars",
    "outvars",
    "metavars",
    "metalm",
    "deathmeta",
    "outcommeta"
  )
)

# create workbook to write tables to Excel
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, sheet = "Information")
openxlsx::writeData(wb, sheet = "Information", x = "Tables in xlsx format for tables in Statistical report: Etiology of heart failure across the ejection fraction spectrum and association with prognosis: an analysis from the Swedish Heart Failure Registry", rowNames = FALSE, keepNA = FALSE)
openxlsx::saveWorkbook(wb,
  file = here::here("output/tabs/tables.xlsx"),
  overwrite = TRUE
)

# create powerpoint to write figs to PowerPoint
figs <- officer::read_pptx()
print(figs, target = here::here("output/figs/figs.pptx"))
