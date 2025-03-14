# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

shfdbpath <- "P:/k2_stat_heartfailure/Projects/20210525_shfdb4/dm/"
datadate <- "20240423"

global_cols <- RColorBrewer::brewer.pal(7, "Dark2")
global_gridcolor <- rgb(235, 235, 235, 100, maxColorValue = 355)
