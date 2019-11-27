# Regional Autonomy GIS data wrangling

# Setup
rm(list=ls())

library(magrittr)
library(ggplot2)

# Load data
epr <- rio::import(paste0(here::here(), "/data/EPR-2018.1.1.csv"))
geo_epr <- rio::import(paste0(here::here(), "/data/GeoEPR-2018.1.1.csv"))
geom <- sf::st_read(paste0(here::here(), "/data/GeoEPR-2018.1.1/GeoEPR.shp")) %>%
  dplyr::select(group, geometry)

mali_conflict <- rio::import(paste0(here::here(), "/data/1997-01-01-2003-12-31-Mali.csv")) %>%
  dplyr::mutate(time = ifelse(year < 2000, 0, 1))
write.csv(mali_conflict, paste0(here::here(), "/data/mali_conflict.csv"))

health <- rio::import(paste0(here::here(), "/data/health.xlsx")) %>%
  dplyr::mutate(`DATE OUVERTURE` = stringr::str_extract(`DATE OUVERTURE`, "^.{4}"))
write.csv(health, paste0(here::here(), "/data/health.csv"))

arc_conf <- dplyr::as_data_frame(rio::import(paste0(here::here(), "/data/final_conflict.xlsx")))
xtable::xtable(arc_conf)


# Subset data
group_t <- epr %>%
  dplyr::filter(reg_aut == "TRUE") %>%
  dplyr::distinct(group) %>%
  dplyr::pull()

group_f <- epr %>%
  dplyr::filter(reg_aut == "FALSE") %>%
  dplyr::distinct(group) %>%
  dplyr::pull()

group_t <- dplyr::as_tibble(group_t)
group_f <- dplyr::as_tibble(group_f)

group_aut <- group_t %>%
  dplyr::filter(value %in% unique(group_f$value))

epr_sub <- epr %>%
  dplyr::filter(group %in% unique(group_aut$value))

geo_epr_sub <- geo_epr %>%
  dplyr::filter(group %in% unique(group_aut$value)) %>%
  dplyr::select(-from, -to)

epr_join <- dplyr::left_join(epr_sub, geo_epr_sub)
unique(epr_join$statename)

af_sub <- c("Mali", "Nigeria", "Chad", "Congo, DRC", "Comoros", "Sudan", "Zambia", "Libya (Tripolitania, Cyrenaica, Fezzan)",
            "Angola", "Ethiopia", "Congo", "Central African Republic", "Niger")

epr_join_af <- epr_join %>%
  dplyr::filter(statename %in% c(af_sub)) %>% 
  dplyr::group_by(statename, from, to, group) %>%
  dplyr::summarise_all(dplyr::funs(dplyr::first(na.omit(.)))) %>% 
  dplyr::filter(status != "SELF-EXCLUSION" &
                  !is.na(reg_aut) &
                  type != "Urban") %>%
  dplyr::group_by(statename, group) %>%
  tidyr::complete(group, from = min(from):2019) %>%
  tidyr::fill(gwid, groupid, gwgroupid, umbrella, size, status, reg_aut, sqkm, type, the_geom) %>%
  dplyr::select(-to, -the_geom) %>%
  dplyr::rename(year = from)

final <- sf::st_sf(dplyr::left_join(epr_join_af, geom)) %>%
  dplyr::filter(statename %in% c("Mali"))

write.csv(final, paste0(here::here(), "/data/epr_af.csv"))
sf::st_write(final, paste0(here::here(), "/data/epr_af.shp"), delete_dsn = TRUE)

#rm(epr, epr_join, epr_sub, geo_epr, geo_epr_sub, group_aut, group_f, group_t, af_sub)