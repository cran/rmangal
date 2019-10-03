## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----load_packages-------------------------------------------------------
library(rmangal)
library(magrittr) # for the pipe %>%
library(tibble) # to use tibble (enhanced data frames)

## ------------------------------------------------------------------------
lagoon <- search_datasets(query = "lagoon")
class(lagoon)
lagoon

## ------------------------------------------------------------------------
lagoon_zetina <- search_datasets(list(ref_id = 22))
lagoon_zetina

## ------------------------------------------------------------------------
all_datasets <- search_datasets("")
glimpse(all_datasets)

## ------------------------------------------------------------------------
zetina_2003 <- search_references(doi = "10.1016/s0272-7714(02)00410-9")

## ------------------------------------------------------------------------
insect_coll <- search_networks(query="insect%")
glimpse(insect_coll)

## ------------------------------------------------------------------------
# List all interaction types available
avail_type()
comp_interac <- search_interactions(type="competition")
# Number of competition interactions in mangal
nrow(comp_interac)

## ------------------------------------------------------------------------
library(sf)
library(mapview)
library(USAboundaries)
area <- us_states(state = "california")
in_CA <- search_networks_sf(area)

mapView(in_CA, legend = FALSE) + mapview(sf::st_cast(area,"MULTILINESTRING"), color = "red", legend = FALSE)

## ------------------------------------------------------------------------
sr_ficus <- search_taxonomy("Ficus")

## ------------------------------------------------------------------------
glimpse(search_taxonomy(tsn = 28749))
glimpse(search_taxonomy(eol = 583069))

## ------------------------------------------------------------------------
sr_ficus2 <- search_nodes("Ficus")

## ------------------------------------------------------------------------
nets_lagoons <- lagoon %>% get_collection
nets_in_CA <- in_CA %>% get_collection
nets_competition <- comp_interac %>% get_collection

## ------------------------------------------------------------------------
nets_lagoons
class(nets_lagoons)

## ------------------------------------------------------------------------
names(nets_lagoons[[1]])
glimpse(nets_lagoons[[1]]$network)
glimpse(nets_lagoons[[1]]$nodes)
glimpse(nets_lagoons[[1]]$edges)
glimpse(nets_lagoons[[1]]$dataset)
glimpse(nets_lagoons[[1]]$reference)

## ------------------------------------------------------------------------
tsn <- c(837855, 169237)
mgn <- lapply(tsn, function(x) search_taxonomy(tsn = x)) %>%
  lapply(get_collection) %>%
  combine_mgNetworks
mgn

## ----as_sf---------------------------------------------------------------
# assuming sf and mapview are is loaded (as we did above)
mg_lag_sf <- search_datasets(query = 'lagoon') %>% get_collection(as_sf = TRUE)
class(mg_lag_sf[[1]]$network)
# let's combine all these sf object into a single one
mapView(mg_lag_sf[[1]]$network) + mapView(mg_lag_sf[[2]]$network)

## ----taxo----------------------------------------------------------------
library(taxize)
tsn_acer <- search_taxonomy("Acer")$taxonomy.tsn
classification(tsn_acer, db = "itis")

## ----igraph--------------------------------------------------------------
library(igraph)
mg_lagoons <- search_datasets(query = 'lagoon') %>% get_collection
# NB the line below returns a list of igraph objects
ig_lagoons <- as.igraph(mg_lagoons)
## Modularity analysis for the first network
modularity(ig_lagoons[[1]], membership(cluster_walktrap(ig_lagoons[[1]])))
## Degree values for all networks
lapply(ig_lagoons, degree)

## ----tidygraph-----------------------------------------------------------
library(tidygraph)
# NB the line below would not work with a mgNetworksCollection (use lapply)
tg_lagoons <-  as_tbl_graph(mg_lagoons[[1]]) %>%
  mutate(centrality_dg = centrality_degree(mode = 'in'))
tg_lagoons %E>% as_tibble
tg_lagoons %N>% as_tibble %>%
  select(original_name, taxonomy.tsn, centrality_dg)

## ----ggraph--------------------------------------------------------------
library(ggraph)
ggraph(tg_lagoons, layout = "stress") +
  geom_edge_parallel(end_cap = circle(.5), start_cap = circle(.5),
        arrow = arrow(length = unit(1, 'mm'), type = 'closed')) +
  geom_node_point(aes(colour = taxonomy.rank), size = 8) +
  theme_graph(background = "grey40", foreground = NA, text_colour = 'white')

## ---- message = FALSE----------------------------------------------------
# library(RefManageR)
# tmpf <- tempfile(, fileext = ".bib")
search_datasets(query = 'lagoon') %>%
  get_collection %>% get_citation %>% cat(sep = "\n")
# file = tmpf)
# ReadBib(tmpf)

