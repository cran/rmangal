#' rmangal
#'
#' A programmatic interface to the Mangal API <https://mangal-interactions.github.io/mangal-api/>.
#'
#' @docType package
#' @importFrom methods is
#' @name rmangal
#' @keywords internal
"_PACKAGE"

# HELPER FUNCTIONS
# Basic
server <- function() "https://mangal.io"
# server <- function() "http://localhost:8080" # dev purpose
base <- function() "/api/v2"
# bearer <- function() ifelse(file.exists(".httr-oauth"),
#   as.character(readRDS(".httr-oauth")), NA)
ua <- httr::user_agent("rmangal")

# Endpoints
endpoints <- function() {
  list(
    dataset = "/dataset",
    environment = "/environment",
    interaction = "/interaction",
    network = "/network",
    node = "/node",
    reference = "/reference",
    attribute = "/attribute",
    taxonomy = "/taxonomy",
    trait = "/trait"
  )
}

## Set memoise httr::GET
mem_get <- memoise::memoise(httr::GET)


# NULL to NA
null_to_na <- function(x) {
    if (is.list(x)) {
      lapply(x, null_to_na)
    } else {
      ifelse(is.null(x), NA, x)
    }
}

##
resp_raw <- function(x) jsonlite::fromJSON(
  httr::content(x, as = "text", encoding = "UTF-8"),
  simplifyVector = FALSE, flatten = TRUE)

## Response => data.frame
resp_to_df <- function(x) {
  if (is.null(x))
    x else do.call(rbind, lapply(null_to_na(x),
      function(x) as.data.frame(x, stringsAsFactors = FALSE)))
}

# flatten + fill
resp_to_df_flt <- function(x) {
  x <- null_to_na(x)
  ldf <- lapply(
    lapply(x, function(x) as.data.frame(x, stringsAsFactors = FALSE)),
    jsonlite::flatten)
  vnm <- unique(unlist(lapply(ldf, names)))
  ldf2 <- lapply(ldf, fill_df, vnm)
  do.call(rbind, ldf2)
}

fill_df <- function(x, nms) {
  id <- nms[! nms %in% names(x)]
  if (length(id)) x[id] <- NA
  x
}


# hh <- resp_to_spatial2(gg)
resp_to_spatial <- function(x, as_sf = FALSE, keep_geom = TRUE) {
  if (is.null(x)) {
    x
  } else {
     dat <- do.call(rbind, lapply(null_to_na(x),
        function(y) as.data.frame(
          y[names(y) != "geom"], stringsAsFactors = FALSE)
        ))
      if (keep_geom) {
        dat <- cbind(dat, do.call(rbind, lapply(x, handle_geom)))
      } else dat
      if (as_sf) resp_to_sf(dat) else dat
  }
}

handle_geom <- function(x) {
  # print(x$geom)
  if (is.null(x$geom)) {
    data.frame(
      geom_type = NA_character_,
      geom_lon = NA_real_,
      geom_lat = NA_real_
    )
  } else {
    tmp <- matrix(unlist(x$geom$coordinates), ncol = 2, byrow = TRUE)
    # names(tmp) <- paste0("geom_", c("lon", "lat"))
    out <- data.frame(
      geom_type = x$geom$type,
      stringsAsFactors = FALSE
    )
    out$geom_lon <- list(tmp[, 1])
    out$geom_lat <- list(tmp[, 2])
    out
  }
}

## Response => spatial -- sf required
resp_to_sf <- function(dat) {
  stop_if_missing_sf()
  if (nrow(dat) == 1) {
    spd <- switch_sf(dat)
  } else spd <- apply(dat, 1, switch_sf)
  sf::st_sf(
   dat[names(dat)[!grepl("geom_", names(dat))]], geom = sf::st_sfc(spd),
    crs = 4326, stringsAsFactors = FALSE
   )
}

## Build sf object based on geom.type
switch_sf <- function(x) {
  if (is.na(x$geom_type)) {
    sf::st_point(matrix(NA_real_, ncol = 2))
  } else {
    co <- cbind(
        as.numeric(unlist(x$geom_lon)),
        as.numeric(unlist(x$geom_lat))
    )
    switch(
      x$geom_type,
      Point = sf::st_point(co),
      Polygon = sf::st_polygon(list(co)),
      stop("Only `Point` and `Polygon` are supported.")
    )
  }
}


stop_if_missing_sf <- function(pkg = "sf") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("sf should be installed to use this functionality", call. = FALSE)
  }
}


#' Get entries based on foreign key
#'
#' @param endpoint `character` API entry point
#' @param ... foreign key column name with the id
#' @return
#' Object returned by [rmangal::get_gen()]
#' @details
#' See endpoints available with `endpoints()`
#' @keywords internal

get_from_fkey <- function(endpoint, verbose = TRUE, ...) {
  query <- list(...)
  # print(get_gen(endpoint = endpoint, query = query)$body)
  resp_to_df(
    get_gen(endpoint = endpoint, query = query, verbose = verbose)$body
  )
}

get_from_fkey_net <- function(endpoint, verbose = TRUE, ...) {
  query <- list(...)
  resp_to_spatial(
    get_gen(endpoint = endpoint, query = query, verbose = verbose)$body
  )
}

get_from_fkey_flt <- function(endpoint, verbose = TRUE, ...) {
  query <- list(...)
  resp_to_df_flt(
    get_gen(endpoint = endpoint, query = query, verbose = verbose)$body
  )
}



#' Generic API function to retrieve several entries
#'
#' @param endpoint `character` API entry point
#' @param query `list` list of parameters passed to the API
#' @param limit `integer` number of entries return by the API (max: 1000)
#' @param verbose `logical` print API code status on error; default: `TRUE`
#' @param ... Further named parameters passed to [httr::GET()].
#'
#' @return
#' Object of class `mgGetResponses`
#' @details
#' See endpoints available with `endpoints()`
#' @keywords internal

get_gen <- function(endpoint, query = NULL, limit = 100, verbose = TRUE, ...) {

  url <- httr::modify_url(server(), path = paste0(base(), endpoint))
  query <- as.list(query)

  # Add number of entries to the param
  query$count <- limit

  # First call used to set pages
  resp <- mem_get(url,
      config = httr::add_headers(`Content-type` = "application/json"), ua,
      query = query, ...)

  # Stop if server status is problematic
  httr::stop_for_status(resp)

  # Prep output object
  responses <- list()
  errors <- NULL


  # Get # pages
  tmp <- unlist(strsplit(httr::headers(resp)$"content-range", split = "\\D"))
  rg <- as.numeric(tmp[grepl("\\d", tmp)])
  pages <- rg[3L] %/% limit

  # Loop over pages
  for (page in 0:pages) {
    if (verbose)
      message("Data retrieval ", signif(100*(page+1)/(pages+1), 3), "%   \r",
        appendLF = FALSE)

    query$page <- page
    resp <- mem_get(url,
      config = httr::add_headers(`Content-type` = "application/json"), ua,
      query = query, ...)

    if (httr::http_error(resp)) {
      if (verbose) msg_request_fail(resp)
      responses[[page + 1]] <- list(body = NULL, response = resp)
      errors <- append(errors, page + 1)
    } else {
      responses[[page + 1]] <- list(body = resp_raw(resp), response = resp)
    }
  }
  if (verbose) empty_line()

  if (!is.null(errors))
    warning("Failed request(s) for page(s): ", paste0(errors, ", "))

  # check error here if desired;
  out <- list(
    body = unlist(lapply(responses, function(x) x$body), recursive = FALSE),
    response = lapply(responses, function(x) x$response)
  )
  class(out) <- "mgGetResponses"
  out
}


#' Generic API function to retrieve singletons
#'
#' @param endpoint `character` API entry point.
#' @param ids `numeric` vector of ids.
#' @param verbose `logical` print API code status on error; default: `TRUE`
#' @param ... httr options, see [httr::GET()].
#' @return
#' Object of class `mgGetResponses`
#' @details
#' See endpoints available with `endpoints()`
#' @keywords internal
get_singletons <- function(endpoint = NULL, ids = NULL, verbose = TRUE,
   ...) {

  stopifnot(!is.null(endpoint) & !is.null(ids))
  # Prep output object
  responses <- list(body = list(), response = list())
  errors <- NULL

  # Loop over ids
  for (i in seq_along(ids)) {
    if (verbose)  message("Processing id: ", ids[i], " \t", i, "/",
      length(ids), "  \r", appendLF = FALSE)
    # Set url
    url <- httr::modify_url(server(), path = paste0(base(), endpoint, "/",
      ids[i]))

    # Call on the API
    resp <- mem_get(url,
      config = httr::add_headers(`Content-type` = "application/json"), ua, ...)

    if (httr::http_error(resp)) {
      if (verbose) msg_request_fail(resp)
      errors <- append(errors, ids[i])
    } else {
      # coerce body to output desired
      responses$body[[i]] <- resp_raw(resp)
      responses$response[[i]] <- resp
    }
  }
  if (verbose) empty_line()

  if (!is.null(errors))
    warning("Failed request(s) for id(s): ", paste0(errors, ", "))

  class(responses) <- "mgGetResponses"
  responses
}


# PRINT/MESSAGES HELPERS

empty_line <- function() {
  message(paste(rep(" ", getOption("width") - 3), collapse = ""), "\r",
    appendLF = FALSE)
}


msg_request_fail <- function(resp) {
  sta <- httr::status_code(resp)
  warning("API request failed: error ", sta)
}


handle_query <- function(query, names_available) {
  if (is.character(query))
    return(list(q = query))
  if (!is.list(query))
    stop("`query` should either be a list or a character string.",
      call. = FALSE)
  if (length(query) > 1) {
    warning("Only the first element of the list is considered.", call. = FALSE)
    query <- query[1L]
  }
  if (! names(query) %in% names_available)
    stop("Only ", paste(names_available, collapse = ", "),
      " are valid names for custom queries.", call. = FALSE)
  query
}



percent_id <- function(y) round(100*sum(!is.na(y))/length(y))

print_taxo_ids <- function(x) {
  paste0(
    "* Current taxonomic IDs coverage for nodes of this network: \n  --> ",
    "ITIS: ", percent_id(x$taxonomy.tsn), "%, ",
    "BOLD: ", percent_id(x$taxonomy.bold), "%, ",
    "EOL: ", percent_id(x$taxonomy.eol), "%, ",
    "COL: ", percent_id(x$taxonomy.col), "%, ",
    "GBIF: ", percent_id(x$taxonomy.gbif), "%, ",
    "NCBI: ", percent_id(x$taxonomy.ncbi), "%\n"
  )
}

print_pub_info <- function(x) {
  paste0("* Published in ref #",  x$id, " DOI:", x$doi)
}

print_net_info <- function(net_id, dat_id, descr, n_edg, n_nod) {
  paste0(
    "* Network #", net_id, " included in dataset #", dat_id, "\n",
    "* Description: ", descr, "\n",
    "* Includes ", n_edg, " edges and ", n_nod, " nodes \n"
  )
}
