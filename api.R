# check from itpde_exp_sec
# fix string = "all"

library(RPostgres)
library(pool)
library(openapi)

# Read credentials from file excluded in .gitignore
readRenviron("/tradestatistics/credentials.txt")

con <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("TRADESTATISTICS_SQL_NAME"),
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

on.exit(poolClose(con))

# Static data -------------------------------------------------------------

countries <- function() {
  dbGetQuery(con, "select * from dgd_countries")
}

countries_colours <- function() {
  dbGetQuery(con, "select * from dgd_colours")
}

industries <- function() {
  dbGetQuery(con, "select * from itpd_industries")
}

sectors <- function() {
  dbGetQuery(con, "select * from itpd_sectors")
}

sectors_colours <- function() {
  dbGetQuery(con, "select * from itpd_colours")
}

# create vectors by continent to filter by using meta variables like americas, africa, etc ----

countries_africa <- unlist(dbGetQuery(con, "select dynamic_code from dgd_countries inner join dgd_colours on dgd_countries.dynamic_code = dgd_colours.iso3_dynamic where region_id = 2"))

countries_americas <- unlist(dbGetQuery(con, "select dynamic_code from dgd_countries inner join dgd_colours on dgd_countries.dynamic_code = dgd_colours.iso3_dynamic where region_id in (3, 4, 10, 12)"))

countries_asia <- unlist(dbGetQuery(con, "select dynamic_code from dgd_countries inner join dgd_colours on dgd_countries.dynamic_code = dgd_colours.iso3_dynamic where region_id in (5, 6, 7, 9, 13, 14)"))

countries_europe <- unlist(dbGetQuery(con, "select dynamic_code from dgd_countries inner join dgd_colours on dgd_countries.dynamic_code = dgd_colours.iso3_dynamic where region_id = 8"))

countries_oceania <- unlist(dbGetQuery(con, "select dynamic_code from dgd_countries inner join dgd_colours on dgd_countries.dynamic_code = dgd_colours.iso3_dynamic where region_id = 11"))

# Clean inputs ----

clean_char_input <- function(x, importer, j) {
  year <- toupper(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  year <- if (grepl("^e-", year)) {
    gsub("[^[:alpha:][:digit:]]-", "", year)
  } else {
    gsub("[^[:alpha:]]-", "", year)
  }
  substr(year, importer, j)
}

clean_num_input <- function(x, importer, j) {
  year <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  if (year != "all") {
    year <- gsub("[^[:digit:]]", "", year)
  }
  substr(year, importer, j)
}

# Checks ------------------------------------------------------------------

## min and max year with data ----

min_year <- function() {
  1986L
}

max_year <- function() {
  2022L
}

check_year <- function(year) {
  if (nchar(year) != 4 || !year >= min_year() || !year <= max_year()) {
    return("The specified year is not a valid integer value. Read the documentation: tradestatistics.io")
  }
  year
}

check_importer <- function(imp) {
  if (imp == "ALL") {
    return("all")
  }

  if (imp %in% c("AF", "AM", "AS", "EU", "OC")) {
    return(imp)
  }

  if (!nchar(imp) <= 5 || !imp %in% countries()$dynamic_code) {
    return("The specified importer is not a valid code or alias. Read the documentation: tradestatistics.io")
  }

  paste0("'", imp, "'")
}

check_exporter <- function(exp) {
  if (exp == "ALL") {
    return("all")
  }

  if (exp %in% c("AF", "AM", "AS", "EU", "OC")) {
    return(exp)
  }

  if (!nchar(exp) <= 5 || !exp %in% countries()$dynamic_code) {
    return("The specified exporter is not a valid code or alias. Read the documentation: tradestatistics.io")
  }

  paste0("'", exp, "'")
}

check_industry <- function(ind) {
  if (any(ind %in% c("all","ALL"))) {
    return("all")
  }

  if (!nchar(ind) <= 3 || !ind %in% industries()$industry_id) {
    return("The specified industry is not a valid code. Read the documentation: tradestatistics.io")
  }

  ind
}

check_sector <- function(sec) {
  if (any(sec %in% c("all","ALL"))) {
    return("all")
  }

  if (!nchar(sec) <= 3 || !sec %in% sectors()$broad_sector_id) {
    return("The specified sector code is not a valid code. Read the documentation: tradestatistics.io")
  }
  sec
}

multiple_countries <- function(r) {
  switch(r,
    "AF" = paste(vapply(countries_africa, function(x) paste0("'", x, "'"), character(1)), collapse = ","),
    "AM" = paste(vapply(countries_americas, function(x) paste0("'", x, "'"), character(1)), collapse = ","),
    "AS" = paste(vapply(countries_asia, function(x) paste0("'", x, "'"), character(1)), collapse = ","),
    "EU" = paste(vapply(countries_europe, function(x) paste0("'", x, "'"), character(1)), collapse = ","),
    "OC" = paste(vapply(countries_oceania, function(x) paste0("'", x, "'"), character(1)), collapse = ",")
  )
}

no_data <- function(table, year = NA, importer = NA, exporter = NA, sector = NA, industry = NA) {
  if (any(table %in% c("itpde", "itpds"))) {
    d <- data.frame(
      year = year,
      importer_iso3_dynamic = importer,
      exporter_iso3_dynamic = exporter,
      broad_sector_id = sector,
      industry_id = industry,
      observation = "No data available for these filtering parameters"
    )
  }

  if (any(table %in% c("itpde_imp", "itpds_imp"))) {
    d <- data.frame(
      year = year,
      importer_iso3_dynamic = importer,
      observation = "No data available for these filtering parameters"
    )
  }

  if (any(table %in% c("itpde_imp_exp", "itpds_imp_exp"))) {
    d <- data.frame(
      year = year,
      importer_iso3_dynamic = importer,
      exporter_iso3_dynamic = exporter,
      observation = "No data available for these filtering parameters"
    )
  }

  if (any(table %in% c("itpde_imp_exp_sec", "itpds_imp_exp_sec"))) {
    d <- data.frame(
      year = year,
      importer_iso3_dynamic = importer,
      exporter_iso3_dynamic = exporter,
      broad_sector_id = sector,
      observation = "No data available for these filtering parameters"
    )
  }

  d
}

# Data functions ----------------------------------------------------------

## ITPD-E/S SEC ----

itpd_sec <- function(year, sector, table) {
  year <- check_year(as.integer(year))
  sector <- check_sector(clean_num_input(sector, 1, 3))

  if (sector != "all") {
    query <- sprintf("select * from %s where year = %s and broad_sector_id = %s", table, year, sector)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, sector = sector)
  }

  chunk
}

itpde_sec <- function(year, sector, table = "itpde_sec") { itpd_sec(year, sector, table) }
itpds_sec <- function(year, sector, table = "itpds_sec") { itpd_sec(year, sector, table) }

## ITPD-E/S IND ----

itpd_ind <- function(year, industry, table) {
  year <- check_year(as.integer(year))
  industry <- check_industry(clean_num_input(industry, 1, 3))

  if (industry != "all") {
    query <- sprintf("select * from %s where year = %s and industry_id = %s", table, year, industry)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, industry = industry)
  }

  chunk
}

itpde_ind <- function(year, industry, table = "itpde_ind") { itpd_ind(year, industry, table) }
itpds_ind <- function(year, industry, table = "itpds_ind") { itpd_ind(year, industry, table) }

## ITPD-E/S IMP ----

itpd_imp <- function(year, importer, table) {
  year <- check_year(as.integer(year))
  importer <- check_importer(clean_char_input(importer, 1, 5))

  if (nchar(importer) == 2L) {
    importer <- multiple_countries(importer)
  }

  if (importer != "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s)", table, year, importer)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, importer = importer)
  }

  chunk
}

itpde_imp <- function(year, importer, table = "itpde_imp") { itpd_imp(year, importer, table) }
itpds_imp <- function(year, importer, table = "itpds_imp") { itpd_imp(year, importer, table) }

## ITPD-E/S IMP-SEC ----

itpd_imp_sec <- function(year, importer, sector, table) {
  year <- check_year(as.integer(year))
  importer <- check_importer(clean_char_input(importer, 1, 5))
  sector <- check_sector(clean_char_input(sector, 1, 3))

  if (nchar(importer) == 2L) {
    importer <- multiple_countries(importer)
  }

  if (importer != "all" & sector != "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s) and broad_sector_id = %s", table, year, importer, sector)
  } else if (importer != "all" & sector == "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s)", table, year, importer)
  } else if (importer == "all" & sector != "all") {
    query <- sprintf("select * from %s where year = %s and broad_sector_id = %s", table, year, sector)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, importer = importer, industry = importer)
  }

  chunk
}

itpde_imp_sec <- function(year, importer, sector, table = "itpde_imp_sec") { itpd_imp_sec(year, importer, sector, table) }
itpds_imp_sec <- function(year, importer, sector, table = "itpds_imp_sec") { itpd_imp_sec(year, importer, sector, table) }

## ITPD-E/S IMP-IND ----

itpd_imp_ind <- function(year, importer, industry, table) {
  year <- check_year(as.integer(year))
  importer <- check_importer(clean_char_input(importer, 1, 5))
  industry <- check_industry(clean_char_input(industry, 1, 3))

  if (nchar(importer) == 2L) {
    importer <- multiple_countries(importer)
  }

  if (importer != "all" & industry != "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s) and industry_id = %s", table, year, importer, industry)
  } else if (importer != "all" & industry == "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s)", table, year, importer)
  } else if (importer == "all" & industry != "all") {
    query <- sprintf("select * from %s where year = %s and industry_id = %s", table, year, industry)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, importer = importer, industry = importer)
  }

  chunk
}

itpde_imp_ind <- function(year, importer, industry, table = "itpde_imp_ind") { itpd_imp_ind(year, importer, industry, table) }
itpds_imp_ind <- function(year, importer, industry, table = "itpds_imp_ind") { itpd_imp_ind(year, importer, industry, table) }

## ITPD-E/S IMP-EXP ----

itpd_imp_exp <- function(year, importer, exporter, table) {
  year <- check_year(as.integer(year))
  importer <- check_importer(clean_char_input(importer, 1, 5))
  exporter <- check_exporter(clean_char_input(exporter, 1, 5))

  if (nchar(importer) == 2L) {
    importer <- multiple_countries(importer)
  }

  if (nchar(exporter) == 2L) {
    exporter <- multiple_countries(exporter)
  }

  if (importer != "all" & exporter != "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s) and exporter_iso3_dynamic in (%s)", table, year, importer, exporter)
  } else if (importer != "all" & exporter == "all") {
    query <- sprintf("select * from %s where year = %s and importer_iso3_dynamic in (%s)", table, year, importer)
  } else if (importer == "all" & exporter != "all") {
    query <- sprintf("select * from %s where year = %s and exporter_iso3_dynamic in (%s)", table, year, exporter)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, importer = importer, exporter = exporter)
  }

  chunk
}

itpde_imp_exp <- function(year, importer, exporter, table = "itpde_imp_exp") { itpd_imp_exp(year, importer, exporter, table) }
itpds_imp_exp <- function(year, importer, exporter, table = "itpds_imp_exp") { itpd_imp_exp(year, importer, exporter, table) }

## ITPD-E/S IMP-EXP ----

itpd_imp_exp_sec <- function(year, importer, exporter, sector, table) {
  year <- check_year(as.integer(year))
  importer <- check_importer(clean_char_input(importer, 1, 5))
  exporter <- check_exporter(clean_char_input(exporter, 1, 5))
  sector <- check_sector(clean_char_input(sector, 1, 3))

  if (nchar(importer) == 2L) {
    importer <- multiple_countries(importer)
  }

  if (nchar(exporter) == 2L) {
    exporter <- multiple_countries(exporter)
  }

  conditions <- sprintf("year = %s", year)

  if (importer != "all") conditions <- c(conditions, sprintf("importer_iso3_dynamic in (%s)", importer))
  if (exporter != "all") conditions <- c(conditions, sprintf("exporter_iso3_dynamic in (%s)", exporter))
  if (sector != "all") conditions <- c(conditions, sprintf("broad_sector_id = %s", sector))
  
  query <- sprintf("select * from %s where %s", table, paste(conditions, collapse = " and "))

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, importer = importer, exporter = exporter, sector = sector)
  }

  chunk
}

itpde_imp_exp_sec <- function(year, importer, exporter, sector, table = "itpde_imp_exp_sec") { itpd_imp_exp_sec(year, importer, exporter, sector, table) }
itpds_imp_exp_sec <- function(year, importer, exporter, sector, table = "itpds_imp_exp_sec") { itpd_imp_exp_sec(year, importer, exporter, sector, table) }

## ITPD-E/S EXP ----

itpd_exp <- function(year, exporter, table) {
  year <- check_year(as.integer(year))
  exporter <- check_exporter(clean_char_input(exporter, 1, 5))

  if (nchar(exporter) == 2L) {
    exporter <- multiple_countries(exporter)
  }

  if (exporter != "all") {
    query <- sprintf("select * from %s where year = %s and exporter_iso3_dynamic in (%s)", table, year, exporter)
  } else {
    query <- sprintf("select * from %s where year = %s", table, year)
  }

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, exporter = exporter)
  }

  chunk
}

itpde_exp <- function(year, exporter, table = "itpde_exp") { itpd_exp(year, exporter, table) }
itpds_exp <- function(year, exporter, table = "itpds_exp") { itpd_exp(year, exporter, table) }

## ITPD-E/S ----

itpd <- function(year, importer, exporter, sector, industry, table) {
  year <- check_year(as.integer(year))
  importer <- check_importer(clean_char_input(importer, 1, 5))
  exporter <- check_exporter(clean_char_input(exporter, 1, 5))
  sector <- check_sector(clean_char_input(sector, 1, 3))
  industry <- check_industry(clean_char_input(industry, 1, 3))

  if (nchar(importer) == 2L) {
    importer <- multiple_countries(importer)
  }

  if (nchar(exporter) == 2L) {
    exporter <- multiple_countries(exporter)
  }

  conditions <- sprintf("year = %s", year)
  
  if (importer != "all") conditions <- c(conditions, sprintf("importer_iso3_dynamic in (%s)", importer))
  if (exporter != "all") conditions <- c(conditions, sprintf("exporter_iso3_dynamic in (%s)", exporter))
  if (sector != "all") conditions <- c(conditions, sprintf("broad_sector_id = %s", sector))
  if (industry != "all") conditions <- c(conditions, sprintf("industry_id = %s", industry))
  
  query <- sprintf("select * from %s where %s", table, paste(conditions, collapse = " and "))

  chunk <- dbGetQuery(con, query)

  if (nrow(chunk) == 0) {
    chunk <- no_data(table, year = year, importer = importer, exporter = exporter, sector = sector, industry = industry)
  }

  chunk
}

itpde <- function(year, importer, exporter, sector, industry, table = "itpde") { itpd(year, importer, exporter, sector, industry, table) }
itpds <- function(year, importer, exporter, sector, industry, table = "itpds") { itpd(year, importer, exporter, sector, industry, table) }

# API ----

api <- api_init(
  title = "Open Trade Statistics API",
  version = "4.0.0",
  description = "International trade data available with different levels of aggregation"
)

api <- api_get(api, "/", function() {
  paste("Hello World! Welcome to Open Trade Statistics API. Go to https://api.tradestatistics.io/__docs__/ or use the R client!")
})

## countries ----

api <- api_get(api, "/countries", function() {
  countries()
})

api <- api_get(api, "/countries_colours", function() {
  countries_colours()
})

# importers-exporters ----

api <- api_get(api, "/importers", function(year = NA) {
  dbGetQuery(con, sprintf("select importer_iso3_dynamic from itpds_imp where year = %s", check_year(as.integer(year))))
})

api <- api_get(api, "/exporters", function(year = NA) {
  dbGetQuery(con, sprintf("select exporter_iso3_dynamic from itpds_exp where year = %s", check_year(as.integer(year))))
})

## years ----

api <- api_get(api, "/years", function() {
  data.frame(year = c(min_year(), max_year()))
})

## sectors ----

api <- api_get(api, "/sectors", function() {
  sectors()
})

api <- api_get(api, "/sectors_colours", function() {
  sectors_colours()
})

## industries ----

api <- api_get(api, "/industries", function() {
  industries()
})

## SEC ----

api <- api_get(api, "/itpde_sec", function(year, sector) {
  itpde_ind(year, sector, table = "itpde_sec")
})

api <- api_get(api, "/itpds_sec", function(year, sector) {
  itpde_ind(year, sector, table = "itpds_sec")
})

## IND ----

api <- api_get(api, "/itpde_ind", function(year, industry) {
  itpde_ind(year, industry, table = "itpde_ind")
})

api <- api_get(api, "/itpds_ind", function(year, industry) {
  itpde_ind(year, industry, table = "itpds_ind")
})

## IMP ----

api <- api_get(api, "/itpde_imp", function(year, importer) {
  itpde_imp(year, importer, table = "itpde_imp")
})

api <- api_get(api, "/itpds_imp", function(year, importer) {
  itpds_imp(year, importer, table = "itpds_imp")
})

## EXP ----

api <- api_get(api, "/itpde_exp", function(year, exporter) {
  itpde_exp(year, exporter, table = "itpde_exp")
})

api <- api_get(api, "/itpds_exp", function(year, exporter) {
  itpds_exp(year, exporter, table = "itpds_exp")
})

## IMP-SEC ----

api <- api_get(api, "/itpde_imp_sec", function(year, importer, sector) {
  itpde_imp_sec(year, importer, sector, table = "itpde_imp_sec")
})

api <- api_get(api, "/itpds_imp_sec", function(year, importer, sector) {
  itpde_imp_sec(year, importer, sector, table = "itpds_imp_sec")
})

## EXP-SEC ----

api <- api_get(api, "/itpde_exp_sec", function(year, exporter, sector) {
  itpde_exp_sec(year, exporter, sector, table = "itpde_exp_sec")
})

api <- api_get(api, "/itpds_exp_sec", function(year, exporter, sector) {
  itpde_exp_sec(year, exporter, sector, table = "itpds_exp_sec")
})

## IMP-IND ----

api <- api_get(api, "/itpde_imp_ind", function(year, importer, industry) {
  itpde_imp_ind(year, importer, industry, table = "itpde_imp_ind")
})

api <- api_get(api, "/itpds_imp_ind", function(year, importer, industry) {
  itpde_imp_ind(year, importer, industry, table = "itpds_imp_ind")
})

## EXP-IND ----

api <- api_get(api, "/itpde_exp_ind", function(year, exporter, industry) {
  itpde_imp_ind(year, exporter, industry, table = "itpde_exp_ind")
})

api <- api_get(api, "/itpds_exp_ind", function(year, exporter, industry) {
  itpds_exp_ind(year, exporter, industry, table = "itpds_exp_ind")
})

## IMP-EXP ----

api <- api_get(api, "/itpde_imp_exp", function(year, importer, exporter) {
  itpde_imp_exp(year, importer, exporter, table = "itpde_imp_exp")
})

api <- api_get(api, "/itpds_imp_exp", function(year, importer, exporter) {
  itpds_imp_exp(year, importer, exporter, table = "itpds_imp_exp")
})

## IMP-EXP-SEC ----

api <- api_get(api, "/itpde_imp_exp_sec", function(year, importer, exporter, sector) {
  itpde_imp_exp_sec(year, importer, exporter, sector, table = "itpde_imp_exp_sec")
})

api <- api_get(api, "/itpds_imp_exp_sec", function(year, importer, exporter, sector) {
  itpds_imp_exp_sec(year, importer, exporter, sector, table = "itpds_imp_exp_sec")
})

## FULL TABLES ----

api <- api_get(api, "/itpde", function(year, importer, exporter, sector, industry) {
  itpde(year, importer, exporter, sector, industry, table = "itpde")
})

api <- api_get(api, "/itpds", function(year, importer, exporter, sector, industry) {
  itpds(year, importer, exporter, sector, industry, table = "itpds")
})

# Available tables ----

api <- api_get(api, "/tables", function() {
  data.frame(
    table = c(
      "countries",
      "countries_colours",
      "importers",
      "exporters",
      "sectors",
      "sectors_colours",
      "industries",
      "years",
      
      "itpde",
      "itpde_imp",
      "itpde_exp",
      "itpde_imp_exp",
      "itpde_imp_sec",
      "itpde_imp_ind",
      "itpde_exp_sec",
      "itpde_exp_ind",
      "itpde_sec",
      "itpde_ind",
      "itpde_imp_exp_sec",

      "itpds",
      "itpds_imp",
      "itpds_exp",
      "itpds_imp_exp",
      "itpds_imp_sec",
      "itpds_imp_ind",
      "itpds_exp_sec",
      "itpds_exp_ind",
      "itpds_sec",
      "itpds_ind",
      "itpds_imp_exp_sec"
    ),

    description = c(
      "Countries",
      "Contries coloured by continent (for visualization)",
      "Importers per year",
      "Exporters per year",
      "Broad sectors",
      "Broad sectors with colour",
      "Industries",
      "Years",

      "International Trade and Production for Estimation",
      "International Trade and Production for Estimation, aggregated by year and importer",
      "International Trade and Production for Estimation, aggregated by year and exporter",
      "International Trade and Production for Estimation, aggregated by year, importer, and exporter",
      "International Trade and Production for Estimation, aggregated by year, importer, and sector",
      "International Trade and Production for Estimation, aggregated by year, importer, and industry",
      "International Trade and Production for Estimation, aggregated by year, exporter, and sector",
      "International Trade and Production for Estimation, aggregated by year, exporter, and industry",
      "International Trade and Production for Estimation, aggregated by year and sector",
      "International Trade and Production for Estimation, aggregated by year and industry",
      "International Trade and Production for Estimation, aggregated by year, importer, exporter, and sector",

      "International Trade and Production for Simulation",
      "International Trade and Production for Simulation, aggregated by year and importer",
      "International Trade and Production for Simulation, aggregated by year and exporter",
      "International Trade and Production for Simulation, aggregated by year, importer, and exporter",
      "International Trade and Production for Simulation, aggregated by year, importer, and sector",
      "International Trade and Production for Simulation, aggregated by year, importer, and industry",
      "International Trade and Production for Simulation, aggregated by year, exporter, and sector",
      "International Trade and Production for Simulation, aggregated by year, exporter, and industry",
      "International Trade and Production for Simulation, aggregated by year and sector",
      "International Trade and Production for Simulation, aggregated by year and industry",
      "International Trade and Production for Simulation, aggregated by year, importer, exporter, and sector"
    ),

    source = c(
      rep("Derived from USITC", 8L),
      rep("USITC", 22L)
    )
  )
})

# RUN API ----

api_run(api, host = "127.0.0.1", port = 5000, debug = TRUE)
