library(neonstore)
library(dplyr)
library(neonUtilities)

products <- neon_products()

# names of everything in store
index_ <- neon_index()
unique(index$table)


#beetles data
neonstore::neon_download(product="DP1.10022.001")
neonstore::neon_store(product="DP1.10022.001")

beetles <- neon_read(table = "bet_expertTaxonomistIDProcessed-basic", product = "DP1.10022.001")
head(beetles)

# temperature data
neonstore::neon_download(product="DP4.00001.001")
neonstore::neon_store(product="DP4.00001.001")
temp <- neon_read(table = "wss_daily_temp", product = "DP4.00001.001")
head(temp)

# site management and event reporting
neonstore::neon_download(product="DP1.10111.001")
neonstore::neon_store(product="DP1.10111.001")

site_man <- neon_read(table = "sim_eventData-basic", product = "DP1.10111.001")
head(site_man)

site_man %>%
  filter(methodTypeChoice == "tillage-conventional" | methodTypeChoice == "tillage-conservation" | methodTypeChoice == "tillage-other")
