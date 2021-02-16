library(neonstore)

# download all of the beetle data
neonstore::neon_download(product="DP1.10022.001")
# importing into database
neonstore::neon_store(product="DP1.10022.001")

# add other neon products?
# snapshots of weather data for neon sites from EFI group? not a nice interface for this 
