# State Community Health Services Advisory Committee as of 1_17_2024
# Updated to use the current data repository location.
# Legacy URL fallback is retained in case the original source is temporarily accessible.

mn_regions_url <- "https://raw.githubusercontent.com/lphmn/data_publicState/main/MN_Regions/SchsacRegionChbCounty.csv"
legacy_region_url <- "https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/State%20Community%20Health%20Services%20Advisory%20Committee%20as%20of%201_17_2024.csv"
legacy_chb_url <- "https://raw.githubusercontent.com/quinCHB/Public-Data-Sources/main/MN%20SCHSAC%20%26%20CHB%20Regions/Community%20Health%20Board%20as%20of%201_17_2024.csv"

read_csv_with_fallback <- function(url, fallback_url) {
  result <- tryCatch(read.csv(url), error = function(e) NULL)
  if (is.null(result)) {
    result <- read.csv(fallback_url)
  }
  result
}

# Prefer the reorganized public data repo, but keep the old repo as a fallback.
# SchsacRegionChbCounty.csv contains both county/region and county/CHB mappings.
df_region_lookup <- read_csv_with_fallback(mn_regions_url, legacy_region_url)

# Handle the new file format if it includes both region and CHB in one file.
if (all(c("County", "Region", "CHB") %in% names(df_region_lookup))) {
  df_schsacRaw <- unique(df_region_lookup[, c("County", "Region")][!is.na(df_region_lookup$County) & !is.na(df_region_lookup$Region), , drop = FALSE])
  df_chbRaw <- unique(df_region_lookup[, c("County", "CHB")][!is.na(df_region_lookup$County) & !is.na(df_region_lookup$CHB), , drop = FALSE])
} else {
  df_schsacRaw <- read_csv_with_fallback(mn_regions_url, legacy_region_url) |> janitor::clean_names()
  df_chbRaw <- read.csv(legacy_chb_url) |> janitor::clean_names()
}

# Some CHB's are at the city level and not the county level which is why I do a full join
# And I still want them to display in the global selectInput choice
# Use a full join to preserve county-to-CHB relationships even when the data source is sparse.
df_globalSchsacChb <- df_schsacRaw |> 
  dplyr::full_join(df_chbRaw, by = dplyr::join_by(county == county))

# When I source this I only want to pull the final data frame which is why I remove the following
rm(df_chbRaw)
rm(df_schsacRaw)
rm(df_region_lookup)
