# State Community Health Services Advisory Committee as of 1_17_2024
df_globalSchsacChb <- read.csv(
  "https://raw.githubusercontent.com/lphmn/data_publicState/main/MN_Regions/SchsacRegionChbCounty.csv"
) |>
  janitor::clean_names()
