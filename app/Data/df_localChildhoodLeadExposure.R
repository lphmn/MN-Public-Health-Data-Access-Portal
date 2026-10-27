
source("./Data/df_globalSchsacChb.R")

#* Childhood Lead Exposure and clean data
#* I will have to do some performance testing to see if it is better to do these calculations all here or to do the calculations based on selected parms
#* My assumption is to pre-compile all the calculations, loading will be a little slower but application interaction should be better 
df_leadRaw <-  read.csv("https://raw.githubusercontent.com/lphmn/data_publicState/refs/heads/main/MN_PHDAP/healthyHomes/childhoodLeadExposure.csv") |> 
  janitor::clean_names() |>
  
  #Removed MN because I am going to calculate it like region and chb
  # There is a difference between this and the MN total; I notified MDH on 5/23/24 that there 
  # MN total doesn't match the sum of the counties with respect to the below grouping
  # I checked this without any data manipulation and there seems to be an error in the numerator
  # The denominator matches
  dplyr::filter(location != "Minnesota") |>
  dplyr::select(indicator, indicator_type, year, location, age_group, ebll_description, num_tested, denominator) |>
  dplyr::left_join(df_globalSchsacChb, by = dplyr::join_by(location == county)) |> 
  
  #Determine MN total with respect to the following fields
  dplyr::group_by(indicator, indicator_type, year, age_group, ebll_description) |>
  dplyr::mutate(mnNumerator = sum(num_tested), mnDenominator = sum(denominator), mnPct = round(sum(num_tested) / sum(denominator) *100,2))  |> 
  dplyr::ungroup() |> 
  
  #Determine Region total with respect to the following fields
  dplyr::group_by(indicator, indicator_type, year, age_group, ebll_description, region) |> 
  dplyr::mutate(regionNumerator = sum(num_tested), regionDenominator = sum(denominator), regionPct = round(sum(num_tested) / sum(denominator) *100,2)) |> 
  dplyr::ungroup() |> 
  
  #Determine CHB total with respect to the following fields
  dplyr::group_by(indicator, indicator_type, year, age_group, ebll_description, chb) |> 
  dplyr::mutate(chbNumerator = sum(num_tested), chbDenominator = sum(denominator), chbPct = round(sum(num_tested) / sum(denominator) *100,2)) |> 
  dplyr::ungroup()

# When I source this I only want to pull the finally data frame which is why I remove the following
rm(df_globalSchsacChb)
