#issues with census data
#have 0-4, and have 5 - but for 5yos, the margin of error is high
#some zip codes (low population zips) are missing data

#Issues with lead data
#zip codes are not consistent across years


library(readxl)
library(tidyverse)
library(tidycensus)
library(dplyr)
census_api_key("YOUR_API_KEY")


file_path1 <- "data/BloodLead-tests-2000-2018.xlsx" 
sheet_names1 <- excel_sheets(file_path)
file_path2 <- "data/Balt-Lead-testing.xlsx" 
sheet_names2 <- c("City_2019", "City_2020", "City_2021")

# Using lapply (base R)
sheets_data1 <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet, skip = 5)
})

sheets_data2 <- lapply(sheet_names2, function(sheet) {
  read_excel(file_path2, sheet = sheet, skip = 2)
})


# Assign names to the list elements
names(sheets_data1) <- sheet_names1
names(sheets_data2) <- sheet_names2

sheets_data1 <- lapply(sheets_data1, function(df) {
  df %>% select(-Total) %>% mutate(ZIPCODE = as.numeric(ZIPCODE))
})

combined_sheets <- c(sheets_data1, sheets_data2)


# Define your new column names
new_colnames <- c("zip", "lead0_4", "lead5_9", "lead10_19",
                  "lead20_34", "lead35_49", "lead50_plus")



# Assuming all_sheets_data is a named list with names like "City_2000", "City_2001", etc.
combined_sheets <- mapply(function(df, name) {
  colnames(df) <- new_colnames
  df <- df %>% 
    filter(zip != "Total", zip != "No Zip") %>%
    mutate(year = as.integer(gsub("City_", "", name))) %>% 
    rowwise() %>%
    mutate(total = sum(c_across(starts_with("lead")), na.rm = TRUE)) %>%
    ungroup()
  return(df)
}, combined_sheets, names(combined_sheets), SIMPLIFY = FALSE)




# Optionally: assign each dataframe to a variable in your global environment
list2env(combined_sheets, envir = .GlobalEnv)


#create long dataset
lead_df_long <- bind_rows(combined_sheets) %>% 
  select(-starts_with("lead")) %>% 
  group_by(zip) %>% 
  mutate(total_allyears = sum(total), 
         appearances = n()
  ) %>% 
  ungroup()





#get denominator
zips_of_interest <- unique(lead_df_long$zip)
zips_of_interest

vars <- c(
  "B01001_003E",
  "B01001_027E",
  "B01003_001" #total population
)

census_raw <- get_acs(
  geography = "zcta",
  variables = vars,
 # state = "MD",
  year = 2023,
  survey = "acs5",
  output = "wide"
)


# test <- census_raw %>% 
#   filter(GEOID == "21287")

census <- census_raw %>%
  filter(GEOID %in% zips_of_interest) %>%
  mutate(pop_under5_2023 = B01001_003E + B01001_027E,
         est_pop5andunder_2023 = round(B01003_001E*.08),
         GEOID = as.numeric(GEOID)) %>% 
  mutate(est_pop5andunder_2023 = ifelse(pop_under5_2023>est_pop5andunder_2023, 
                                        pop_under5_2023, 
                                        est_pop5andunder_2023)) %>% 
  rename(zip = GEOID) %>% 
  select(zip, est_pop5andunder_2023)

census %>% filter(pop_under5_2023>est_pop5andunder_2023)

lead_df_with_census <- lead_df_long %>%
  left_join(census, by = 'zip')

lead_df_with_census <- lead_df_with_census %>%
  mutate(pct_elevated_bll = round(total/est_pop5andunder_2023*100,1))

data_for_vis <- lead_df_with_census %>% 
  filter(year == 2021)%>% 
  select(zip, pct_elevated_bll, total) 
  

write_csv(lead_df_long, "data/lead_data_by_zip_2000_2021.csv")
write_csv(data_for_vis, "data/data_for_vis.csv")


#visualize 

  
lead_df_long %>% 
  filter(total_allyears >100) %>% 
  arrange(total_allyears) %>% 
  filter(zip != "21202") %>% 
  ggplot(aes(x = year, y = total)) + 
  geom_line()+
  facet_wrap(~zip)
dev.off()

