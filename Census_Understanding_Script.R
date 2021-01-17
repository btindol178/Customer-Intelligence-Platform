##########################################################################################################################################################################################################
# LOAD REQURED PACKAGES WORKSPACE AND SET WD
rm(list=ls())
setwd("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard")

##########################################################################################################################################################################################################
##########################################################################################################################################################################################################
##########################################################################################################################################################################################################
install.packages("pacman")
pacman::p_load(dplyr, data.table, lubridate,zoo,riem,tidyr,countrycode,RCurl,stringr,tidyverse,shiny,miniUI,taskscheduleR,gtrendsR,tidycensus,sf,leaflet,mapview,viridis,tidyquant,tigris,tmap,sf,maps,tidycensus,xts,ggsflabel,scales,tmaptools,purrr,plotly) 
x <- c("lubridate","data.table","zoo","riem","dplyr","tidyr","countrycode","RCurl","stringr","tidyverse","shiny","miniUI","taskscheduleR","gtrendsR","tidycensus","sf","leaflet","mapview","viridis","tidyquant","tigris","tmap","sf","maps","tidycensus","xts","ggsflabel","scales","tmaptools","purrr","plotly")
lapply(x, require, character.only = TRUE)
require(lubridate);require(data.table);require(zoo);require(riem);require(dplyr);require(tidyr);require(countrycode);require(RCurl);require(stringr);require(tidyverse);require(shiny);require(miniUI);require(taskscheduleR);require(gtrendsR);require(tidycensus);require(sf);require(leaflet);require(mapview);require(viridis);require(tidyquant);require(tigris);require(tmap);require(sf);require(maps);require(xts);require(scales);require(ggsflabel);require(tmaptools);require(purrr);require(plotly)


# Get state information to loop over
stateid <- read.csv("statesid.csv");stateid <- stateid$state_id

# census key code
census_api_key("e3a3dbad3edfa4d96cb59f65931694b311565c63",install = TRUE,overwrite = TRUE)

# Census variable list
all_vars_acs5 <- load_variables(year = 2018, dataset = "acs5") # read all the variable list
all_vars_acs5


options(tigris_use_cache = TRUE)


#Enter the variables and geographies below
census_title <- c("Median Household Income by County:\n
Coefficient of Variation")
census_var <- c("B19013_001E")
census_geog <- c("county")
census_state <- c("or")
acs_data <- get_acs(geography = census_geog, variables =
                      census_var, state = census_state, output = "wide")

#Make more readable column names
acs_data <- acs_data %>%
  rename(MHI_est = B19013_001E ,
         MHI_moe = B19013_001M)
#Calculate the SE, CV for future reference
acs_data <- acs_data %>%
  mutate(se = MHI_moe/1.645,
         cv = (se/MHI_est)*100)

#Plot Percentages with Derived MOE
acs_plot <- acs_data %>%
  ggplot(aes(x = MHI_est,
             y = reorder(NAME, MHI_est))) +
  geom_point(color = "black", size = 2) +
  geom_errorbarh(aes(xmin = MHI_est - MHI_moe,
                     xmax = MHI_est + MHI_moe )) +
  labs(title = paste(census_title),
       subtitle =
         paste0("Oregon 2011-2015 American Community Survey"),
       x = "Median Household Income") +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal() + theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
plot(acs_plot)


mhi_tables <- c("B19013_001")
#download tracts and county, get the tracts for PDX
#Metro counties and counties for the state
mhi_tract <- get_acs(geography = "tract",
                     variables = mhi_tables,
                     state = "OR",
                     geometry = TRUE)

mhi_tract <- mhi_tract %>%
  mutate(CountyFIPS = str_sub(GEOID, 1, 5))
metro_counties <- c("41051", "41005", "41009", "41067", "41071") # Get county code from mhi_tract by splitting GEOID
metro_tract <- mhi_tract %>%
  filter(CountyFIPS %in% metro_counties)
or_mhi_county <- get_acs(geography ="county", variables = mhi_tables, state = "OR", geometry = TRUE)

p1 <- ggplot() +
  geom_sf(data = metro_tract, aes(fill = estimate)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()
p1

# Plot by county
mhi_tract <- get_acs(geography = "county",
                     variables = mhi_tables,
                     state = "OR",
                     geometry = TRUE)


# pLOTTING OUT THE COUNTY INFORMATION
p2 <- ggplot() +
  geom_sf(data = mhi_tract, aes(fill = estimate)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()
p2


# PLot out tract for whole state
mhi_tract <- get_acs(geography = "tract",
                     variables = mhi_tables,
                     state = "OR",
                     geometry = TRUE)


# pLOTTING OUT THE COUNTY INFORMATION
p3 <- ggplot() +
  geom_sf(data = mhi_tract, aes(fill = estimate)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()
p3


# PLot out tract for whole state
mhi_tract <- get_acs(geography = "state",
                     variables = mhi_tables,
                     state = "OR",
                     geometry = TRUE)


# pLOTTING OUT THE COUNTY INFORMATION
p4 <- ggplot() +
  geom_sf(data = mhi_tract, aes(fill = estimate)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis(labels = scales::dollar,
                     name = "MHI Estimate") +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Median Household Income for PDX Metro\n at the census tract level",
       subtitle = "An R 'sf' Example") + theme_minimal()
p4


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################

#name the tables we need
vul_vars <- c("B17001_001", "B17001_002", "B02001_001",
              "B02001_002", "B01001_003", "B01001_020",
              "B01001_021", "B01001_022", "B01001_023",
              "B01001_024", "B01001_025","B01001_027",
              "B01001_044", "B01001_045", "B01001_046",
              "B01001_047", "B01001_048", "B01001_049")

#grab the data for Oregon
vul_acs <-  get_acs(geography = "tract", variables = vul_vars,
          state = "OR", output = "wide")
vul_acs <- vul_acs %>%
  mutate(CountyFIPS = str_sub(GEOID, 1, 5))


vul2 <- vul_acs %>%
  mutate(PovShare = B17001_002E/B17001_001E,
         NonWhite = (B02001_001E - B02001_002E)/B02001_001E,
         Under5 = (B01001_003E + B01001_027E)/B02001_001E,
         Older64Male = B01001_020E + B01001_021E +
           B01001_022E +B01001_023E + B01001_024E +
           B01001_025E,
         Older64Female = B01001_044E +
           B01001_045E + B01001_046E + B01001_047E +
           B01001_048E + B01001_049E,
         Older64 =
           (Older64Male + Older64Female)/B02001_001E) %>%
  select(NAME, GEOID, CountyFIPS, PovShare,
         NonWhite, Under5, Older64)

vul2 <- vul2 %>%
  mutate(
    z_Pov = (PovShare - mean(PovShare, na.rm = TRUE))/
      sd(PovShare, na.rm = TRUE),
    z_NonWhite = (NonWhite - mean(NonWhite, na.rm = TRUE))/
      sd(NonWhite, na.rm = TRUE),
    z_Under5 = (Under5 - mean(Under5, na.rm = TRUE))/
      sd(Under5, na.rm = TRUE),
    z_Older64 = (Older64 - mean(Older64, na.rm = TRUE))/
      sd(Older64, na.rm = TRUE))
vul2 <- vul2 %>%
  mutate(VulIndex = (z_Pov + z_NonWhite + z_Under5 +
                       z_Older64)/4) %>%
  select(GEOID, CountyFIPS, z_Pov, z_NonWhite, z_Under5,
         z_Older64, VulIndex)


metro_counties <- c("41051", "41005", "41009",
                    "41067", "41071")
vul2 <- vul2 %>%
  filter(CountyFIPS %in% metro_counties)
or_tracts <- tracts(state = "OR")
metro_vul <- inner_join(vul2, or_tracts,
                        by = c("GEOID" = "GEOID")) %>%
  select(1:7, geometry) %>% st_as_sf()


# pLOTTING OUT THE COUNTY INFORMATION
p5 <- ggplot() +
  geom_sf(data = metro_vul, aes(fill = VulIndex)) +
  coord_sf(datum = NA) +
  theme(plot.title = element_text(size = 16,
                                  face = "bold", margin = margin(b=10))) +
  theme(plot.subtitle = element_text(size = 14,
                                     margin = margin(b = -20))) +
  theme(plot.caption = element_text(size = 9,
                                    margin = margin(t = -15), hjust = 0)) +
  scale_fill_viridis() +
  labs(caption = "Source: US Census Bureau ACS (2011-2015)",
       title = "Social Vunerability Index",
       subtitle = "An R 'sf' Example") + theme_minimal()
p5




################################################################################################################################
################################################################################################################################
###############################################################################################################################
# PLOTTING...
michigan_pop <- get_acs(geography = "county", 
                     variables = "B01003_001", 
                     state = "MI",
                     geometry = TRUE) 

michigan_pop

# PLOT OUT CENSUS INFORMATION
mapviewOptions(legend.pos = "bottomright")
mapviewOptions(leafletWidth = 800)
mapviewOptions()
mapviewOptions(default = TRUE)
mapview(michigan_pop, zcol = "estimate", native.crs = TRUE, crs = 5070)

# Plotting another way
mapviewOptions(legend.pos = "bottomright")
mapview(michigan_pop,zcol = "estimate") # plot estimate columns

################################################################################################################################
################################################################################################################################
###############################################################################################################################

# Getting more data and understanding options
# top 10 populations in alphabethical order for2017
top10 <- get_acs(geography = "state",
        variables = "B03002_012",
        year = 2017,
        survey = "acs1",
        summary_var = "B03002_001") %>% 
  head(10)
top10


# Get largest counties with hispanic populations in 2017
tophispanic <- get_acs(geography = "county",
        variables = "B03002_012",
        year = 2017,
        survey = "acs1",
        summary_var = "B03002_001") %>%
  top_n(10,wt = estimate)

tophispanic


# Median house hold income for asian, white and hispanic
medhh <- get_acs(geography = "county",
                 variables = c("B19013D_001","B19013H_001","B19013I_001"),
                 year = 2017,
                 survey = "acs1",summary_var = "B19013_001") %>% 
  top_n(n = 10,wt= summary_est)
medhh

# Median house hold income in LA county white, asain, the county itself, hispanic, black
medhhLA<- get_acs(geography = "county",
        state="CA",
        county = "037",
        variables =c("B19013_001",
                     "B19013B_001",
                     "B19013D_001",
                     "B19013H_001",
                     "B19013I_001"),
        year = 2017,
        survey = "acs1")
medhhLA


# Calculate proportions with summary_est
groupstopull <- c("B03002_003","B03002_004",
                  "B03002_006","B03002_012")
congressionaldistrict <- get_acs(geography = "congressional district",
        variables = groupstopull,
        year = 2017,
        survey = "acs1",
        summary_var = "B03002_001")

congressionaldistrict
############################################################################
# Get_estimates to pull data over time
timezz <-get_estimates(geography = "combined statistical area",
              product = "population",
              year = 2018,
              time_series = T)

timezz

# Get_estimates to pull data NOT OVER TIME ( TOP ONE ABOVE THIS IS ADDED SLOWLY)
timey <-get_estimates(geography = "combined statistical area",
                       product = "population",
                       year = 2018)

timey

##################################################################################
##################################################################################
##################################################################################
# Mapping
ca_tract_geo <- get_acs(geography = "tract",
                        state="CA",
                        variables = "B19013_001",
                        geometry = T)
head(ca_tract_geo)

ca_tract_geo %>% 
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "plasma",
                       labels=scales::dollar) +
  theme_minimal()+
  labs(title = "Tract-Level HH Median Income",
       fill = "HH Median Income")



####################################################
# zoom in on riverside county only
ca_tract_geo %>% 
  filter(str_detect(NAME,"Riverside County")) %>% 
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "plasma",
                       labels=scales::dollar) +
  theme_minimal()+
  labs(title = "Tract-Level HH Median Income",
       fill = "HH Median Income")

##################################################################################
##################################################################################
##################################################################################
# Getting data for multiple years easily
years <- lst(2012, 2017) 

# which counties?
my_counties <- c(
  "Alameda",
  "Contra Costa",
  "Marin",
  "Napa",
  "San Francisco",
  "San Mateo",
  "Santa Clara",
  "Solano",
  "Sonoma"
)

# which census variables?
my_vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001"
)

# loop over list of years and get 1 year acs estimates
bay_area_multi_year <- map_dfr(
  years,
  ~ get_acs(
    geography = "county",
    variables = my_vars,
    state = "CA",
    county = my_counties,
    year = .x,
    survey = "acs1",
    geometry = FALSE
  ),
  .id = "year"  # when combining results, add id var (name of list item)
) %>%
  select(-moe) %>%  # shhhh
  arrange(variable, NAME) %>% 
  print()
bay_area_multi_year

# compare years, I'll reshape the data and then calculate absolute change and percent change from 2012 to 2017. I'll also adjust the 2012 median income estimate for inflation.

# reshape and calculate percent change in income
# CALCULATE THE PERCENT CHANGE
bay_area_12_17 <- bay_area_multi_year %>% 
  spread(year, estimate, sep = "_") %>% 
  mutate(
    year_2012 = if_else(
      variable == "median_income",
      round(year_2012 * 1.068449, 0),  # multiply 2012 by cpi inflation factor
      year_2012
    ),
    change = year_2017 - year_2012,
    pct_change = change / year_2012 * 100
  )
bay_area_12_17

# which counties had the largest percent increase in median income?
bay_area_12_17 %>% 
  filter(variable == "median_income") %>% 
  arrange(desc(pct_change))

###########################################################################################################
###########################################################################################################
###########################################################################################################
#PLOT ON INTERACTIVE OPENSTREET LEAFLET MAP...

my_states <- c("NH", "ME", "VT")

my_vars <- c(
  total_pop = "B01003_001",
  median_income = "B19013_001"
)

multi_state_tract <- get_acs(
  geography = "tract",
  variables = my_vars,
  state = my_states,
  year = 2016,
  survey = "acs5",
  geometry = FALSE
) %>% 
  print()

# get county subdivision formichigan
single_state_subdiv <- get_acs(
  geography = "county subdivision",
  variables = my_vars,
  state = "MI",
  year = 2016,
  survey = "acs5",
  geometry = FALSE
) %>% 
  print()

#But, if you try to get county subdivision data for more than one state in a single get_acs() call, you get an error.
get_acs(
  geography = "county subdivision",
  variables = my_vars,
  state = my_states,
  year = 2016,
  survey = "acs5",
  geometry = FALSE
)

# Get multi state subdivision
multi_state_subdiv <- map_dfr(
  my_states,
  ~ get_acs(
    geography = "county subdivision",
    variables = my_vars,
    state = .,
    year = 2016,
    survey = "acs5",
    geometry = FALSE
  )
) %>% 
  print()

# get fips codes
fips_codes <- tidycensus::fips_codes

#We can filter the fips_codes data frame to include only counties in our selected states, 
#and then use map2_dfr() to loop over each of the state/county FIPS code combinations in this table. This will make a separate call to get_acs() and the Census API for each county in our three states.

my_counties <- fips_codes %>%
  filter(state %in% my_states)

multi_state_bg <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "block group",
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2016,
    survey = "acs5",
    geometry = FALSE
  )
) %>% 
  print()

#Lastly, one of the great features of tidycensus is that it links with the tigris package to get simple feature geometries along with Census estimates. This makes it quite easy to make a choropleth map with Census data. So we'll repeat our previous call and specify geometry = TRUE in get_acs().

#But to do this for multiple states, we have to modify our previous approach because it is currently not possible to use bind_rows() with sf objects (map2_dfr() uses bind_rows() to combine the results of the loop). One way around this is to use map2() and then combine the list of data frames with the rbind.sf method.
multi_state_bg_geo_list <- map2(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "block group",
    variables = my_vars,
    state = .x,
    county = .y,
    year = 2016,
    survey = "acs5",
    geometry = TRUE,
    output = "wide"  # get data in wide format for easier mapping
  )
)

#get population and median income 
multi_state_bg_geo <- reduce(multi_state_bg_geo_list, rbind) %>% 
  print()

# define a little helper function to format dollars for map
make_dollar <- function(x, digits = 0) {
  paste0("$", formatC(x, digits = digits, format = "f", big.mark = ","))
}

tmap_mode("view")
tm_shape(multi_state_bg_geo) +
  tm_fill(
    col = "median_incomeE",
    palette = "Greens",
    style = "jenks",
    contrast = c(0.3, 1),
    title = "Median HH Income",
    textNA = "Not Available",
    id = "NAME",
    popup.vars = c(
      "Median HH Income" = "median_incomeE",
      "Total Population" = "total_popE"
    ),
    popup.format = list(
      median_incomeE = list(fun = make_dollar),
      total_popE = list(format = "f", digits = 0)
    ),
    legend.format = list(fun = make_dollar)
  ) +
  tm_borders(col = "darkgray") +
  tm_view(
    alpha = 0.85,
    basemaps = "Stamen.TonerLite",
    view.legend.position = c("right", "bottom")
  )

###########################################################################################################
###########################################################################################################
###########################################################################################################
# Creating beautiful demographic maps in R with the tidycensus and tmap packages

dat12 <- get_acs("county", table = "B27001", year = 2012, 
                output = "tidy", state = NULL, geometry = FALSE) %>%
  rename(`2012` = estimate) %>%
  select(-NAME, -moe) 

dat16 <- get_acs("county", table = "B27001", year = 2016, 
                 output = "tidy", state = NULL, geometry = TRUE, shift_geo = TRUE) %>%
  rename(`2016` = estimate) %>%
  select(-moe)
#####################################################
# SHAPE FILE FOR COUNTY AND STATE DATA
# County data
data("county_laea", package = "tidycensus")
counties <- county_laea
# State data
 data("state_laea", package = "tidycensus")
states <- state_laea

# MERGING THE 2 YEARS FOR THE SAMEVARIABLE
dat <- left_join(dat16, dat12, by = c("GEOID", "variable"))
st_geometry(dat) <- NULL # This drops the geometry and leaves a table

head(dat)

######################################################
# We use the dplyr function case_when() instead of a series of if/then statements. Essentially this function allows us to say "if the variable value is B27001_009, B27001_012, B27001_037 or B27001_040 then assign the value of "pop1834" and so on.

dat <- mutate(dat,
              cat = case_when(
                variable %in% paste0("B27001_0",
                                     c("09","12","37","40")) ~ "pop1834",
                variable %in% paste0("B27001_0",
                                     c("11","14","39","42")) ~ "pop1834ni")) %>%
  filter(!is.na(cat))


# In order to calculate our estimates we'll stack the data, separated by year, using tidyr::gather then group based on GEOID, NAME, year and our new cat field so that we have counts for total population and population without insurance for each county for each year. Then we use spread() to push the total population and population without insurance into their own variables. The final table should include a county-year summation for variables pop1834 and pop1834ni.
# Create long version
dat <- tidyr::gather(dat, year, estimate, c(`2012`, `2016`))

# Group the data by our new categories and sum
dat <- group_by(dat, GEOID, NAME, year, cat) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  tidyr::spread(cat, estimate) 



#Calculate the final estimates
# For our final numbers we want % of uninsured population ages 18-34 for both 2012 and 2016. 
#In addition we'll calculate the difference in % uninsured between years (i.e. 2016 - 2012).

dat <- mutate(dat, est = (pop1834ni/pop1834) * 100) %>%
  select(-c(pop1834, pop1834ni)) %>%
  tidyr::spread(year, est) %>%
  mutate(diff = `2016`-`2012`)


# VISUALIZATION
#Take a quick look at the data using ggplot.
# Compare the county distributions as well as the median % uninsured by year using facet_wrap.
datlong <- select(dat, -diff) %>%
  tidyr::gather(year, estimate, c(`2012`, `2016`)) %>%
  group_by(year) %>%
  mutate(med = round(median(estimate, na.rm = TRUE), 1))

# Plot histogram
ggplot(datlong, aes(estimate)) +
  geom_histogram(fill = "firebrick2", 
                 color = "white", bins = 60) +
  xlab("Uninsured adults ages 18-34 by county (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~year, ncol = 1) +
  geom_vline(aes(xintercept = med,
                 group = year), lty = "dashed") +
  geom_text(aes(label = paste("Median = ", med), x = med, y = 55))

######################################################################################
#Counties with greatest change (+/-) in % uninsured
#We're curious to know which counties experienced the largest increase or decrease in the % of uninsured. Use the function dplyr::top_n to get the first and last 10 records from the diff field.

d10 <- top_n(dat, 10, diff) %>%
  mutate(type = "Insured population decreased",
         difftemp = diff)

i10 <- top_n(dat, -10, diff) %>%
  mutate(type = "Insured population increased",
         difftemp = abs(diff))

id10 <- bind_rows(list(i10, d10)) %>%
  arrange(desc(difftemp))
ggplot(id10) + 
  geom_col(aes(x = forcats::fct_reorder(NAME, difftemp), 
               y = difftemp, fill = type)) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick2", "cyan4")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Counties with the greatest change (+/-) in
    insured population, ages 18-34, 2012-2016") +
  ylab("Difference in % insured (2016 - 2012)") +
  xlab("")

#####################################################################################
 #Create a final geographic file for use with tmap
#You may remember that our initial file, dat16, included the actual geography while the file we just created is tabular-only. In order to map the variables we just created we can join dat with the spatial object dat16. Because of the way tidycensus formats the data (stacked) we'll remove all variables except for one and join the objects by GEOID and NAME.
# dat16 is our original geographic object and dat is the tabular

# dat16 is our original geographic object and dat is the tabular data
shp <- dat16 %>%
  filter(variable == "B27001_001") # much faster than using distinct()
select(GEOID, NAME) %>%
  left_join(dat, by = c("GEOID", "NAME")) %>%
  arrange(GEOID) %>%
  rename(uninsured_2012 = `2012`,
         uninsured_2016 = `2016`,
         uninsured_diff = diff)

# Remove the Aleutians West from shp for display purposes.
# NOTE: this isn't necessary since I'm using the shift_geo
# argument in the get_acs function. However if you're not
# using shift_geo or joining to a different spatial layer
# for the full US you may want to consider removing this 
# record for display purposes.
shp <- filter(shp, GEOID != "02016")


#Part 2: Creating beautiful maps with tmap
#One of the best features of tmap is how easy it is to create maps. The tmap syntax is modeled after ggplot2 whereby the initial command specifies the shape object and data input (tm_shape()) and is followed by the map layer (e.g., tm_polygons() or tm_lines()). Layers can be stacked similar to ggplot2 using the + symbol. In addition, attribute elements can be added to the map and maps can be faceted similar to using facets in ggplot2.
#Moving forward we encourage you to explore the tmap documentation as this package is extremely function-rich and we are only skimming the surface of what's available in this powerful package.
#If you skipped Part 1 (otherwise you can skip this step)

shp <- st_read("C:/Users/blake/OneDrive/Stryker Project/Country Leaflet Information Dashboard/acs_2012_2016_county_us_B27001.shp",
               stringsAsFactors = FALSE) %>%
  rename(uninsured_2012 = un_2012,
         uninsured_2016 = un_2016,
         uninsured_diff = unnsrd_) %>%
  mutate(STFIPS = stringr::str_sub(GEOID, 1, 2))


#Super easy mapping
#With tmap there is not a lot of data preparation that needs to happen before mapping. With very little code you can create a simple map.\
#A) The easiest possible map, just the geography
#The "shifted" geography we downloaded from tidycensus is a huge help. The shift_geo argument creates subsets of the continental US, Alaska and Hawaii. The author of tmap has a helpful write-up here with guidance for displaying subsets.
# Define the shape and the layer elements

#Define the shape and the layer elements
tm_shape(shp) +
  tm_polygons()


#B) Add a variable to your map
#Below is a map of the 2012 data using all of the tmap defaults - easy but not all that pretty! The default classification does not work well with this data and in some places it's difficult to see the counties due to the heavy gray outlines.

tm_shape(shp) +
  tm_polygons("uninsured_2012")

#C) Change the shapes
#The package provides a lot of flexibility in terms of different approaches to mapping your data. Here we use bubbles in place of polygons and note that no additional data processing is required.

tm_shape(shp) +
  tm_bubbles("uninsured_2012")


# Plot empire state building
# Create a simple geographic object with one point
dat <- data.frame(c("Empire State Building"), 
                  lat = c(40.748595), 
                  long = c(-73.985718))
sites <- sf::st_as_sf(dat, coords = c("long", "lat"), 
                      crs = 4326, 
                      agr = "identity")

tm_shape(shp) +
  tm_polygons() +
  tm_shape(sites) +
  tm_dots(size = 2)

# Plot blue color scheme
var <- "uninsured_2012"

tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              style = "quantile",
              palette = "BuPu") +
  tm_legend(legend.position = c("left", "bottom"))


#User-defined classification
#For controlling the cut points in our data we'll drop the style argument and use breaks. Notice too in this example that we changed the color of the county outlines and added a little transparency so that they're not as overwhelming.

cuts <- c(0, 10, 20, 30, 40, 100)

tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))


#Additional color options
#Example 1: Apply type of palette instead of palette scheme
#If you don't know exactly which color scheme to use but you'd like to apply a sequential palette you can use palette = "seq". This will apply colors from the first sequential set of colors in the RColorBrewer color schemes.

tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "seq", 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))


#Example 2: Reverse the color scheme
#We can also reverse the color schemes with a simple -. Here we reverse the BuPu color scheme - which doesn't make any sense in this context!
  
  tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "-BuPu", 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))
  
  
 # Example 3: Choose custom colors
 # If you want to assign colors outside of RColorBrewer simply create a vector of HEX codes and apply them to the palette argument.
  
  mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")
  
  tm_shape(shp, projection = 2163) +
    tm_polygons(var,
                breaks = cuts,
                palette = mycols, 
                border.col = "white", 
                border.alpha = 0.5) +
    tm_legend(legend.position = c("left", "bottom"))
  
  
#Add titles to the map
 # Add a main map title and a legend title.
  mymap <- tm_shape(shp, projection = 2163) +
    tm_polygons(var,
                breaks = cuts,
                palette = "BuPu", 
                border.col = "white", 
                border.alpha = 0.5,
                title = "Uninsured (%)") +
    tm_legend(legend.position = c("left", "bottom")) +
    tm_layout(title = "Uninsured adults ages 18-34 by county, 2012",
              title.size = 1.1,
              title.position = c("center", "top"))
  
  mymap
  
  
  #Increase the map margins
 # The default value for the inner margins (margins inside the frame) is a little small (0.02). Make room for adding other elements to the map. The order of inner.margins inputs is bottom, left, top and right. Values can be between 0 and 1.
  
  mymap + tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08))
  
  
  # COMPASS
  mymap + 
    tm_scale_bar() + 
    tm_compass()
  
  
  # ADD A SCALE BAR AND NORTH ARROW
  # Add unit argument to tm_shape
  tm_shape(shp, projection = 2163, unit = "mi")
  
  # Customize scale bar and north arrow
  mymap + tm_scale_bar(color.dark = "gray60",
                       position = c("right", "bottom")) + 
    tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
               color.dark = "gray60", text.color = "gray60",
               position = c("left", "top"))
  
  
 
###########################################################################
  # PLOT 2 MAPS ABOVE EACH OTHER
  # ADD MULTIPLE MAPS TO A PAGE
  createMap <- function(.data, varname, statecol, maptitle){
    
    tm_shape(.data, projection = 2163, unit = "mi") +
      tm_polygons(varname,
                  breaks = cuts,
                  palette = "BuPu", 
                  border.col = "white", 
                  border.alpha = 0.5,
                  title = "Uninsured (%)") +
      tm_legend(legend.position = c("left", "bottom")) +
      tm_layout(title = maptitle,
                title.size = 1.1,
                title.position = c("center", "top"),
                inner.margins = c(0.06, 0.10, 0.10, 0.08),
                frame = FALSE) +
      tm_scale_bar(color.dark = "gray60",
                   position = c("right", "bottom")) + 
      tm_shape(states) +
      tm_borders(col = statecol)
    
  }
  
  m1 <- createMap(shp, 
                  varname = "uninsured_2012", 
                  statecol = "green", 
                  maptitle = "Here is title 1")
  
  m2 <- createMap(shp, 
                  varname = "uninsured_2016", 
                  statecol = "yellow",
                  maptitle = "Here is title 2")
  
  tmap_arrange(m1, m2, ncol = 1)
  
 #################################################################
  # Plot 2 maps at once again next to eachother
  var2 <- c("uninsured_2012", "uninsured_2016")
  title2 <- c("Uninsured adults ages 18-34 by county, 2012", 
              "Uninsured adults ages 18-34 by county, 2016")
  
  createMap(shp, 
            varname = var2, 
            statecol = "black", 
            maptitle = title2)
  
  ###############################################################
#  Using facets with "long" data
 # Faceting in tmap is very similar to faceting in ggplot, that is, small multiples will be created based on a given variable.
   # Below we use the tm_facet function and the by argument. In order for this to work with our data we'll need to create a "long" version. Use the gather function from tidyr. As you can see below we now have 2 records for each county, the 2012 and 2016 estimates.
  
  shplong <- select(shp, GEOID, NAME, uninsured_2012, uninsured_2016) %>%
    tidyr::gather(year, estimate, c(uninsured_2012, uninsured_2016)) %>%
    arrange(GEOID)
  
  glimpse(shplong)
  
  mymap <- tm_shape(shplong, projection = 2163) +
    tm_polygons("estimate",
                breaks = cuts,
                palette = "BuPu", border.col = "white", 
                border.alpha = 0.3,
                title = "Uninsured (%)") +
    tm_facets(by = "year", free.coords = TRUE, ncol = 1) +
    tm_shape(states) +
    tm_borders(col = "black")
  mymap
  
#############################################################################################  
  # Getting data for every Census tract in the US with purrr and tidycensus
  
us <- unique(fips_codes$state)[1:51]
  
totalpop <- map_df(us, function(x) {
    get_acs(geography = "tract", variables = "B01003_001", 
            state = x) # add years here
  })
  
  str(totalpop)

###############################################
  # get spatial object from it
options(tigris_use_cache = TRUE)
  
  totalpop_sf <- reduce(
    map(us, function(x) {
      get_acs(geography = "county", variables = "B01003_001", 
              state = x, geometry = TRUE)
    }), 
    rbind
  )
  
  str(totalpop_sf)
  
# TRYING TO GET  ALL OF THE TRACTS FOR EACH STATE IN R IF THIS DOESNT WORK THEN USE THE METHOD I KNOW
  library(sf)
  options(tigris_use_cache = TRUE)
  
  totalpop_sf <- reduce(
    map(us, function(x) {
      get_acs(geography = "county", variables = "B01003_001", 
              state = x, geometry = TRUE)
    }), 
    rbind
  )
  
  str(totalpop_sf)
  ##################################################################################
  ########################################################################################################
  ########################################################################################################
 # NET MIGRATION PER 1000 RESIDENTS BY COUNTY
 options(tigris_use_cache = TRUE)
  
 us_components <- get_estimates(geography = "state", product = "components")
  
 us_components
 
#uNIQUE US_COMPONENTS
unique(us_components$variable)
  
# NET MIGRATION
net_migration <- get_estimates(geography = "county",
                               variables = "RNETMIG",
                               geometry = TRUE,
                               shift_geo = TRUE)

net_migration

# MAKING A PLOT FOR NET MIGRATION 
order = c("-15 and below", "-15 to -5", "-5 to +5", "+5 to +15", "+15 and up")

net_migration <- net_migration %>%
  mutate(groups = case_when(
    value > 15 ~ "+15 and up",
    value > 5 ~ "+5 to +15",
    value > -5 ~ "-5 to +5",
    value > -15 ~ "-15 to -5",
    TRUE ~ "-15 and below"
  )) %>%
  mutate(groups = factor(groups, levels = order))

ggplot() +
  geom_sf(data = net_migration, aes(fill = groups, color = groups), lwd = 0.1) +
  geom_sf(data = tidycensus::state_laea, fill = NA, color = "black", lwd = 0.1) +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_color_brewer(palette = "PuOr", direction = -1, guide = FALSE) +
  coord_sf(datum = NA) +
  theme_minimal(base_family = "Roboto") +
  labs(title = "Net migration per 1000 residents by county",
       subtitle = "US Census Bureau 2017 Population Estimates",
       fill = "Rate",
       caption = "Data acquired with the R tidycensus package | @kyle_e_walker")




# UNIQUE MAP!!!!!!!!!!!!!!!!!
la_age_hisp <- get_estimates(geography = "county",
                               product = "characteristics",
                               breakdown = c("SEX", "AGEGROUP", "HISP"),
                               breakdown_labels = TRUE,
                               state = "CA",
                               county = "Los Angeles")
  
  la_age_hisp
  
# TO GET ESTIMATES OF POPULATION CHARACTERISTICS
  compare <- filter(la_age_hisp, str_detect(AGEGROUP, "^Age"),
                    HISP != "Both Hispanic Origins",
                    SEX != "Both sexes") %>%
    mutate(value = ifelse(SEX == "Male", -value, value))
  
  ggplot(compare, aes(x = AGEGROUP, y = value, fill = SEX)) +
    geom_bar(stat = "identity", width = 1) +
    theme_minimal(base_family = "Roboto") +
    scale_y_continuous(labels = function(y) paste0(abs(y / 1000), "k")) +
    scale_x_discrete(labels = function(x) gsub("Age | years", "", x)) +
    scale_fill_manual(values = c("darkred", "navy")) +
    coord_flip() +
    facet_wrap(~HISP) +
    labs(x = "",
         y = "2017 Census Bureau population estimate",
         title = "Population structure by Hispanic origin",
         subtitle = "Los Angeles County, California",
         fill = "",
         caption = "Data source: US Census Bureau population estimates & tidycensus R package")
  
  
#########################3
# PYRIMID PLOT WITH 2 FACTORS USING PLOTLY
  SEX <- compare$SEX
  AGE <- compare$AGEGROUP
  VALUE <- compare$value
  plot_ly(compare,x = VALUE, y = AGE, color  = SEX, type = 'bar', orientation = 'h',
          hoverinfo = 'y+text+name', text = VALUE) %>%
    layout(bargap = 0.1, barmode = 'overlay',
           xaxis = list(tickmode = 'array', tickvals = c(-200000, -100000, 0, 100000, 200000),
                        ticktext = c(-200000, -100000, 0, 100000, 200000)))
  
 