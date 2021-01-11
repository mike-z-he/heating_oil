#### Figures for Manuscript
#### December 15, 2020
#### Mike He

## Required packages
library(tidyverse)
library(sf)
# lizzy: added install.packages("spdep")
library(spdep)
# lizzy: added install.packages("spatialreg")
library(spatialreg)
library(janitor)
# lizzy: added install.packages("patchwork")
library(patchwork)
library(maptools)
# lizzy: added install.packages("rgeos")
library(rgeos)
library(rgdal)
library(viridis)
# lizzy: added install.packages("ggsn")
library(ggsn)
library(modelr)

# lizzy: ignore setwd, use relative paths
# setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/heating_oil/lyuou")
options(mc.cores=parallel::detectCores())

### Reading in data
# lizzy: changed to relative path
# lizzy: put 'heating_oil' repo inside 'data' folder you emailed me
# lizzy: all extra files you sent me are one folder above the working directory
dta <- st_read('../model_data_export.shp')
head(dta)

### Remove outliers ###
# lizzy: why these cutoffs?

# mike: I did this with Marianthi in her office back when we were inspecting potential 
# outliers. These were either completely unreasonable numbers or numbers that were in severe disagreement with each other.
# For example: delta_6 and d_ro6 refers to two different data sources that document changes away from heating oil #6. 
# When these numbers differed drastically (e.g. 300 vs 3), we chose to remove these. Similar, we removed a handful of 
# numbers that were very large or negative that seemed unreasonable (i.e. 250 conversions in a very small census tract; 
# "negative" conversions which means that there were actually heating oil #6 boilers added in a tract, which is unreasonable 
# since they were being banned, etc.)

# removed 15 census tracks
dta <- dta[-which(dta$d_ro6>200 | dta$d_ro6< -20),]

dta <- dta[-which((dta$delta_6< 5 & dta$d_ro6 >25)|dta$d_ro6 > 45),]

dta <- dta[-which(dta$d_ro4 >25), ]


#### Figure 1 (Map) ####

viz <- st_read('../NYC tract.shp') %>% 
  rename(
    no2_16 = no2_16mean,
    so2_12 = so2_12_mea,
    so2_16 = so2_16_mea,
    pm_12 = pm_12_mean,
    pm_16 = pm_16_mean
  ) %>% 
  mutate(
    no2_diff = no2_12 - no2_16,
    so2_diff = so2_12 - so2_16,
    pm_diff = pm_12 - pm_16
  ) %>% 
  st_transform(., 4269) %>% # lizzy: this changes coordinate reference system
  as_Spatial()
  
viz@data$id <- rownames(viz@data)
gpclibPermit() # lizzy: what does this do?

# mike: it has to do with support in the maptools package. 
# Depending on what packages I have active, this line may or may not be needed.

viz.fort <- broom::tidy(viz, region="id") # lizzy: put lat, long, etc. in data frame
viz_plot <- plyr::join(viz.fort, viz@data, by="id")
head(viz_plot)
# lizzy: checking ave diff
#        names(viz_plot)
#        summary(viz_plot$no2_diff)
#        summary(viz_plot$so2_diff)
#        summary(viz_plot$pm_diff)
#        These don't match manuscript, maybe some weighting?

# mike: Okay. I checked on this, and it seems like it is because the shapefile that is used to generate the map is 
# different from the one used for the analysis, and the 15 census tracts removed in the analysis isn't removed in this 
# shapefile. I'll remove them and regenerate the maps, but the averages if you use summary(dta$no2_diff) should provide 
# the correct estimates.

# lizzy: checking ave diff
names(dta)
summary(dta$no2_diff)
mean(dta$no2_diff, na.rm = TRUE)/mean(dta$no2_12, na.rm = TRUE)

summary(dta$so2_diff)
mean(dta$so2_diff, na.rm = TRUE)/mean(dta$so2_12, na.rm = TRUE)

summary(dta$pm_diff)
mean(dta$pm_diff, na.rm = TRUE)/mean(dta$pm_12, na.rm = TRUE)

# lizzy: Yep, that matches!

# lizzy: I can't run this whole files because of overlapping object names
#        no2_plot here is a map and later on it is a point and errorbar plot
#        should have unique names
#        I should be able to source this file all at once
no2_map <- ggplot() + 
  geom_blank(data = viz.fort, aes(long, lat)) + 
  geom_map(data = viz@data, map = viz.fort, aes(fill = no2_diff, map_id = id), size=0.15) +
  scale_fill_viridis(name = expression('Reduction in NO'[2] * ', ppb')) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = c(0.2, 0.75),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size=12)) +
  north(viz_plot, scale = 0.12, symbol = 1, location="topright") + 
  scalebar(data = viz_plot, dist = 10, st.dist = 0.05, st.size = 4, dist_unit = 'km', transform = T, model = 'WGS84')

so2_map <- ggplot() + 
  geom_blank(data = viz.fort, aes(long, lat)) + 
  geom_map(data = viz@data, map = viz.fort, aes(fill = so2_diff, map_id = id), size=0.15) +
  scale_fill_viridis(name = expression('Reduction in SO'[2] * ', ppb')) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = c(0.2, 0.75),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size=12)) +
  north(viz_plot, scale = 0.12, symbol = 1, location="topright") + 
  scalebar(data = viz_plot, dist = 10, st.dist = 0.05, st.size = 4, dist_unit = 'km', transform = T, model = 'WGS84')

pm_map <- ggplot() + 
  geom_blank(data = viz.fort, aes(long, lat)) + 
  geom_map(data = viz@data, map = viz.fort, aes(fill = pm_diff, map_id = id), size=0.15) +
  scale_fill_viridis(name = expression('Reduction in PM'[2.5] * ', ug/m'^3)) +
  coord_equal() +
  theme_bw() +
  theme(legend.position = c(0.2, 0.75),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(color = 'black'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12)) +
  north(viz_plot, scale = 0.12, symbol = 1, location="topright") + 
  scalebar(data = viz_plot, dist = 10, st.dist = 0.05, st.size = 4, dist_unit = 'km', transform = T, model = 'WGS84')

so2_map + pm_map + no2_map
# lizzy: maps match!

# lizzy: where is the code for this sentence in the results
# 'Reductions in NO2 and SO2 were spatially dependent with the highest clusters in Midtown Manhattan and Upper East Side'
# is it the moran's i on OLS residuals?
# Do you do a local test for the clusters you mention in the manuscript or is that more qualitative?

#### Figure 2 (Main results) ####
### SO2
sen_so2 <- dta %>% 
  filter(!is.na(so2_diff)) %>% 
  as_Spatial()

nb_so2 <- poly2nb(sen_so2)  # lizzy: what does this do?
# lizzy: #Creates a queen's neighborhood weight matrix

wgt_so2 <- nb2listw(nb_so2) # lizzy: what does this do?
# lizzy: convert the neighborhood matrix into a list so that the connections between counties can be used in Moran's I test.

# mike: Building neighbors and the construction of spatial weights, which are both required for the spatial analysis. 
# these must be defined prior to running the spatial regression models.

## Spatial Lag Model, using new RO4 and RO6 data (delta_4 and delta_6)
so2_lag <- lagsarlm(so2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                      medtrk + car + avg_year + med_income, data = sen_so2, listw = wgt_so2, 
                    zero.policy = TRUE, tol.solve = 1e-20)
# lizzy: define these variables somewhere

# d_ro2: change in the number of buildings that used heating oil #2 (from Benchmark)
# d_ro4: change in the number of buildings that used heating oil #4 (from Benchmark)
# d_ro6: change in the number of buildings that used heating oil #6 (from Benchmark)
# delta_4: change in the number of buildings that used heating oil #4 (from Spot the Soot)
# delta_6: change in the number of buildings that used heating oil #6 (from Spot the Soot)
# d_ng: change in the number of buildings that used natural gas
# d_d2: change in the number of buildings that used diesel #2
# bus: vehicle miles traveled (VMT) by buses
# hvytrk: miles traveled by heavy-duty trucks
# medtrk: miles traveled by medium-duty trucks
# car: miles traveled by cars
# avg_year: year that the building was built
# med_income: median household income (surrogate for SES per census tract)

# lizzy: what does zero policy do?

# mike: I believe this allows the spatial regression in the scenario that there are regions without neighbors.

# lizzy: what does listw do?

# mike: Spatial weight objects that contains the information about neighbors

summary(so2_lag)$Coef

## Original Model (RO2, RO4, RO6)
# lizzy: what is the difference between this and above?

# mike: So there are are actually two resources that we utilize for the heating oil conversion data: Benchmark (d_ro2, 
# d_ro4, d_ro6) and Spot the Soot (delta_4 and delta_6). Long story short, Benchmark only provides heating oil conversion 
# for buildings greater than 50k sq ft, so it doesn't cover all of the fuel conversions in a particular census tract. Spot 
# the Soot covers the smaller buildings, but data is only available for conversion to/from heating oil #4 and #6, but not 
# #2. Therefore, we ran the models twice to see whether changing the source of the data impacted the effect estimates.

so2_lag_original <- lagsarlm(so2_diff ~ d_ro2 + d_ro4 + d_ro6 + d_ng + d_d2 + bus + hvytrk + 
                               medtrk + car + avg_year + med_income, data = sen_so2, listw = wgt_so2, 
                             tol.solve = 1e-20, zero.policy = TRUE)

summary(so2_lag_original)$Coef

### PM2.5
sen_pm <- dta %>% 
  filter(!is.na(pm_diff)) %>% 
  as_Spatial()

nb_pm <- poly2nb(sen_pm)
wgt_pm <- nb2listw(nb_pm)

## New Model
pm_lag <- lagsarlm(pm_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                     medtrk + car + avg_year + med_income, data = sen_pm, listw = wgt_pm, 
                   tol.solve = 1e-20, zero.policy = TRUE)

summary(pm_lag)$Coef

## Original Model
pm_lag_original <- lagsarlm(pm_diff ~ d_ro2 + d_ro4 + d_ro6 + d_ng + d_d2 + bus + hvytrk + 
                              medtrk + car + avg_year + med_income, data = sen_pm, listw = wgt_pm, 
                            tol.solve = 1e-20, zero.policy = TRUE)

summary(pm_lag_original)$Coef

### NO2
sen_no2 <- dta %>% 
  filter(!is.na(no2_diff)) %>% 
  as_Spatial()

nb_no2 <- poly2nb(sen_no2)
wgt_no2 <- nb2listw(nb_no2)

## New Model
no2_lag <- lagsarlm(no2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                      medtrk + car + avg_year + med_income, data = sen_no2, listw = wgt_no2, 
                    tol.solve = 1e-20, zero.policy = TRUE)

summary(no2_lag)$Coef

## Original Model
no2_lag_original <- lagsarlm(no2_diff ~ d_ro2 + d_ro4 + d_ro6 + d_ng + d_d2 + bus + hvytrk + 
                               medtrk + car + avg_year + med_income, data = sen_no2, listw = wgt_no2, 
                             tol.solve = 1e-20, zero.policy = TRUE)

summary(no2_lag_original)$Coef

# lizzy: I used these model tables to check table S1 coefficients
# you have confidence intervals for oil #6 in the next section
# i don't see CI for oil, 2, 4, diesel, or natural gas, checking them here

summary(no2_lag)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names() %>% 
  mutate(pi = estimate*10,
         lower = (estimate*10) - 1.96 * (std_error*10),
         upper = (estimate*10) + 1.96 * (std_error*10)) %>% 
  select(var_name, pi, lower, upper) %>% 
  slice(2:6)

summary(so2_lag)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names() %>% 
  mutate(pi = estimate*10,
         lower = (estimate*10) - 1.96 * (std_error*10),
         upper = (estimate*10) + 1.96 * (std_error*10)) %>% 
  select(var_name, pi, lower, upper) %>% 
  slice(2:6)
# lizzy: I get (-0.05, 0.12) as the CI for oil #4, manuscript table S1 has upper rounded to 0.11

summary(pm_lag)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names() %>% 
  mutate(pi = estimate*10,
         lower = (estimate*10) - 1.96 * (std_error*10),
         upper = (estimate*10) + 1.96 * (std_error*10)) %>% 
  select(var_name, pi, lower, upper) %>% 
  slice(2:6)

### Plot of all three

## Clean names
no2_coef <- summary(no2_lag)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

so2_coef <- summary(so2_lag)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

pm_coef <- summary(pm_lag)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

so2_coef_original <- summary(so2_lag_original)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

pm_coef_original <- summary(pm_lag_original)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

no2_coef_original <- summary(no2_lag_original)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

## Calculate CIs (by per 10)

# lizzy: row 4 is delta_6
# delta_6: change in the number of buildings that used heating oil #6 (from Spot the Soot)
# d_ro6: change in the number of buildings that used heating oil #6 (from Benchmark)
so2 <- rbind(so2_coef[4, ], so2_coef_original[4, ]) %>% 
  mutate(pi = estimate*10,
         upper = (estimate*10) + 1.96 * (std_error*10),
         lower = (estimate*10) - 1.96 * (std_error*10))

pm <- rbind(pm_coef[4, ], pm_coef_original[4, ]) %>% 
  mutate(pi = estimate*10,
         upper = (estimate*10) + 1.96 * (std_error*10),
         lower = (estimate*10) - 1.96 * (std_error*10))

no2 <- rbind(no2_coef[4, ], no2_coef_original[4, ]) %>% 
  mutate(pi = estimate*10,
         upper = (estimate*10) + 1.96 * (std_error*10),
         lower = (estimate*10) - 1.96 * (std_error*10))

so2
pm
no2

## Actual plots
# lizzy: same name as above, should use new name
so2_plot <- so2 %>% 
  ggplot() + 
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name), size = 2.5) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name), size = 1.2) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('SO'[2])) + 
  geom_hline(yintercept = 0)

pm_plot <- pm %>% 
  ggplot() +  
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name), size = 2.5) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name), size = 1.2) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('PM'[2.5])) + 
  geom_hline(yintercept = 0)

no2_plot <- no2 %>% 
  ggplot() +  
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name), size = 2.5) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name), size = 1.2) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('NO'[2])) + 
  geom_hline(yintercept = 0)

so2_plot + pm_plot + no2_plot
# lizzy: Plots match manuscript!

#### Figure 3 (Effect Modification)####
## SO2
sen_so2_cat <- sen_so2
sen_so2_cat$income_cat <- ntile(sen_so2_cat$med_income, 4)

delta <- dta %>% 
  as_tibble() %>% 
  select(d_ro4, d_ro6, delta_4, delta_6)

## By income categories
so2_em_cat <- lagsarlm(so2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + medtrk + 
                         car + avg_year + med_income + as_factor(income_cat):delta_6, data = sen_so2_cat, 
                       listw = wgt_so2, zero.policy = TRUE, tol.solve = 1e-20)

# get point estimates:
so2_coef_cat <- so2_em_cat$coefficients

so2_cat_coef1 <- (so2_coef_cat[4])*10 # delta6
so2_cat_coef2 <- (so2_coef_cat[4] + so2_coef_cat[13])*10 # delta6 + interaction w income cat 2
so2_cat_coef3 <- (so2_coef_cat[4] + so2_coef_cat[14])*10 # delta6 + interaction w income cat 3
so2_cat_coef4 <- (so2_coef_cat[4] + so2_coef_cat[15])*10 # delta6 + interaction w income cat 4

# get CIs:
so2_cat_vcovs <- vcov(so2_em_cat)

so2_cat_se1 <- (sqrt(so2_cat_vcovs[5,5]))*10 # st err delta6 
so2_cat_se2 <- (sqrt(so2_cat_vcovs[5,5] + so2_cat_vcovs[14,14]))*10 # st err delta6 + st err interaction w income cat 2
so2_cat_se3 <- (sqrt(so2_cat_vcovs[5,5] + so2_cat_vcovs[15,15]))*10 # st err delta6 + st err interaction w income cat 3
so2_cat_se4 <- (sqrt(so2_cat_vcovs[5,5] + so2_cat_vcovs[16,16]))*10 # st err delta6 + st err interaction w income cat 4

# plot interaction with categorical med_income
a <- tibble(
  coef = c(so2_cat_coef1, so2_cat_coef2, so2_cat_coef3, so2_cat_coef4),
  se = c(so2_cat_se1, so2_cat_se2, so2_cat_se3, so2_cat_se4)
  ) %>% 
  mutate(
    upper_ci = coef + 1.96 * se,
    lower_ci = coef - 1.96 * se
  ) %>% 
  ggplot(aes(x = 1:nrow(.))) +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), size = 1.3) +
  geom_point(aes(y = coef), size = 3) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = 'red') +
  labs(
    x = 'Income group, from low to high (by quartile)',
    y = expression('Effect estimate of heating oil #6 on SO'[2]),
    title = ''
  ) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text  = element_text(size = 14)
  )


## PM2.5
sen_pm_cat <- sen_pm
sen_pm_cat$income_cat <- ntile(sen_pm_cat$med_income, 4)

pm_em_cat <- lagsarlm(pm_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + medtrk + car + 
                        avg_year + med_income + as_factor(income_cat):delta_6, data = sen_pm_cat, 
                      listw = wgt_pm, zero.policy = TRUE, tol.solve = 1e-20)

# get point estimates:
pm_coef_cat <- pm_em_cat$coefficients

pm_cat_coef1 <- (pm_coef_cat[4])*10
pm_cat_coef2 <- (pm_coef_cat[4] + pm_coef_cat[13])*10
pm_cat_coef3 <- (pm_coef_cat[4] + pm_coef_cat[14])*10
pm_cat_coef4 <- (pm_coef_cat[4] + pm_coef_cat[15])*10

# get CIs:
pm_cat_vcovs <- vcov(pm_em_cat)

pm_cat_se1 <- (sqrt(pm_cat_vcovs[5,5]))*10
pm_cat_se2 <- (sqrt(pm_cat_vcovs[5,5] + pm_cat_vcovs[14,14]))*10
pm_cat_se3 <- (sqrt(pm_cat_vcovs[5,5] + pm_cat_vcovs[15,15]))*10
pm_cat_se4 <- (sqrt(pm_cat_vcovs[5,5] + pm_cat_vcovs[16,16]))*10

# plot interaction with categorical med_income
b <- tibble(
  coef = c(pm_cat_coef1, pm_cat_coef2, pm_cat_coef3, pm_cat_coef4),
  se = c(pm_cat_se1, pm_cat_se2, pm_cat_se3, pm_cat_se4)
) %>% 
  mutate(
    upper_ci = coef + 1.96 * se,
    lower_ci = coef - 1.96 * se
  ) %>% 
  ggplot(aes(x = 1:nrow(.))) +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), size = 1.3) +
  geom_point(aes(y = coef), size = 3) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'red') +
  labs(
    x = 'Income group, from low to high (by quartile)',
    y = expression('Effect estimate of heating oil #6 on PM'[2.5]),
    title = ''
  ) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text  = element_text(size = 14)
  )


## NO2
sen_no2_cat <- sen_no2
sen_no2_cat$income_cat <- ntile(sen_no2_cat$med_income, 4)

no2_em_cat <- lagsarlm(no2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + medtrk + car + 
                         avg_year + med_income + as_factor(income_cat):delta_6, data = sen_no2_cat, 
                       listw = wgt_no2, zero.policy = TRUE, tol.solve = 1e-20)

# get point estimates:
no2_coef_cat <- no2_em_cat$coefficients

no2_cat_coef1 <- (no2_coef_cat[4])*10
no2_cat_coef2 <- (no2_coef_cat[4] + no2_coef_cat[13])*10
no2_cat_coef3 <- (no2_coef_cat[4] + no2_coef_cat[14])*10
no2_cat_coef4 <- (no2_coef_cat[4] + no2_coef_cat[15])*10

# get CIs:
no2_cat_vcovs <- vcov(no2_em_cat)

no2_cat_se1 <- (sqrt(no2_cat_vcovs[5,5]))*10
no2_cat_se2 <- (sqrt(no2_cat_vcovs[5,5] + no2_cat_vcovs[14,14]))*10
no2_cat_se3 <- (sqrt(no2_cat_vcovs[5,5] + no2_cat_vcovs[15,15]))*10
no2_cat_se4 <- (sqrt(no2_cat_vcovs[5,5] + no2_cat_vcovs[16,16]))*10

# plot interaction with categorical med_income
c <- tibble(
  coef = c(no2_cat_coef1, no2_cat_coef2, no2_cat_coef3, no2_cat_coef4),
  se = c(no2_cat_se1, no2_cat_se2, no2_cat_se3, no2_cat_se4)
) %>% 
  mutate(
    upper_ci = coef + 1.96 * se,
    lower_ci = coef - 1.96 * se
  ) %>% 
  ggplot(aes(x = 1:nrow(.))) +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), size = 1.3) +
  geom_point(aes(y = coef), size = 3) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'red') +
  labs(
    x = 'Income group, from low to high (by quartile)',
    y = expression('Effect estimate of heating oil #6 on NO'[2]),
    title = ''
  ) + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text  = element_text(size = 14)
  )

# a / b / c
a + b + c
# lizzy: matches figure 3

## Significance testing of effect modification using ANOVA (this doesn't work since its a spatial lag model)
#anova(so2_lag, so2_em_cat, test = "Chisq")
#anova(pm_lag, pm_em_cat, test = "Chisq")
#anova(no2_lag, no2_em_cat, test = "Chisq")


#### Figure S1 ####
# lizzy: I don't have these files!!!
ci_so2 <- read.csv("../figure_s1_so2.csv")
d <- ci_so2 %>%
  ggplot() + 
  theme_bw() + 
  geom_point(aes(x = source, y = pi, color = source), position = position_dodge2(width = 0.5), size=2.5) + 
  geom_errorbar(aes(x = source, ymin = lower, ymax = upper, color = source, linetype = analysis), 
                position = position_dodge2(), size=1.2, width = 0.5) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('SO'[2])) + 
  geom_hline(yintercept = 0)

ci_pm <- read.csv("../figure_s1_pm.csv")
e <- ci_pm %>%
  ggplot() + 
  theme_bw() + 
  geom_point(aes(x = source, y = pi, color = source), position = position_dodge2(width = 0.5), size=2.5) + 
  geom_errorbar(aes(x = source, ymin = lower, ymax = upper, color = source, linetype = analysis), 
                position = position_dodge2(), size=1.2, width = 0.5) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('PM'[2.5])) + 
  geom_hline(yintercept = 0)

ci_no2 <- read.csv("../figure_s1_no2.csv")
f <- ci_no2 %>%
  ggplot() + 
  theme_bw() + 
  geom_point(aes(x = source, y = pi, color = source), position = position_dodge2(width = 0.5), size=2.5) + 
  geom_errorbar(aes(x = source, ymin = lower, ymax = upper, color = source, linetype = analysis), 
                position = position_dodge2(), size=1.2, width = 0.5) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('NO'[2])) + 
  geom_hline(yintercept = 0)

d + e + f
# lizzy: figure matches supplement!


#### Figure S2 ####
##Reading in data (Lyuou's code)
soot_13 <- read_csv('../CH_SpotSoot_2013.csv') %>% 
  janitor::clean_names() %>% 
  mutate(
    bbl = as.character(bbl),
    year_built = as.numeric(year_built)
  )

# lizzy: i get this warning, but no errors
# Warning: 29 parsing failures.
#   1: Problem with `mutate()` input `year_built`.
# ℹ NAs introduced by coercion
# ℹ Input `year_built` is `as.numeric(year_built)`. 
# 2: In mask$eval_all_mutate(dots[[i]]) : NAs introduced by coercion

pluto <- read_csv('../pluto_19v1.csv') %>% 
  janitor::clean_names() %>% 
  select(borough:ct2010, zipcode, address) # dropping some columns because we only need BBL and tracts 

# lizzy: i get this warning, but no errors
# Warning: 13 parsing failures.

# convert all acronyms to the boro codes, and padding the leading 0s in front of block and lot number
pluto_tidy <- pluto %>% 
  mutate(
    boro = ifelse(borough == 'MN', 1, ifelse( # lizzy: case_when() syntax would help here
      borough == 'BX', 2, ifelse(
        borough == 'BK', 3, ifelse(
          borough == 'QN', 4, 5
        )
      )
    )
    ),
    county_code = ifelse(borough == 'MN', '061', ifelse(
      borough == 'BX', '005', ifelse(
        borough == 'BK', '047', ifelse(
          borough == 'QN', '081', '085'
        )
      )
    )
    )
  ) %>% 
  mutate(# create geoid
    geoid = str_pad(ct2010 * 100, 6, pad = '0'),
    geoid = str_c('36', county_code, geoid, sep = ''), 
    # geoid = state code (36) + county code (061, 005, 047, 081, 085) + census tract(6-digit)
  ) %>% 
  mutate(# create bbl
    block = str_pad(block, 5, pad = '0'),
    lot = str_pad(lot, 4, pad = '0'), # padding the leading zeros for block and lot
    bbl = str_c(boro, block, lot, sep = '') # combine boro block lot to get bbl
  )

soot_13_tidy <- soot_13 %>% 
  left_join(., pluto_tidy, by = 'bbl') %>%
  select(bbl:coop, geoid)

sum(is.na(soot_13_tidy$geoid))
mean(is.na(soot_13_tidy$geoid))
## Skipping the geocoding by geoid section (since it requires a Google account); only 80 (1%) missing

## Counting buildings by census tract (Lyuou's code)
bldg_tract_13 <- soot_13_tidy %>% 
  group_by(geoid, primary_fuel) %>% 
  summarize(n = n())

ro6_13 <- bldg_tract_13 %>% 
  filter(primary_fuel == '#6')

model_sub <- dta %>% 
  inner_join(., ro6_13, by = 'geoid') %>% 
  select(-n)


##Subset analysis: SO2
so2_sub <- model_sub %>% 
  filter(!is.na(so2_diff)) %>% 
  as_Spatial()
nb_so2_sub <- poly2nb(so2_sub)
wgt_so2_sub <- nb2listw(nb_so2_sub, zero.policy = TRUE)

# lizzy: in the models above, 'zero.policy = TRUE' is only in the spatial regression model
# does this change anything?

so2_lag_sub <- lagsarlm(so2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + 
                          bus + hvytrk + medtrk + car + avg_year + med_income, 
                        data = so2_sub, listw = wgt_so2_sub, tol.solve = 1e-20, zero.policy = TRUE)

summary(so2_lag_sub)$Coef %>% 
  as_tibble(rownames = 'variable_name') %>% 
  janitor::clean_names() %>% 
  knitr::kable()


##Subset analysis: PM2.5
pm_sub <- model_sub %>% 
  filter(!is.na(pm_diff)) %>% 
  as_Spatial()
nb_pm_sub <- poly2nb(pm_sub)
wgt_pm_sub <- nb2listw(nb_pm_sub, zero.policy = TRUE)

pm_lag_sub <- lagsarlm(pm_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                         medtrk + car + avg_year + med_income, data = pm_sub, listw = wgt_pm_sub, 
                       tol.solve = 1e-20, zero.policy = TRUE)
summary(pm_lag_sub)$Coef %>% 
  as_tibble(rownames = 'variable_name') %>% 
  janitor::clean_names() %>% 
  knitr::kable()


##Subset analysis: NO2
no2_sub <- model_sub %>% 
  filter(!is.na(no2_diff)) %>% 
  as_Spatial()
nb_no2_sub <- poly2nb(no2_sub)
wgt_no2_sub <- nb2listw(nb_no2_sub, zero.policy = TRUE)

no2_lag_sub <- lagsarlm(no2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                          medtrk + car + avg_year + med_income, data = no2_sub, listw = wgt_no2_sub, 
                        tol.solve = 1e-20, zero.policy = TRUE)
summary(no2_lag_sub)$Coef %>% 
  as_tibble(rownames = 'variable_name') %>% 
  janitor::clean_names() %>% 
  knitr::kable()


## Plotting results
## Clean names

# lizzy: deleted repeated segment

so2_coef_sub <- summary(so2_lag_sub)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()
  
pm_coef_sub <- summary(pm_lag_sub)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

no2_coef_sub <- summary(no2_lag_sub)$Coef %>% 
  as_tibble(rownames = 'var_name') %>% 
  janitor::clean_names()

## Renaming (so plots work)
so2_coef_sub$var_name[4] <- "Delta_6"
pm_coef_sub$var_name[4] <- "Delta_6"
no2_coef_sub$var_name[4] <- "Delta_6"

# lizzy: this works, but is confusing! should have changed to 'Subset' or something obvious

## Calculate CIs (by per 10)
so2 <- rbind(so2_coef[4, ], so2_coef_sub[4, ]) %>% 
  mutate(pi = estimate*10,
         upper = (estimate*10) + 1.96 * (std_error*10),
         lower = (estimate*10) - 1.96 * (std_error*10))

pm <- rbind(pm_coef[4, ], pm_coef_sub[4, ]) %>% 
  mutate(pi = estimate*10,
         upper = (estimate*10) + 1.96 * (std_error*10),
         lower = (estimate*10) - 1.96 * (std_error*10))

no2 <- rbind(no2_coef[4, ], no2_coef_sub[4, ]) %>% 
  mutate(pi = estimate*10,
         upper = (estimate*10) + 1.96 * (std_error*10),
         lower = (estimate*10) - 1.96 * (std_error*10))

# lizzy: ^ don't reuse data frame names!
# lizzy: don't reuse plot names!

## Actual plots
so2_plot <- so2 %>% 
  ggplot() + 
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name), size = 2.5) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name), size = 1.2) +
  scale_x_discrete(labels = c('Original', 'Subset')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('SO'[2])) + 
  geom_hline(yintercept = 0)

pm_plot <- pm %>% 
  ggplot() +  
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name), size = 2.5) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name), size = 1.2) +
  scale_x_discrete(labels = c('Original', 'Subset')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face="bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('PM'[2.5])) + 
  geom_hline(yintercept = 0)

no2_plot <- no2 %>% 
  ggplot() +  
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name), size = 2.5) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name), size = 1.2) +
  scale_x_discrete(labels = c('Original', 'Subset')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank()) + 
  labs(x = expression('NO'[2])) + 
  geom_hline(yintercept = 0)

## This looks very similiar to Figure 2...as expected
so2_plot + pm_plot + no2_plot
# lizzy: looks like figure S2!


#### Table S2 ####
## For this, we're using weight = wgt_no2 for all in order to be able to compare the residuals in the next step 
## (need same sample size)
## SO2
sen_no2 <- dta %>% 
  filter(!is.na(no2_diff)) %>% 
  as_Spatial()

nb_no2 <- poly2nb(sen_no2)
wgt_no2 <- nb2listw(nb_no2)

so2_ols <- lm(so2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + medtrk + car + 
                avg_year + med_income, data = sen_no2)

so2_ols %>% 
  broom::tidy() %>% 
  mutate(lower = estimate - 1.96 * std.error,
         upper = estimate + 1.96 * std.error
  )

lm.LMtests(so2_ols, wgt_no2, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"), zero.policy = TRUE)
# lizzy: Run Langrane Multiplier tests to identify the type of spatial regression model to run.

# lizzy: why didn't you run a Durbin model? Since robust lag and error are both significant

so2_lag_re <- lagsarlm(so2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                         medtrk + car + avg_year + med_income, data = sen_no2, listw = wgt_no2, 
                       tol.solve = 1e-20, zero.policy = TRUE)

summary(so2_lag_re)$Coef %>% 
  as_tibble(rownames = 'term') %>% 
  janitor::clean_names() %>% 
  mutate(
    lower = estimate - 1.96 * std_error,
    upper = estimate + 1.96 * std_error
  )


## PM2.5
pm_ols <- lm(pm_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + medtrk + car + 
                avg_year + med_income, data = sen_no2)

pm_ols %>% 
  broom::tidy() %>% 
  mutate(lower = estimate - 1.96 * std.error,
         upper = estimate + 1.96 * std.error
  )

lm.LMtests(pm_ols, wgt_no2, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"), zero.policy = TRUE)

pm_lag_re <- lagsarlm(pm_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                         medtrk + car + avg_year + med_income, data = sen_no2, listw = wgt_no2, 
                       tol.solve = 1e-20, zero.policy = TRUE)

# lizzy: typo here, but doesn't affect results
summary(pm_lag_re)$Coef %>% 
  as_tibble(rownames = 'term') %>% 
  janitor::clean_names() %>% 
  mutate(
    lower = estimate - 1.96 * std_error,
    upper = estimate + 1.96 * std_error
  )


## NO2
no2_ols <- lm(no2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + medtrk + car + 
               avg_year + med_income, data = sen_no2)

no2_ols %>% 
  broom::tidy() %>% 
  mutate(lower = estimate - 1.96 * std.error,
         upper = estimate + 1.96 * std.error
  )

lm.LMtests(no2_ols, wgt_no2, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"), zero.policy = TRUE)

no2_lag_re <- lagsarlm(no2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                        medtrk + car + avg_year + med_income, data = sen_no2, listw = wgt_no2, 
                      tol.solve = 1e-20, zero.policy = TRUE)

# lizzy: same here, typo that doesn't matter
summary(no2_lag_re)$Coef %>% 
  as_tibble(rownames = 'term') %>% 
  janitor::clean_names() %>% 
  mutate(
    lower = estimate - 1.96 * std_error,
    upper = estimate + 1.96 * std_error
  )

# OLS residuals
sen_no2 <- dta %>% 
  filter(!is.na(no2_diff))

res_ols <- sen_no2 %>% 
  as_tibble() %>% 
  spread_residuals(data = ., so2_ols, pm_ols, no2_ols) %>% # lizzy: i have never seen this function, cool!
  select(geoid, geometry, so2_ols:no2_ols)

# lag residuals
missing_income <- sen_no2 %>% 
  as_tibble() %>% 
  mutate(
    seq = rownames(.),
    seq = as.numeric(seq)
  ) %>% 
  filter(is.na(med_income)) %>% 
  select(geoid, geometry, seq)

missing_income$so2 <- NA
missing_income$pm <- NA
missing_income$no2 <- NA

res_lag <- sen_no2 %>% 
  as_tibble() %>% 
  mutate(seq = rownames(.),
         seq = as.numeric(seq)) %>%
  filter(!is.na(med_income)) %>% 
  select(geoid, geometry, seq) %>% 
  cbind(., summary(so2_lag_re)$residuals, summary(pm_lag_re)$residuals, summary(no2_lag_re)$residuals) %>% 
  rename(
    so2 = `summary(so2_lag_re)$residuals`,
    pm = `summary(pm_lag_re)$residuals`,
    no2 = `summary(no2_lag_re)$residuals`
  ) %>% 
  rbind(., missing_income) %>% 
  arrange(seq)


## Moran's I
#SO2
moran.test(res_ols$so2_ols,listw = wgt_no2, na.action = na.omit, zero.policy = TRUE)
moran.test(res_lag$so2,listw = wgt_no2, na.action = na.exclude, zero.policy = TRUE)

#PM2.5
moran.test(res_ols$pm_ols,listw = wgt_no2, na.action = na.omit, zero.policy = TRUE)
moran.test(res_lag$pm,listw = wgt_no2, na.action = na.exclude, zero.policy = TRUE)

#NO2
moran.test(res_ols$no2_ols,listw = wgt_no2, na.action = na.omit, zero.policy = TRUE)
moran.test(res_lag$no2,listw = wgt_no2, na.action = na.exclude, zero.policy = TRUE)

# lizzy: matches table S2
# you can't really have a p-value of 1, it is just rounded by R, so I would put >0.999 in the table