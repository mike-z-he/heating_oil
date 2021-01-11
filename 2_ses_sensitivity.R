#### SES Sensitivity Analysis
#### December 1, 2020

## Required packages
library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(janitor)
library(patchwork)


#setwd("D:/Users/profu/Documents/Schoolwork/PhD/Research Projects/heating_oil/lyuou")
options(mc.cores=parallel::detectCores())



####Basic Models: SO2, PM2.5, NO2####

### Reading in data
# lizzy: changed to relative path
dta <- st_read('../model_data_export.shp')
head(dta)

### Remove outliers ###
dta <- dta[-which(dta$d_ro6>200 | dta$d_ro6< -20),]

dta <- dta[-which((dta$delta_6< 5 & dta$d_ro6 >25)|dta$d_ro6 > 45),]

dta <- dta[-which(dta$d_ro4 >25), ]


### SO2
sen_so2 <- dta %>% 
  filter(!is.na(so2_diff)) %>% 
  as_Spatial()

nb_so2 <- poly2nb(sen_so2)
wgt_so2 <- nb2listw(nb_so2)

## Spatial Lag Model, using new RO4 and RO6 data (delta_4 and delta_6)
# lizzy: without ave yr or median income
so2_lag <- lagsarlm(so2_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                      medtrk + car, data = sen_so2, listw = wgt_so2, 
                    zero.policy = TRUE, tol.solve = 1e-20)
summary(so2_lag)$Coef %>% 
  knitr::kable(.)

## Original Model (RO2, RO4, RO6)
so2_lag_original <- lagsarlm(so2_diff ~ d_ro2 + d_ro4 + d_ro6 + d_ng + d_d2 + bus + hvytrk + 
                               medtrk + car, data = sen_so2, listw = wgt_so2, 
                             tol.solve = 1e-20, zero.policy = TRUE)
summary(so2_lag_original)$Coef %>%
  knitr::kable(.)


### PM2.5
sen_pm <- dta %>% 
  filter(!is.na(pm_diff)) %>% 
  as_Spatial()

nb_pm <- poly2nb(sen_pm)
wgt_pm <- nb2listw(nb_pm)

## New Model
pm_lag <- lagsarlm(pm_diff ~ d_ro2 + delta_4 + delta_6 + d_ng + d_d2 + bus + hvytrk + 
                     medtrk + car, data = sen_pm, listw = wgt_pm, 
                   tol.solve = 1e-20, zero.policy = TRUE)
summary(pm_lag)$Coef

## Original Model
pm_lag_original <- lagsarlm(pm_diff ~ d_ro2 + d_ro4 + d_ro6 + d_ng + d_d2 + bus + hvytrk + 
                              medtrk + car, data = sen_pm, listw = wgt_pm, 
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
                      medtrk + car, data = sen_no2, listw = wgt_no2, 
                    tol.solve = 1e-20, zero.policy = TRUE)
summary(no2_lag)$Coef

## Original Model
no2_lag_original <- lagsarlm(no2_diff ~ d_ro2 + d_ro4 + d_ro6 + d_ng + d_d2 + bus + hvytrk + 
                               medtrk + car, data = sen_no2, listw = wgt_no2, 
                             tol.solve = 1e-20, zero.policy = TRUE)
summary(no2_lag_original)$Coef


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

## Calculate CIs
so2 <- rbind(so2_coef[4, ], so2_coef_original[4, ]) %>% 
  mutate(upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

pm <- rbind(pm_coef[4, ], pm_coef_original[4, ]) %>% 
  mutate(upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

no2 <- rbind(no2_coef[4, ], no2_coef_original[4, ]) %>% 
  mutate(upper = estimate + 1.96 * std_error,
         lower = estimate - 1.96 * std_error)

## Calculate CIs (by per 10)
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


## Actual plots
so2_plot <- so2 %>% 
  ggplot() + 
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name)) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name)) +
  ylim(0, 0.22) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face="bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank()) + 
  labs(x = expression('SO'[2])) + 
  geom_hline(yintercept = 0)

pm_plot <- pm %>% 
  ggplot() +  
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name)) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name)) +
  ylim(0, 0.086) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face="bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank()) + 
  labs(x = expression('PM'[2.5])) + 
  geom_hline(yintercept = 0)

no2_plot <- no2 %>% 
  ggplot() +  
  theme_bw() + 
  geom_point(aes(x = var_name, y = pi, color = var_name)) + 
  geom_errorbar(aes(x = var_name, ymin = lower, ymax = upper, color = var_name)) +
  ylim(0, 0.27) +
  scale_x_discrete(labels = c('Benchmark', 'Spot the Soot')) + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank()) + 
  labs(x = expression('NO'[2])) + 
  geom_hline(yintercept = 0)

## This looks very similar to the plot in the markdown doc, but still slightly different...
so2_plot + pm_plot + no2_plot
