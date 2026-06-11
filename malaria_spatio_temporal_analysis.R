# Loading libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(MASS)
library(AER)
library(corrplot)
library(dplyr)
library(stats)
library(car)

#Malaria data 
malaria <- read_xlsx("PASCAL DATA.xlsx") |>
  separate_wider_delim(Month, delim = " ", names = c("Month", "Year")) |>
  arrange(Region)

## Climate Data
# 2020
climate_2020 <- read_xls("GENERALIZED LINEAR_\\DATASET_\\CLIMATIC 2020.xls") |>
  dplyr::select(-1) |>
  dplyr::rename(date = datetime...3) |>
  mutate(Month = month(date, label = TRUE, abbr = FALSE),
         Year = year(date)) |>
  group_by(Year, Month, name) |>
  summarise(
    temp = mean(temp),
    precip = sum(precip),
    humidity = mean(humidity),
    feelslike = mean(feelslike),
    dew = mean(dew),
    sealevelpressure = mean(sealevelpressure),
    solarradiation = mean(solarradiation),
    solarenergy = mean(solarenergy),
    uvindex = mean(uvindex)
  ) |>
  mutate(precip = precip * 100) |>  
  arrange(name) |>
  ungroup()


# 2021
climate_2021 <- read_xls("GENERALIZED LINEAR_\\DATASET_\\CLIMATIC 2021.xls") |>
  mutate(Month = month(datetime, label = TRUE, abbr = FALSE),
         Year = year(datetime)) |>
  group_by(Year, Month, name) |>     # ⭐ Include Year so it does not disappear
  summarise(
    temp = mean(temp),
    precip = sum(precip),
    humidity = mean(humidity),
    feelslike = mean(feelslike),
    dew = mean(dew),
    sealevelpressure = mean(sealevelpressure),
    solarradiation = mean(solarradiation),
    solarenergy = mean(solarenergy),
    uvindex = mean(uvindex)
  ) |>
  mutate(precip = precip * 100) |>   
  arrange(name) |>
  ungroup() |>
  slice(-97)
# 2022
climate_2022 <- read_xlsx("GENERALIZED LINEAR_\\DATASET_\\CLIMATIC 2022.xlsx") |>
  mutate(Month = month(datetime, label = TRUE, abbr = FALSE),
         Year = year(datetime)) |>
  group_by(Year, Month, name) |>     
  summarise(
    temp = mean(temp),
    precip = sum(precip),
    humidity = mean(humidity),
    feelslike = mean(feelslike),
    dew = mean(dew),
    sealevelpressure = mean(sealevelpressure),
    solarradiation = mean(solarradiation),
    solarenergy = mean(solarenergy),
    uvindex = mean(uvindex)
  ) |>
  mutate(precip = precip * 100) |>    
  arrange(name) |>
  ungroup() |>
  slice(-97)


## 2023
climate_2023 <- read_xlsx("GENERALIZED LINEAR_\\DATASET_\\CLIMATIC 2023.xlsx") |>
  mutate(Month = month(datetime, label = TRUE, abbr = FALSE),
         Year = year(datetime)) |>
  group_by(Year, Month, name) |>     
  summarise(
    temp = mean(temp),
    precip = sum(precip),
    humidity = mean(humidity),
    feelslike = mean(feelslike),
    dew = mean(dew),
    sealevelpressure = mean(sealevelpressure),
    solarradiation = mean(solarradiation),
    solarenergy = mean(solarenergy),
    uvindex = mean(uvindex)
  ) |>
  mutate(precip = precip * 100) |>    
  arrange(name) |>
  ungroup() |>
  slice(-97)

## 2024
climate_2024 <- read_xlsx("GENERALIZED LINEAR_\\DATASET_\\CLIMATIC 2024.xlsx") |>
  mutate(Month = month(datetime, label = TRUE, abbr = FALSE),
         Year = year(datetime)) |>
  group_by(Year, Month, name) |>     
  summarise(
    temp = mean(temp),
    precip = sum(precip),
    humidity = mean(humidity),
    feelslike = mean(feelslike),
    dew = mean(dew),
    sealevelpressure = mean(sealevelpressure),
    solarradiation = mean(solarradiation),
    solarenergy = mean(solarenergy),
    uvindex = mean(uvindex)
  ) |>
  mutate(precip = precip * 100) |>    
  arrange(name) |>
  ungroup() |>
  slice(-97)

#Climate data
# Combine all climate data (Year already included)
malaria <- malaria |>
  mutate(Year = as.numeric(Year))

climate <- bind_rows(
  climate_2020,
  climate_2021,
  climate_2022,
  climate_2023,
  climate_2024
)

# Merge with malaria
malaria_climate <- climate |>
  left_join(malaria,
            by = c("name" = "Region",
                   "Month",
                   "Year")) |>
  filter(Year<2025)|>
  mutate(Date = ymd(paste(Year, Month, "01", sep = "-")))


#### Visualize Incidence against  Rainfall ####

#####---- RAINFALL AND REGIONS ---- ####

regions <- c("Ashanti", "Ahafo",
  "Bono", "Bono East", "Central", "Eastern", "Greater Accra", 
  "North East", "Northern", "Oti", "Savannah", "Upper East", 
  "Upper West", "Volta", "Western", "Western North"
)

plots <- list()

for (reg in regions) {
  df_region <- malaria_climate %>%
    filter(name == reg, Year<2025) %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
    group_by(Month, Year) %>%
    summarise(
      precip = sum(precip, na.rm = TRUE),
      malaria = mean(`Uncomplicated Malaria Tested Positive`, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Date = ymd(paste(Year, Month, "01", sep = "-")))
  
  max_malaria <- max(df_region$malaria, na.rm = TRUE)
  max_precip  <- max(df_region$precip, na.rm = TRUE)
  scale_factor <- max_malaria / max_precip
  
  p <- ggplot(df_region, aes(x = Date)) +
    geom_col(aes(y = malaria), fill = "black") +
    xlab("Year") +
    geom_line(aes(y = precip * scale_factor), color = "red", size = 1) +
    geom_point(aes(y = precip * scale_factor), color = "red4", size = 2) +
    scale_y_continuous(
      name = "Malaria Incidence",
      sec.axis = sec_axis(
        trans = ~ . / scale_factor,
        name = "Rainfall (mm)"
      )
    ) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months",
                 limits = c(ymd("2020-01-01"), ymd("2024-12-01")),
) + 
    labs(title = reg) +
    theme_classic() +
    theme(
      axis.title.y = element_text(size = 10),
      axis.title.y.right = element_text(color = "red", size = 10),
      axis.text.y.right = element_text(color = "red"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
    )
  
  plots[[reg]] <- p
}

pages <- split(plots, ceiling(seq_along(plots)/4))  

for (i in seq_along(pages)) {
  page_plot <- wrap_plots(pages[[i]], ncol = 2, nrow = 2) + 
    plot_annotation(title = paste("Malaria Cases and Rainfall"))
  
  print(page_plot)
  
}


#### ----- Determination of Lags ---- #### 
vars <- c(
  "precip",
  "temp",
  "humidity",
  "dew",
  "solarenergy",
  "solarradiation",
  "uvindex",
  "feelslike",
  "sealevelpressure"
)

max_lag <- 3
response_var <- "Uncomplicated Malaria Tested Positive"

# Loop through each variable
for(varname in vars){
  
  cat("\n\n===== Variable:", varname, "=====\n")
  
  lag_results <- data.frame(
    lag = 0:max_lag,
    correlation = NA,
    p_value = NA
  )
  
  x <- malaria_climate[[varname]]
  y <- malaria_climate[[response_var]]
  
  for(l in 0:max_lag){
    x_lag <- dplyr::lag(x, l)
    test <- cor.test(y, x_lag, use="complete.obs")
    lag_results$correlation[l+1] <- test$estimate
    lag_results$p_value[l+1] <- test$p.value
  }
  
  print(lag_results)
  
  significant_lags <- lag_results %>%
    filter(p_value < 0.05) %>%
    pull(lag)
  
  cat("Significant lags for", varname, ":", significant_lags, "\n")
  
  if(length(significant_lags) > 0){
    lagged_vars <- data.frame(lapply(significant_lags, function(l) dplyr::lag(x, l)))
    colnames(lagged_vars) <- paste0(varname, "_lag", significant_lags)
    
    #  correlation among the  lags
    cor_matrix <- cor(lagged_vars, use = "complete.obs")
    print(cor_matrix)
  }
}


#  lag plot
lag_plot_df <- bind_rows(
  lapply(vars, function(varname) {
    x <- malaria_climate[[varname]]
    y <- malaria_climate[[response_var]]
    
    df <- data.frame(
      lag = 0:max_lag,
      correlation = sapply(0:max_lag, function(l) cor(y, dplyr::lag(x, l), use = "complete.obs")),
      p_value = sapply(0:max_lag, function(l) cor.test(y, dplyr::lag(x, l), use = "complete.obs")$p.value)
    )
    
    df$variable <- varname
    df
  })
)

# plot
ggplot(lag_plot_df, aes(x = lag, y = correlation, color = variable)) +
  geom_line(linewidth = 1) +   # updated from size = 1
  geom_point(size = 2) +
  labs(
    title = "Correlation vs Lag Month",
    x = "Lag (months)",
    y = "Pearson Correlation",
    color = "Variable"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )


malaria_climate$month <- as.integer(format(malaria_climate$Date, "%m"))

malaria_lagged <- malaria_climate

malaria_lagged$season <- with(malaria_lagged,
                          ifelse(month %in% 4:7, "Major Rainy",
                                 ifelse(month %in% 9:10, "Minor Rainy", "Dry"))
)

malaria_lagged$season <- factor(malaria_lagged$season,
                            levels = c("Dry", "Major Rainy", "Minor Rainy"))

malaria_lagged$month <- malaria_climate$month
malaria_lagged$season <- with(malaria_lagged,
                              ifelse(month %in% 4:7, "Major Rainy",
                                     ifelse(month %in% 9:10, "Minor Rainy", "Dry"))
)

malaria_lagged$season <- factor(malaria_lagged$season,
                                levels = c("Dry", "Major Rainy", "Minor Rainy"))

## lagged covariates 
selected_lags <- list(
  precip = c(0,1,2,3),
  temp = c(0),
  humidity = c(0,1),
  dew = c(0,1,2,3),
  solarenergy = c(0),
  uvindex = c(1),
  feelslike = c(0),
  sealevelpressure = c(0)
)

malaria_lagged_lags <- malaria_lagged

for(var in names(selected_lags)){
  for(lag_val in selected_lags[[var]]){
    col_name <- paste0(var, "_lag", lag_val)
    malaria_lagged_lags[[col_name]] <- dplyr::lag(malaria_lagged_lags[[var]], lag_val)
  }
}

lagged_vars <- unlist(lapply(names(selected_lags), function(v) {
  paste0(v, "_lag", selected_lags[[v]])
}))

model_data <- malaria_lagged_lags[, c("Uncomplicated Malaria Tested Positive", lagged_vars, "season")]

model_data[lagged_vars] <- scale(model_data[lagged_vars], center = TRUE, scale = FALSE) 



#### ---- VIF CHECK BEFORE MODEL FITTING ---- ####

temp_model <- lm(
  `Uncomplicated Malaria Tested Positive` ~ 
    precip_lag0 + precip_lag1 + precip_lag2 + precip_lag3 +
    temp_lag0 + 
    humidity_lag0 + humidity_lag1 +
    dew_lag0 + dew_lag1 + dew_lag2 + dew_lag3 + 
    solarenergy_lag0 + 
    uvindex_lag1 +        # <-- lag1, not lag0
    feelslike_lag0 + 
    sealevelpressure_lag0 + 
    season,
  data = model_data
)
# Compute VIF

vif(temp_model)


####
model_data <-model_data |>
  mutate(Year = malaria_lagged$Year,
         Month = malaria_lagged$Month,
         Region = malaria_climate$name)


# Define only the final selected lags after VIF removal
selected_final_vars <- c(
  "precip_lag0", "precip_lag1", "precip_lag2", "precip_lag3",
  "temp_lag0",
  "humidity_lag1",
  "dew_lag2", "dew_lag3",
  "solarenergy_lag0",
  "uvindex_lag1",
  "sealevelpressure_lag0"
)

# Save scaling parameters for interpretation
scaling_params <- data.frame(
  variable = selected_final_vars,
  mean     = colMeans(model_data[, selected_final_vars], 
                      na.rm = TRUE),
  sd       = apply(model_data[, selected_final_vars], 
                   2, sd, na.rm = TRUE)
)

model_data[selected_final_vars] <- scale(
  model_data[selected_final_vars],
  center = TRUE,
  scale  = TRUE
)

####
model_data <- model_data |>
  mutate(
    Year   = malaria_lagged$Year,
    Month  = malaria_lagged$Month,
    Region = malaria_climate$name
  )

### Fit Poisson model 
poisson_model <- glm(
  formula = `Uncomplicated Malaria Tested Positive` ~ 
    precip_lag0 + precip_lag1 + precip_lag2 + precip_lag3 +
    temp_lag0 +
    humidity_lag1 +
    dew_lag2 + dew_lag3 +
    solarenergy_lag0 +
    uvindex_lag1 +
    sealevelpressure_lag0 +
    season,
  data = model_data,
  family = poisson(link = "log")
)

# Overdispersion test : Pearson method 
dispersiontest(poisson_model, alternative = "greater")

#### ---- NEGATIVE BINOMIAL MODEL FIT ----- ####
nb_model <- glm.nb(
  `Uncomplicated Malaria Tested Positive` ~
    precip_lag0 + precip_lag1 + precip_lag2 + precip_lag3 +
    temp_lag0 +
    humidity_lag1 +
    dew_lag2 + dew_lag3 +
    solarenergy_lag0 +
    uvindex_lag1 +
    sealevelpressure_lag0 +
    season,
  data = model_data
)


#### ---- POISSON VS NB COMPARISON TABLE ---- ####

# Get Poisson estimates
poisson_coef <- summary(poisson_model)$coefficients
poisson_irr  <- exp(poisson_coef[, 1])
poisson_ci   <- exp(confint(poisson_model))
poisson_p    <- poisson_coef[, 4]

# Get NB estimates
nb_coef <- summary(nb_model)$coefficients
nb_irr  <- exp(nb_coef[, 1])
nb_ci   <- exp(confint(nb_model))
nb_p    <- nb_coef[, 4]

# Build comparison dataframe
comparison_table <- data.frame(
  Variable = rownames(nb_coef),
  
  # Poisson
  Poisson_IRR   = round(poisson_irr, 3),
  Poisson_Lower = round(poisson_ci[, 1], 3),
  Poisson_Upper = round(poisson_ci[, 2], 3),
  Poisson_p     = round(poisson_p, 4),
  
  # NB
  NB_IRR   = round(nb_irr, 3),
  NB_Lower = round(nb_ci[, 1], 3),
  NB_Upper = round(nb_ci[, 2], 3),
  NB_p     = round(nb_p, 4)
)

print(comparison_table)

# Model fit comparison

cat("Poisson AIC:", AIC(poisson_model), "\n")
cat("NB AIC:     ", AIC(nb_model), "\n\n")

cat("Poisson BIC:", BIC(poisson_model), "\n")
cat("NB BIC:     ", BIC(nb_model), "\n\n")

cat("Poisson Log-Likelihood:", logLik(poisson_model), "\n")
cat("NB Log-Likelihood:     ", logLik(nb_model), "\n\n")

cat("Poisson Residual Deviance:", poisson_model$deviance, "\n")
cat("NB Residual Deviance:     ", nb_model$deviance, "\n\n")

# Poisson dispersion 
poisson_dispersion <- sum(residuals(poisson_model, type = "pearson")^2) /
  poisson_model$df.residual

cat("Poisson Dispersion Parameter:", poisson_dispersion, "\n")

# NB dispersion (theta)
cat("Negative Binomial Dispersion (theta):", nb_model$theta, "\n")


# Pearson residuals
residuals_nb <- residuals(nb_model, type = "pearson")
phi <- sum(residuals_nb^2) / nb_model$df.residual
phi



library(MASS)
library(pscl)

library(MASS)

# =========================
# REDUCED MODEL (CLIMATE ONLY)
# =========================

nb_reduced <- glm.nb(
  `Uncomplicated Malaria Tested Positive` ~ 
    precip_lag0 + precip_lag1 + precip_lag2 + precip_lag3 +
    temp_lag0 +
    humidity_lag1,
  data = model_data
)

# =========================
# FULL MODEL
# =========================

nb_full <- nb_model

# =========================
# 1. LIKELIHOOD RATIO TEST
# =========================


lrt_nb <- anova(nb_reduced, nb_full, test = "Chisq")
print(lrt_nb)

# =========================
# 2. AIC COMPARISON
# =========================


AIC(nb_reduced, nb_full)



#### ---- RESIDUAL DIAGNOSTIC PLOTS ---- ####
# Pearson residuals
resid_pearson  <- residuals(nb_model, type = "pearson")
resid_deviance <- residuals(nb_model, type = "deviance")
fitted_vals    <- fitted(nb_model)
sqrt_abs_resid <- sqrt(abs(resid_pearson))

diag_df <- data.frame(
  fitted         = fitted_vals,
  pearson_resid  = resid_pearson,
  deviance_resid = resid_deviance,
  sqrt_abs_resid = sqrt_abs_resid
)

# (a) Residuals vs Fitted
p1 <- ggplot(diag_df, aes(x = fitted, y = pearson_resid)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 0.8) +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Pearson Residuals") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# (b) Q-Q Plot
p2 <- ggplot(diag_df, aes(sample = deviance_resid)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Deviance Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# (c) Histogram
p3 <- ggplot(diag_df, aes(x = pearson_resid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey70", color = "white") +
  geom_density(color = "blue", linewidth = 0.8) +
  labs(title = "Distribution of Pearson Residuals", x = "Pearson Residuals", y = "Density") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# (d) Scale-Location
p4 <- ggplot(diag_df, aes(x = fitted, y = sqrt_abs_resid)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 0.8) +
  labs(title = "Scale-Location", x = "Fitted Values", y = expression(sqrt("|Pearson Residuals|"))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Print panel
(p1 | p2) / (p3 | p4)


ggsave(
  filename = "nb_diagnostic_plots.png",
  plot = (p1 | p2) / (p3 | p4),
  width  = 16,
  height = 14,
  dpi    = 300
)







#### ----- Spatio-Temporal Modeling ---- ### 
install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable")
install.packages("spdep")
install.packages("sf")
install.packages("tmap")
install.packages("janitor") 
install.packages("spdep")# run once
library(RColorBrewer)
library(spdep)
library(janitor)
library(stringr)
library(sf)  # for shapefile
library(spdep) # for spatial tests
library(INLA)
library(tmap)
library(classInt)
library(grid) # for unit()


### Load shape file 
regions <- st_read("Districts_271/District_272.shp")
regions <- st_make_valid(regions)

# Fix region names and summarise geometry
regions <- regions %>%
  mutate(Region = ifelse(Region == "Greate Accra", "Greater Accra", Region)) %>%
  group_by(Region) %>%
  summarise(geometry = st_union(geometry))

#### ----- Prepare malaria data -----
malaria_climate <- malaria_climate %>%
  janitor::clean_names() %>%
  mutate(Region = name)

mal_region <- malaria_climate %>%
  group_by(Region) %>%
  summarise(uncomplicated_malaria_tested_positive = sum(uncomplicated_malaria_tested_positive, na.rm = TRUE))

#### ----- Merge with shapefile -----
shp_data <- regions %>%
  left_join(mal_region, by = "Region") %>%
  st_make_valid()

### -----Spatial Dependency Tests------ #######
# Monte Carlo Moran’s I
set.seed(123)
nb <- poly2nb(shp_data, snap = 0.001)
lw <- nb2listw(nb, style = "B")  
moran_mc <- moran.mc(shp_data$`Uncomplicated Malaria Tested Positive`, lw, nsim = 99999)
print(moran_mc)


### -----Spatial Heterogeneity Tests------ #######
# Chi-square test for heterogeneity
counts <- shp_data$`Uncomplicated Malaria Tested Positive`
chisq_test <- chisq.test(counts)
chisq_test


#### ----- INLA adjacency -----
nb_region <- poly2nb(shp_data)
adj_mat <- nb2mat(nb_region, style = "B", zero.policy = TRUE)
INLA::inla.write.graph(adj_mat, file = "region_adj.graph")


#### ----- Prepare population data -----
population_df <- data.frame(
  Year = 2021:2024,
  Western = c(2060585,2106523,2153191,2200146),
  Central = c(2859821,2936148,3013687,3091703),
  `Greater Accra` = c(5455692,5623395,5793761,5965173),
  Volta = c(1659040,1679652,1700591,1721658),
  Eastern = c(2925653,2959585,2994055,3028737),
  Ashanti = c(5440463,5517037,5594826,5673094),
  `Western North` = c(880921,900582,920556,940652),
  Ahafo = c(564668,574002,583483,593024),
  Bono = c(1208649,1241830,1275539,1309454),
  `Bono East` = c(1203400,1238114,1273379,1308862),
  Savannah = c(653277,674595,696252,718041),
  `North East` = c(658946,681444,704300,727296),
  `Upper East` = c(1301226,1330771,1360784,1390982),
  `Upper West` = c(901502,924633,948131,971773),
  Oti = c(747248,760050,773056,786142),
  Northern = c(2310928,2399787,2490056,2580881)
  
  
)

# Estimate 2020 population
population_2020 <- (population_df[1,-1]*2) - population_df[2,-1]
population_2020 <- data.frame(Year = 2020, population_2020)
ghana_population <- rbind(population_2020, population_df)
# ---- Long format
pop_long <- ghana_population %>%
  pivot_longer(
    -Year,
    names_to = "Region",
    values_to = "Population"
  )


pop_long <- pop_long %>%
  mutate(
    Region = Region %>%
      str_replace_all("\\.", " ") %>%  # replace dots with spaces
      str_squish() %>%                 # remove extra spaces
      str_trim()                        # trim ends
  )

model_data <- model_data %>%
  mutate(
    Region = str_squish(str_trim(Region))
  )

### Merge all

model_data3 <- model_data %>%
  left_join(pop_long, by = c("Region", "Year")) %>%
  left_join(regions, by = "Region") %>%
  st_as_sf()


model_data4 <- model_data3 %>%
  # Compute expected counts proportional to population for each year
  group_by(Year) %>%
  mutate(
    expected = Population * sum(`Uncomplicated Malaria Tested Positive`, na.rm = TRUE) /
      sum(Population, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Create INLA-friendly indices
  mutate(
    region_id  = as.numeric(factor(Region)),  # unique numeric ID for each region
    region_iid = region_id,                   # independent region effect (iid)
    time_id    = Year - min(Year) + 1,        # time index starting at 1
    st_id      = 1:n()                        # unique ID for spatio-temporal interaction
  )


model_data5 <- model_data4 %>%
  clean_names() 



# ============================================================
#### ---- FULL MODEL FIT WITH CPO ---- ####
# ============================================================

formula_nb <- uncomplicated_malaria_tested_positive ~
  precip_lag0 + precip_lag1 + precip_lag2 + precip_lag3 +
  temp_lag0 +
  humidity_lag1 +
  dew_lag2 + dew_lag3 +
  solarenergy_lag0 +
  uvindex_lag1 +
  sealevelpressure_lag0 + season +
  offset(log(expected)) +
  f(region_id,  model = "besag",
    graph = "region_adj.graph",
    hyper = list(prec = list(
      prior = "pc.prec",
      param = c(0.5, 0.01)))) +   # spatial prior: sigma=0.5
  f(region_iid, model = "iid",
    hyper = list(prec = list(
      prior = "pc.prec",
      param = c(0.5, 0.01)))) +   # unstructured prior: sigma=0.5
  f(time_id,    model = "rw1",
    hyper = list(prec = list(
      prior = "pc.prec",
      param = c(0.1, 0.01)))) +   # temporal prior: sigma=0.1
  f(st_id,      model = "iid",
    hyper = list(prec = list(
      prior = "pc.prec",
      param = c(0.5, 0.01))))     # interaction prior: sigma=0.5

result_nb <- inla(
  formula_nb,
  family  = "nbinomial",
  data    = model_data5,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic  = TRUE,
    waic = TRUE,
    cpo  = TRUE,
    po   = TRUE
  ),
  control.family = list(
    hyper = list(theta = list(
      prior = "loggamma",
      param = c(1, 0.00005)))
  )
)

cat("\n===== BASELINE MODEL FITTED =====\n")
summary(result_nb)

# ============================================================
#### ---- PRIOR SENSITIVITY TEST 1 ---- ####
# Stronger Spatial Prior
# Change: spatial sigma from 0.5 to 0.2
# Purpose: test sensitivity to spatial precision prior
# ============================================================

formula_nb_s1 <- update(formula_nb,
                        . ~ . -
                          f(region_id, model = "besag",
                            graph = "region_adj.graph",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.5, 0.01)))) +
                          f(region_id, model = "besag",
                            graph = "region_adj.graph",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.2, 0.01))))
)

result_nb_s1 <- inla(
  formula_nb_s1,
  family = "nbinomial",
  data   = model_data5,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic  = TRUE,
    waic = TRUE,
    cpo  = TRUE,
    po   = TRUE
  ),
  control.family = list(
    hyper = list(theta = list(
      prior = "loggamma",
      param = c(1, 0.00005)))
  )
)

cat("\n===== PRIOR SENSITIVITY TEST 1 FITTED =====\n")
cat("Stronger Spatial Prior (sigma = 0.2)\n")

# ============================================================
#### ---- PRIOR SENSITIVITY TEST 2 ---- ####
# Alternative Temporal Prior
# Change: temporal sigma from 0.1 to 0.5
# Purpose: test sensitivity to temporal precision prior
# ============================================================

formula_nb_s2 <- update(formula_nb,
                        . ~ . -
                          f(time_id, model = "rw1",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.1, 0.01)))) +
                          f(time_id, model = "rw1",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.5, 0.01))))
)

result_nb_s2 <- inla(
  formula_nb_s2,
  family = "nbinomial",
  data   = model_data5,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic  = TRUE,
    waic = TRUE,
    cpo  = TRUE,
    po   = TRUE
  ),
  control.family = list(
    hyper = list(theta = list(
      prior = "loggamma",
      param = c(1, 0.00005)))
  )
)

cat("\n===== PRIOR SENSITIVITY TEST 2 FITTED =====\n")
cat("Alternative Temporal Prior (sigma = 0.5)\n")

# ============================================================
#### ---- PRIOR SENSITIVITY TEST 3 ---- ####
# Alternative Interaction Prior
# Change: interaction sigma from 0.5 to 0.2
# Purpose: test sensitivity to interaction precision prior
# ============================================================

formula_nb_s3 <- update(formula_nb,
                        . ~ . -
                          f(st_id, model = "iid",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.5, 0.01)))) +
                          f(st_id, model = "iid",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.2, 0.01))))
)

result_nb_s3 <- inla(
  formula_nb_s3,
  family = "nbinomial",
  data   = model_data5,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic  = TRUE,
    waic = TRUE,
    cpo  = TRUE,
    po   = TRUE
  ),
  control.family = list(
    hyper = list(theta = list(
      prior = "loggamma",
      param = c(1, 0.00005)))
  )
)

cat("\n===== PRIOR SENSITIVITY TEST 3 FITTED =====\n")
cat("Alternative Interaction Prior (sigma = 0.2)\n")

# ============================================================
#### ---- STRUCTURAL TEST 1 ---- ####
# RW2 Temporal Trend
# Change: rw1 to rw2
# Purpose: test whether smoother temporal trend improves fit
# ============================================================

formula_nb_s4 <- update(formula_nb,
                        . ~ . -
                          f(time_id, model = "rw1",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.1, 0.01)))) +
                          f(time_id, model = "rw2",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.1, 0.01))))
)

result_nb_s4 <- inla(
  formula_nb_s4,
  family = "nbinomial",
  data   = model_data5,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic  = TRUE,
    waic = TRUE,
    cpo  = TRUE,
    po   = TRUE
  ),
  control.family = list(
    hyper = list(theta = list(
      prior = "loggamma",
      param = c(1, 0.00005)))
  )
)

cat("\n===== STRUCTURAL TEST 1 FITTED =====\n")
cat("RW2 Temporal Trend\n")

# ============================================================
#### ---- STRUCTURAL TEST 2 ---- ####
# No Spatio-Temporal Interaction
# Change: remove st_id term entirely
# Purpose: quantify contribution of interaction term
# ============================================================

formula_nb_s5 <- update(formula_nb,
                        . ~ . -
                          f(st_id, model = "iid",
                            hyper = list(prec = list(
                              prior = "pc.prec",
                              param = c(0.5, 0.01))))
)

result_nb_s5 <- inla(
  formula_nb_s5,
  family = "nbinomial",
  data   = model_data5,
  control.predictor = list(compute = TRUE),
  control.compute   = list(
    dic  = TRUE,
    waic = TRUE,
    cpo  = TRUE,
    po   = TRUE
  ),
  control.family = list(
    hyper = list(theta = list(
      prior = "loggamma",
      param = c(1, 0.00005)))
  )
)

cat("\n===== STRUCTURAL TEST 2 FITTED =====\n")
cat("No Spatio-Temporal Interaction\n")

# ============================================================
#### ---- FIX FAILED CPO VALUES ---- ####
# ============================================================

fix_cpo <- function(result, name) {
  n_failed <- sum(result$cpo$failure > 0,
                  na.rm = TRUE)
  if (n_failed > 0) {
    cat("Fixing", n_failed,
        "failed CPO values for:", name, "\n")
    result <- inla.cpo(result)
  }
  return(result)
}

result_nb    <- fix_cpo(result_nb,
                        "Baseline")
result_nb_s1 <- fix_cpo(result_nb_s1,
                        "Stronger Spatial Prior")
result_nb_s2 <- fix_cpo(result_nb_s2,
                        "Alternative Temporal Prior")
result_nb_s3 <- fix_cpo(result_nb_s3,
                        "Alternative Interaction Prior")
result_nb_s4 <- fix_cpo(result_nb_s4,
                        "RW2 Temporal Trend")
result_nb_s5 <- fix_cpo(result_nb_s5,
                        "No Spatio-Temporal Interaction")

# ============================================================
#### ---- MODEL COMPARISON TABLE ---- ####
# ============================================================

model_performance <- data.frame(
  Model = c(
    "Baseline",
    "Stronger Spatial Prior (sigma=0.2)",
    "Alternative Temporal Prior (sigma=0.5)",
    "Alternative Interaction Prior (sigma=0.2)",
    "RW2 Temporal Trend",
    "No Spatio-Temporal Interaction"
  ),
  Type = c(
    "Baseline",
    "Prior Sensitivity",
    "Prior Sensitivity",
    "Prior Sensitivity",
    "Structural",
    "Structural"
  ),
  WAIC = round(c(
    result_nb$waic$waic,
    result_nb_s1$waic$waic,
    result_nb_s2$waic$waic,
    result_nb_s3$waic$waic,
    result_nb_s4$waic$waic,
    result_nb_s5$waic$waic
  ), 2),
  DIC = round(c(
    result_nb$dic$dic,
    result_nb_s1$dic$dic,
    result_nb_s2$dic$dic,
    result_nb_s3$dic$dic,
    result_nb_s4$dic$dic,
    result_nb_s5$dic$dic
  ), 2),
  Log_CPO = round(c(
    sum(log(result_nb$cpo$cpo),    na.rm = TRUE),
    sum(log(result_nb_s1$cpo$cpo), na.rm = TRUE),
    sum(log(result_nb_s2$cpo$cpo), na.rm = TRUE),
    sum(log(result_nb_s3$cpo$cpo), na.rm = TRUE),
    sum(log(result_nb_s4$cpo$cpo), na.rm = TRUE),
    sum(log(result_nb_s5$cpo$cpo), na.rm = TRUE)
  ), 2)
)

cat("\n--- Prior Sensitivity and Model Specification ---\n")
print(model_performance)

# ============================================================
#### ---- CPO VALIDATION FOR BASELINE ---- ####
# ============================================================

cpo_vals <- result_nb$cpo$cpo
pit_vals <- result_nb$cpo$pit




cat("\n--- Baseline CPO Summary ---\n")
cat("Total observations:  ",
    length(cpo_vals),                              "\n")
cat("Mean CPO:            ",
    formatC(mean(cpo_vals, na.rm = TRUE),
            format = "e", digits = 4),             "\n")
cat("Log CPO Score:       ",
    round(sum(log(cpo_vals), na.rm = TRUE), 2),    "\n")
cat("Min CPO:             ",
    formatC(min(cpo_vals, na.rm = TRUE),
            format = "e", digits = 4),             "\n")
cat("Max CPO:             ",
    formatC(max(cpo_vals, na.rm = TRUE),
            format = "e", digits = 4),             "\n")
cat("Problematic CPO (<1e-10): ",
    sum(cpo_vals < 1e-10, na.rm = TRUE),           "\n")



# CPO by region
cpo_by_region <- data.frame(
  region_id = model_data5$region_id,
  year      = model_data5$year,
  cpo       = cpo_vals,
  pit       = pit_vals
) %>%
  group_by(region_id) %>%
  summarise(
    mean_cpo = round(mean(cpo, na.rm = TRUE), 6),
    log_cpo  = round(sum(log(cpo),
                         na.rm = TRUE), 2),
    mean_pit = round(mean(pit, na.rm = TRUE), 3),
    n_obs    = n(),
    .groups  = "drop"
  ) %>%
  arrange(log_cpo)

cat("\n--- CPO by Region (worst first) ---\n")
print(cpo_by_region)

# Get region names mapping
region_names <- as.data.frame(model_data5) %>%
  dplyr::select(region_id, region) %>%
  distinct() %>%
  arrange(region_id)

print(region_names)

# Merge region names with CPO results
cpo_by_region_named <- cpo_by_region %>%
  left_join(region_names, by = "region_id") %>%
  dplyr::select(region_id, region, mean_cpo, 
                log_cpo, mean_pit, n_obs) %>%
  arrange(log_cpo)

cat("\n--- CPO by Region with Names ---\n")
print(cpo_by_region_named)

# CPO by year
cpo_by_year <- data.frame(
  year = model_data5$year,
  cpo  = cpo_vals,
  pit  = pit_vals
) %>%
  group_by(year) %>%
  summarise(
    mean_cpo = round(mean(cpo, na.rm = TRUE), 6),
    log_cpo  = round(sum(log(cpo),
                         na.rm = TRUE), 2),
    mean_pit = round(mean(pit, na.rm = TRUE), 3),
    n_obs    = n(),
    .groups  = "drop"
  )

cat("\n--- CPO by Year ---\n")
print(cpo_by_year)

# ============================================================
#### ---- PIT HISTOGRAM ---- ####
# ============================================================

pit_df <- data.frame(pit = pit_vals)

pit_plot <- ggplot(pit_df, aes(x = pit)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins  = 20,
    fill  = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = 1,
    color      = "red",
    linetype   = "dashed",
    linewidth  = 0.8
  ) +
  annotate(
    "text",
    x     = 0.75,
    y     = 1.15,
    label = "Expected uniform",
    color = "red",
    size  = 3.5
  ) +
  labs(
    title    = "PIT Histogram: Model Calibration",
    subtitle = "Uniform distribution indicates good calibration",
    x        = "Probability Integral Transform (PIT)",
    y        = "Density"
  ) +
  theme_classic() +
  theme(
    plot.title    = element_text(hjust = 0.5,
                                 face = "bold"),
    plot.subtitle = element_text(hjust = 0.5,
                                 size = 9)
  )

ggsave(
  filename = "pit_histogram.png",
  plot     = pit_plot,
  width    = 8,
  height   = 6,
  dpi      = 300
)

print(pit_plot)


ks_test <- ks.test(pit_vals, "punif", 0, 1)
cat("KS Statistic:", round(ks_test$statistic, 4), "\n")
cat("p-value:     ", round(ks_test$p.value,   4), "\n")

# ============================================================
#### ---- COEFFICIENT STABILITY PLOT ---- ####
# ============================================================

all_fixed <- bind_rows(
  get_fixed(result_nb,    "Baseline"),
  get_fixed(result_nb_s1, "Stronger Spatial Prior"),
  get_fixed(result_nb_s2, "Alternative Temporal Prior"),
  get_fixed(result_nb_s3, "Alternative Interaction Prior")
) %>%
  filter(Variable != "(Intercept)")

# ---- Rename variables to thesis names ----
variable_labels <- c(
  "precip_lag0"           = "Rainfall L0",
  "precip_lag1"           = "Rainfall L1",
  "precip_lag2"           = "Rainfall L2",
  "precip_lag3"           = "Rainfall L3",
  "temp_lag0"             = "Temperature L0",
  "humidity_lag1"         = "Humidity L1",
  "dew_lag2"              = "Dew Point L2",
  "dew_lag3"              = "Dew Point L3",
  "solarenergy_lag0"      = "Solar Energy L0",
  "uvindex_lag1"          = "UV Index L1",
  "sealevelpressure_lag0" = "Sea Level Pressure L0",
  "seasonMajor Rainy"     = "Major Rainy Season",
  "seasonMinor Rainy"     = "Minor Rainy Season"
)

# Apply labels
all_fixed_renamed <- all_fixed %>%
  mutate(
    Variable = dplyr::recode(Variable,
                             !!!variable_labels)
  )

# Set variable order bottom to top
variable_order <- c(
  "Rainfall L0",
  "Rainfall L1",
  "Rainfall L2",
  "Rainfall L3",
  "Temperature L0",
  "Humidity L1",
  "Dew Point L2",
  "Dew Point L3",
  "Solar Energy L0",
  "UV Index L1",
  "Sea Level Pressure L0",
  "Major Rainy Season",
  "Minor Rainy Season"
)

all_fixed_renamed$Variable <- factor(
  all_fixed_renamed$Variable,
  levels = rev(variable_order)
)

# ---- Build stability plot ----
stability_plot <- ggplot(
  all_fixed_renamed,
  aes(x     = Mean,
      y     = Variable,
      color = Model,
      shape = Model)
) +
  geom_point(
    position  = position_dodge(width = 0.6),
    size      = 2.5
  ) +
  geom_errorbar(
    aes(xmin = Lower, xmax = Upper),
    position    = position_dodge(width = 0.6),
    width       = 0.3,
    linewidth   = 0.6,
    orientation = "y"
  ) +
  geom_vline(
    xintercept = 0,
    color      = "black",
    linetype   = "dashed",
    linewidth  = 0.5
  ) +
  scale_color_manual(
    values = c(
      "Baseline"                      = "#2196F3",
      "Stronger Spatial Prior"        = "#E91E63",
      "Alternative Temporal Prior"    = "#4CAF50",
      "Alternative Interaction Prior" = "#FF9800"
    )
  ) +
  scale_shape_manual(
    values = c(
      "Baseline"                      = 16,
      "Stronger Spatial Prior"        = 17,
      "Alternative Temporal Prior"    = 15,
      "Alternative Interaction Prior" = 18
    )
  ) +
  labs(
    title    = "Coefficient Stability Across Prior Specifications",
    subtitle = "Posterior means and 95% credible intervals",
    x        = "Posterior Mean",
    y        = NULL,
    color    = "Model",
    shape    = "Model"
  ) +
  theme_classic() +
  theme(
    plot.title      = element_text(hjust = 0.5,
                                   face = "bold",
                                   size = 12),
    plot.subtitle   = element_text(hjust = 0.5,
                                   size = 9),
    axis.text.y     = element_text(size = 10),
    axis.text.x     = element_text(size = 9),
    axis.title.x    = element_text(size = 10),
    legend.position = "right",
    legend.title    = element_text(size = 9),
    legend.text     = element_text(size = 8)
  )

ggsave(
  filename = "coefficient_stability.png",
  plot     = stability_plot,
  width    = 12,
  height   = 7,
  dpi      = 300
)

print(stability_plot)


# ============================================================
#### ---- FIXED EFFECTS AND HYPERPARAMETERS ---- ####
# ============================================================

fixed_effects    <- result_nb$summary.fixed
fixed_effects_df <- data.frame(
  Variable = rownames(fixed_effects),
  Mean     = round(fixed_effects$mean,         4),
  SD       = round(fixed_effects$sd,           4),
  Lower    = round(fixed_effects$`0.025quant`, 4),
  Upper    = round(fixed_effects$`0.975quant`, 4)
)

cat("\n--- Fixed Effects ---\n")
print(fixed_effects_df)

hyper <- result_nb$summary.hyperpar

hyper_table <- data.frame(
  Parameter = rownames(hyper),
  Mean      = round(hyper$mean,         2),
  Lower     = round(hyper$`0.025quant`, 2),
  Upper     = round(hyper$`0.975quant`, 2)
)

cat("\n--- Hyperparameters ---\n")
print(hyper_table)







# ============================================================
#### ---- RISK MAPPING ---- ####
# ============================================================

model_data6 <- model_data5 %>%
  mutate(
    fitted_mean  = result_nb$summary.fitted.values$mean,
    fitted_lower = result_nb$summary.fitted.values$`0.025quant`,
    fitted_upper = result_nb$summary.fitted.values$`0.975quant`,
    RR           = fitted_mean  / expected,
    RR_low       = fitted_lower / expected,
    RR_high      = fitted_upper / expected
  )

# Yearly RR
rr_yearly <- model_data6 %>%
  group_by(region, year) %>%
  summarise(
    RR      = mean(RR,      na.rm = TRUE),
    RR_low  = mean(RR_low,  na.rm = TRUE),
    RR_high = mean(RR_high, na.rm = TRUE),
    .groups = "drop"
  )

# Extract geometry as plain dataframe
regions_geom <- data.frame(
  Region   = regions$Region,
  geometry = regions$geometry
)

# Create sf from geometry dataframe
regions_geom <- st_as_sf(regions_geom)

# Function to create yearly RR sf object
make_rr_sf <- function(data, yr) {
  data %>%
    filter(year == yr) %>%
    as.data.frame() %>%
    left_join(
      regions_geom,
      by = c("region" = "Region")
    ) %>%
    st_as_sf()
}

# Create yearly datasets
rr_2020 <- make_rr_sf(rr_yearly, 2020)
rr_2021 <- make_rr_sf(rr_yearly, 2021)
rr_2022 <- make_rr_sf(rr_yearly, 2022)
rr_2023 <- make_rr_sf(rr_yearly, 2023)
rr_2024 <- make_rr_sf(rr_yearly, 2024)

# Apply RR categories
add_rr_cat <- function(df) {
  df$RR <- as.numeric(df$RR)
  df$RR[df$RR < 0] <- 0
  df$RR[df$RR > 2] <- 2
  df$RR_cat <- cut(
    df$RR,
    breaks         = custom_breaks,
    include.lowest = TRUE,
    labels         = custom_labels,
    right          = TRUE
  )
  return(df)
}

rr_2020 <- add_rr_cat(rr_2020)
rr_2021 <- add_rr_cat(rr_2021)
rr_2022 <- add_rr_cat(rr_2022)
rr_2023 <- add_rr_cat(rr_2023)
rr_2024 <- add_rr_cat(rr_2024)



# RR plotting function
plot_rr <- function(df, year) {
  ggplot(df) +
    geom_sf(aes(fill = RR_cat),
            color = "grey30", size = 0.2) +
    scale_fill_manual(
      values   = colors,
      name     = paste0("RR-", year),
      na.value = "white"
    ) +
    coord_sf(clip = "off") +
    theme_minimal() +
    theme(
      legend.position      = c(0.80, 1.00),
      legend.justification = c("left", "top"),
      legend.key.size      = unit(0.22, "cm"),
      legend.title         = element_text(size = 7),
      legend.text          = element_text(size = 6),
      axis.text            = element_blank(),
      axis.ticks           = element_blank(),
      panel.grid           = element_blank(),
      plot.margin          = unit(
        c(0.2, 0.2, 0.2, 0.2), "cm"
      )
    )
}

# Generate RR maps
p20 <- plot_rr(rr_2020, "2020")
p21 <- plot_rr(rr_2021, "2021")
p22 <- plot_rr(rr_2022, "2022")
p23 <- plot_rr(rr_2023, "2023")
p24 <- plot_rr(rr_2024, "2024")

# Arrange layout
rr_top_row    <- p20 | p21 | p22
rr_bottom_row <- p23 | p24

rr_final_plot <- rr_top_row / rr_bottom_row +
  plot_layout(heights = c(1, 1),
              widths  = c(1, 1, 1))

print(rr_final_plot)

ggsave(
  filename = "rr_maps_2020_2024.png",
  plot     = rr_final_plot,
  width    = 14,
  height   = 12,
  dpi      = 300
)

# Get exact RR values by region and year
rr_summary <- rr_yearly %>%
  as.data.frame() %>%
  dplyr::select(region, year, RR, RR_low, RR_high) %>%
  mutate(
    RR      = round(RR,      3),
    RR_low  = round(RR_low,  3),
    RR_high = round(RR_high, 3)
  ) %>%
  arrange(year, desc(RR))

print(rr_summary)


# ============================================================
#### ---- EXCEEDANCE PROBABILITY MAPS ---- ####
# ============================================================

# Split exceedance by year using regions_geom
make_ep_sf <- function(data, yr) {
  data %>%
    filter(year == yr) %>%
    as.data.frame() %>%
    left_join(
      regions_geom,
      by = c("region" = "Region")
    ) %>%
    st_as_sf()
}

ep_2020 <- make_ep_sf(exceed_yearly, 2020)
ep_2021 <- make_ep_sf(exceed_yearly, 2021)
ep_2022 <- make_ep_sf(exceed_yearly, 2022)
ep_2023 <- make_ep_sf(exceed_yearly, 2023)
ep_2024 <- make_ep_sf(exceed_yearly, 2024)

cat("Exceedance datasets created successfully\n")
cat("ep_2020 rows:", nrow(ep_2020), "\n")
cat("ep_2021 rows:", nrow(ep_2021), "\n")
cat("ep_2022 rows:", nrow(ep_2022), "\n")
cat("ep_2023 rows:", nrow(ep_2023), "\n")
cat("ep_2024 rows:", nrow(ep_2024), "\n")

# Exceedance plotting function
plot_ep <- function(df, year) {
  ggplot(df) +
    geom_sf(aes(fill = mean_exceed_prob),
            color = "grey30", size = 0.2) +
    scale_fill_gradient(
      low    = "lightyellow",
      high   = "darkred",
      name   = paste0("P(RR>1.5)\n", year),
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.50, 0.75, 1.00),
      labels = c("0", "0.25", "0.50",
                 "0.75", "1.00")
    ) +
    coord_sf(clip = "off") +
    theme_minimal() +
    theme(
      legend.position      = c(0.80, 1.00),
      legend.justification = c("left", "top"),
      legend.key.size      = unit(0.22, "cm"),
      legend.title         = element_text(size = 7),
      legend.text          = element_text(size = 6),
      axis.text            = element_blank(),
      axis.ticks           = element_blank(),
      panel.grid           = element_blank(),
      plot.margin          = unit(
        c(0.2, 0.2, 0.2, 0.2), "cm"
      )
    )
}

# Generate exceedance maps
ep20 <- plot_ep(ep_2020, "2020")
ep21 <- plot_ep(ep_2021, "2021")
ep22 <- plot_ep(ep_2022, "2022")
ep23 <- plot_ep(ep_2023, "2023")
ep24 <- plot_ep(ep_2024, "2024")

# Arrange layout
ep_top_row    <- ep20 | ep21 | ep22
ep_bottom_row <- ep23 | ep24

ep_final_plot <- ep_top_row / ep_bottom_row +
  plot_layout(heights = c(1, 1),
              widths  = c(1, 1, 1)) +
  plot_annotation(
    title    = "Exceedance Probability: P(RR > 1.5)",
    subtitle = "2020-2024",
    theme = theme(
      plot.title    = element_text(
        hjust = 0.5,
        face  = "bold",
        size  = 13
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size  = 9
      )
    )
  )

print(ep_final_plot)

ggsave(
  filename = "exceedance_maps_2020_2024.png",
  plot     = ep_final_plot,
  width    = 14,
  height   = 12,
  dpi      = 300
)

# Get exceedance probability by region and year
ep_summary <- exceed_yearly %>%
  as.data.frame() %>%
  dplyr::select(region, year, 
                mean_exceed_prob, mean_RR) %>%
  mutate(
    mean_exceed_prob = round(mean_exceed_prob, 3),
    mean_RR          = round(mean_RR, 3)
  ) %>%
  arrange(year, desc(mean_exceed_prob))

print(ep_summary)


# ============================================================
#### ---- HOTSPOT DETECTION (LISA) ---- ####
# ============================================================

# LISA colors
lisa_colors <- c(
  "H-H"  = "red",
  "H-L"  = "skyblue",
  "L-H"  = "navy",
  "L-L"  = "orange",
  "None" = "white"
)

# LISA function to avoid repetitive code
run_lisa <- function(df, sig_level = 0.05) {
  
  # Use st_join correctly
  df <- st_join(
    df,
    regions %>% dplyr::select(Region),
    join = st_equals
  )
  
  nb_rr   <- poly2nb(df, queen = TRUE)
  lw_rr   <- nb2listw(nb_rr, style = "W",
                      zero.policy = TRUE)
  lisa_rr <- localmoran(df$RR, lw_rr,
                        zero.policy = TRUE)
  
  df$lisa_I   <- lisa_rr[, "Ii"]
  df$lisa_p   <- lisa_rr[, "Pr(z != E(Ii))"]
  df$RR_z     <- scale(df$RR)[, 1]
  df$lag_RR   <- lag.listw(lw_rr, df$RR,
                           zero.policy = TRUE)
  df$lag_RR_z <- scale(df$lag_RR)[, 1]
  
  df$lisa_cluster <- "None"
  sig <- df$lisa_p <= sig_level
  
  df$lisa_cluster[sig & df$RR_z > 0 &
                    df$lag_RR_z > 0] <- "H-H"
  df$lisa_cluster[sig & df$RR_z < 0 &
                    df$lag_RR_z < 0] <- "L-L"
  df$lisa_cluster[sig & df$RR_z > 0 &
                    df$lag_RR_z < 0] <- "H-L"
  df$lisa_cluster[sig & df$RR_z < 0 &
                    df$lag_RR_z > 0] <- "L-H"
  return(df)
}

# Run LISA for each year
rr_2020 <- run_lisa(rr_2020)
rr_2021 <- run_lisa(rr_2021)
rr_2022 <- run_lisa(rr_2022)
rr_2023 <- run_lisa(rr_2023)
rr_2024 <- run_lisa(rr_2024)

# LISA plotting function
plot_lisa <- function(df, year) {
  ggplot(df) +
    geom_sf(aes(fill = lisa_cluster),
            color = "grey30", size = 0.2) +
    scale_fill_manual(
      values   = lisa_colors,
      na.value = "white",
      name     = paste0("LISA-", year)
    ) +
    coord_sf(clip = "off") +
    theme_minimal() +
    theme(
      legend.position      = c(0.8, 1),
      legend.justification = c("left", "top"),
      legend.key.size      = unit(0.25, "cm"),
      legend.title         = element_text(size = 7),
      legend.text          = element_text(size = 6),
      axis.text            = element_blank(),
      axis.ticks           = element_blank(),
      panel.grid           = element_blank(),
      plot.margin          = unit(
        c(0.2, 0.2, 0.2, 0.2), "cm"
      )
    )
}

# Generate LISA maps
l20 <- plot_lisa(rr_2020, "2020")
l21 <- plot_lisa(rr_2021, "2021")
l22 <- plot_lisa(rr_2022, "2022")
l23 <- plot_lisa(rr_2023, "2023")
l24 <- plot_lisa(rr_2024, "2024")

# Arrange layout
lisa_top_row    <- l20 | l21 | l22
lisa_bottom_row <- l23 | l24

lisa_final_plot <- lisa_top_row / lisa_bottom_row +
  plot_layout(heights = c(1, 1),
              widths  = c(1, 1, 1))

print(lisa_final_plot)

ggsave(
  filename = "lisa_maps_2020_2024.png",
  plot     = lisa_final_plot,
  width    = 14,
  height   = 12,
  dpi      = 300
)



lisa_summary <- bind_rows(
  rr_2020 %>% as.data.frame() %>%
    dplyr::select(region, lisa_cluster, RR) %>%
    mutate(year = 2020),
  rr_2021 %>% as.data.frame() %>%
    dplyr::select(region, lisa_cluster, RR) %>%
    mutate(year = 2021),
  rr_2022 %>% as.data.frame() %>%
    dplyr::select(region, lisa_cluster, RR) %>%
    mutate(year = 2022),
  rr_2023 %>% as.data.frame() %>%
    dplyr::select(region, lisa_cluster, RR) %>%
    mutate(year = 2023),
  rr_2024 %>% as.data.frame() %>%
    dplyr::select(region, lisa_cluster, RR) %>%
    mutate(year = 2024)
) %>%
  filter(lisa_cluster != "None") %>%
  arrange(year, lisa_cluster)

print(lisa_summary)

# ============================================================
#### ---- SPATIO-TEMPORAL MODEL DIAGNOSTICS ---- ####
# ============================================================

#### ---- DIAGNOSTIC 1: FITTED VS OBSERVED ---- ####

fitted_vals <- result_nb$summary.fitted.values$mean
actual_vals <- model_data5$uncomplicated_malaria_tested_positive

fit_obs_df <- data.frame(
  actual = actual_vals,
  fitted = fitted_vals,
  year   = model_data5$year
)

fit_cor <- cor(actual_vals, fitted_vals)
cat("\nFitted vs Observed Correlation:",
    round(fit_cor, 4), "\n")

p1 <- ggplot(
  fit_obs_df,
  aes(x = actual, y = fitted)
) +
  geom_point(
    aes(color = factor(year)),
    alpha = 0.5,
    size  = 1.5
  ) +
  geom_abline(
    intercept = 0,
    slope     = 1,
    color     = "red",
    linetype  = "dashed",
    linewidth = 0.8
  ) +
  geom_smooth(
    method    = "lm",
    se        = TRUE,
    color     = "blue",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x     = max(actual_vals) * 0.25,
    y     = max(fitted_vals) * 0.90,
    label = paste0("r = ", round(fit_cor, 3)),
    size  = 4,
    color = "black"
  ) +
  labs(
    title = "(a) Fitted versus Observed",
    x     = "Observed Cases",
    y     = "Fitted Cases",
    color = "Year"
  ) +
  theme_classic() +
  theme(
    plot.title      = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 10
    ),
    legend.position = "right",
    legend.title    = element_text(size = 8),
    legend.text     = element_text(size = 7)
  )

# ============================================================
#### ---- DIAGNOSTIC 2: TEMPORAL RANDOM EFFECT ---- ####
# ============================================================

temporal_re <- result_nb$summary.random$time_id

temporal_df <- data.frame(
  time_id = temporal_re$ID,
  mean    = temporal_re$mean,
  lower   = temporal_re$`0.025quant`,
  upper   = temporal_re$`0.975quant`,
  year    = min(model_data5$year) +
    (temporal_re$ID - 1)
)



p2 <- ggplot(
  temporal_df,
  aes(x = year)
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill  = "steelblue",
    alpha = 0.3
  ) +
  geom_line(
    aes(y = mean),
    color     = "steelblue",
    linewidth = 1
  ) +
  geom_point(
    aes(y = mean),
    color = "steelblue",
    size  = 3
  ) +
  geom_hline(
    yintercept = 0,
    color      = "red",
    linetype   = "dashed",
    linewidth  = 0.8
  ) +
  scale_x_continuous(
    breaks = min(model_data5$year):
      max(model_data5$year)
  ) +
  labs(
    title = "(b) Posterior Temporal Random Effect",
    x     = "Year",
    y     = "log scale"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 10
    )
  )

# ============================================================
#### ---- DIAGNOSTIC 3: SPATIAL RANDOM EFFECT ---- ####
# ============================================================

spatial_re <- result_nb$summary.random$region_id

spatial_df <- data.frame(
  region_id = spatial_re$ID,
  mean      = spatial_re$mean,
  lower     = spatial_re$`0.025quant`,
  upper     = spatial_re$`0.975quant`
)


# Merge with region names and geometry
spatial_map <- spatial_df %>%
  left_join(
    as.data.frame(model_data5) %>%
      dplyr::select(region_id, region) %>%
      distinct(),
    by = "region_id"
  ) %>%
  left_join(
    regions,
    by = c("region" = "Region")
  ) %>%
  st_as_sf()

p3 <- ggplot(spatial_map) +
  geom_sf(
    aes(fill = mean),
    color = "grey30",
    size  = 0.2
  ) +
  scale_fill_distiller(
    palette   = "RdYlBu",
    direction = -1,
    name      = "Spatial RE\n(log scale)"
  ) +
  labs(
    title = "(c) Posterior Spatial Random Effect"
  ) +
  theme_minimal() +
  theme(
    plot.title           = element_text(
      hjust = 0.5,
      face  = "bold",
      size  = 11
    ),
    legend.position      = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.background    = element_rect(
      fill      = "white",
      colour    = "grey80",
      linewidth = 0.3
    ),
    legend.key.size  = unit(1.2, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 6),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid       = element_blank()
  )
# ============================================================
#### ---- COMBINE ALL THREE WITH PATCHWORK ---- ####
# ============================================================

diagnostic_plot <- (p1 | p2) / p3 +
  plot_layout(
    heights = c(1, 1.2)
  ) +
  plot_annotation(
    title    = "Spatio-Temporal Model Diagnostics",
    subtitle = paste0(
      "Fitted vs observed, temporal random ",
      "effect and spatial random effect"
    ),
    theme = theme(
      plot.title    = element_text(
        hjust = 0.5,
        face  = "bold",
        size  = 10
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size  = 9
      )
    )
  )

print(diagnostic_plot)

ggsave(
  filename = "spatio_temporal_diagnostics.png",
  plot     = diagnostic_plot,
  width    = 14,
  height   = 20,
  dpi      = 300
)

