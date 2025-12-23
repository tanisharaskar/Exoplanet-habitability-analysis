library(readr)
ex= (Exoplanets)
str(ex)
nrow(ex)
ncol(ex)
# Replace column names with the first row values
colnames(ex) <- as.character(unlist(ex[1, ]))

# Remove that first row from the data
ex <- ex[-1, ]

# Check the result
names(ex)[1:10]
View(ex)
keep <- c(
  # Identification
  "planet_name", "host_star_name", "planet_letter",
  
  # System composition
  "num_stars_in_system", "num_planets_in_system", "is_circumbinary",
  
  # Discovery info
  "discovery_method", "detected_by_transit", "detected_by_radial_velocity", "is_controversial",
  
  # Orbital parameters
  "orbital_period_days", "orbit_semi_major_axis_au", "eccentricity", "inclination_deg",
  
  # Energy and temperature (habitability zone indicators)
  "insolation_flux_earth_flux", "equilibrium_temperature_k",
  
  # Planetary radius (Earth + Jupiter units, with uncertainty and flags)
  "planet_radius_earth_radius", "planet_radius_upper_unc_earth_radius", "planet_radius_lower_unc_earth_radius", 
  "planet_radius_earth_limit_flag", "planet_radius_jupiter_radius", "planet_radius_upper_unc_jupiter_radius", 
  "planet_radius_lower_unc_jupiter_radius", "planet_radius_jupiter_limit_flag",
  
  # Planetary mass (Earth + Jupiter units, with uncertainty and flags)
  "planet_mass_earth_mass", "planet_mass_upper_unc_earth_mass", "planet_mass_lower_unc_earth_mass",
  "planet_mass_earth_limit_flag", "planet_mass_jupiter_mass", "planet_mass_upper_unc_jupiter_mass",
  "planet_mass_lower_unc_jupiter_mass", "planet_mass_jupiter_limit_flag", "planet_mass_provenance",
  
  # Planetary density (g/cm3, with uncertainty and flag)
  "planet_density_gcm3", "planet_density_upper_unc_gcm3", "planet_density_lower_unc_gcm3", "planet_density_limit_flag",
  
  # Stellar characteristics (the host star defines the environment)
  "stellar_effective_temp_k", "stellar_radius_solar_radius", "stellar_mass_solar_mass",
  "stellar_luminosity_log_solar", "stellar_metallicity_dex", "stellar_age_gyr", "stellar_density_gcm3",
  
  # Distance and location
  "distance_pc", "ra_deg", "dec_deg"
)


exodf <- ex[, intersect(keep, names(ex)), drop = FALSE]
str(exodf)
head(exodf)
View(exodf)
nrow(exodf)
ncol(exodf)
# Replace text-based NULLs and blanks with NA
exodf[] <- lapply(exodf, function(x) {
  x[x %in% c("NULL", "null", "")] <- NA
  x
})
summary(exodf)

# Count of NAs per column
na_count <- colSums(is.na(exodf))
# Percentage of NAs per column
na_percent <- round((na_count / nrow(exodf)) * 100, 2)
# Combine into a tidy dataframe
na_summary <- data.frame(
  Variable = names(exodf),
  Missing_Count = na_count,
  Missing_Percent = na_percent
)
# missing %
na_summary <- na_summary[order(-na_summary$Missing_Percent),]
head(na_summary,47 )

# Identify numeric-like variables
num_vars <- c(
  "num_stars_in_system", "num_planets_in_system", "is_circumbinary",
  "detected_by_transit", "detected_by_radial_velocity", "is_controversial",
  "orbital_period_days", "orbit_semi_major_axis_au", "eccentricity",
  "inclination_deg", "insolation_flux_earth_flux", "equilibrium_temperature_k",
  "planet_radius_earth_radius", "planet_radius_upper_unc_earth_radius",
  "planet_radius_lower_unc_earth_radius", "planet_radius_earth_limit_flag",
  "planet_radius_jupiter_radius", "planet_radius_upper_unc_jupiter_radius",
  "planet_radius_lower_unc_jupiter_radius", "planet_radius_jupiter_limit_flag",
  "planet_mass_earth_mass", "planet_mass_upper_unc_earth_mass",
  "planet_mass_lower_unc_earth_mass", "planet_mass_earth_limit_flag",
  "planet_mass_jupiter_mass", "planet_mass_upper_unc_jupiter_mass",
  "planet_mass_lower_unc_jupiter_mass", "planet_mass_jupiter_limit_flag",
  "planet_density_gcm3", "planet_density_upper_unc_gcm3",
  "planet_density_lower_unc_gcm3", "planet_density_limit_flag",
  "stellar_effective_temp_k", "stellar_radius_solar_radius",
  "stellar_mass_solar_mass", "stellar_luminosity_log_solar",
  "stellar_metallicity_dex", "stellar_age_gyr", "stellar_density_gcm3",
  "distance_pc", "ra_deg", "dec_deg"
)

# Convert safely to numeric
exodf[num_vars] <- lapply(exodf[num_vars], as.numeric)

# Verify structure
str(exodf)

# Binary variables to encode
bin_vars <- c(
  "is_circumbinary",
  "detected_by_transit",
  "detected_by_radial_velocity",
  "is_controversial"
)

# Convert to factors with clear labels
exodf[bin_vars] <- lapply(exodf[bin_vars], function(x) {
  factor(x, levels = c(0, 1), labels = c("No", "Yes"))
})

# Check result
summary(exodf[bin_vars])

## -------------------------------
## Univariate Analysis
## -------------------------------

# Select numeric variables for univariate EDA
uni_vars <- c(
  "planet_mass_earth_mass",
  "planet_radius_earth_radius",
  "planet_density_gcm3",
  "orbital_period_days",
  "orbit_semi_major_axis_au",
  "eccentricity",
  "insolation_flux_earth_flux",
  "equilibrium_temperature_k",
  "stellar_effective_temp_k",
  "stellar_mass_solar_mass"
)

# Ensure numeric (safety check)
exodf[uni_vars] <- lapply(exodf[uni_vars], as.numeric)

# -------------------------------
# Summary statistics
# -------------------------------
summary(exodf[uni_vars])

# -------------------------------
# Histograms (original scale)
# -------------------------------
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))

hist(exodf$planet_mass_earth_mass,
     breaks = 40, main = "Planet Mass (Earth Masses)",
     xlab = "Earth Masses")

hist(exodf$planet_radius_earth_radius,
     breaks = 40, main = "Planet Radius (Earth Radii)",
     xlab = "Earth Radii")

hist(exodf$planet_density_gcm3,
     breaks = 40, main = "Planet Density (g/cm³)",
     xlab = "Density")

hist(exodf$orbital_period_days,
     breaks = 40, main = "Orbital Period (days)",
     xlab = "Days")

hist(exodf$orbit_semi_major_axis_au,
     breaks = 40, main = "Semi-Major Axis (AU)",
     xlab = "AU")

hist(exodf$eccentricity,
     breaks = 40, main = "Orbital Eccentricity",
     xlab = "Eccentricity")

hist(exodf$insolation_flux_earth_flux,
     breaks = 40, main = "Insolation Flux (Earth Units)",
     xlab = "Earth Flux")

hist(exodf$equilibrium_temperature_k,
     breaks = 40, main = "Equilibrium Temperature (K)",
     xlab = "Kelvin")

hist(exodf$stellar_effective_temp_k,
     breaks = 40, main = "Stellar Effective Temperature (K)",
     xlab = "Kelvin")

hist(exodf$stellar_mass_solar_mass,
     breaks = 40, main = "Stellar Mass (Solar Masses)",
     xlab = "Solar Masses")

par(mfrow = c(1, 1))

# -------------------------------
# Log-scale histograms (skewed vars)
# -------------------------------
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

hist(log10(exodf$planet_mass_earth_mass),
     breaks = 40, main = "Log Planet Mass",
     xlab = "log10(Earth Masses)")

hist(log10(exodf$orbital_period_days),
     breaks = 40, main = "Log Orbital Period",
     xlab = "log10(Days)")

hist(log10(exodf$insolation_flux_earth_flux),
     breaks = 40, main = "Log Insolation Flux",
     xlab = "log10(Earth Flux)")

hist(log10(exodf$orbit_semi_major_axis_au),
     breaks = 40, main = "Log Semi-Major Axis",
     xlab = "log10(AU)")

par(mfrow = c(1, 1))

## -------------------------------
## Bivariate Analysis
## -------------------------------

# Safety: ensure numeric
biv_vars <- c(
  "planet_mass_earth_mass",
  "planet_radius_earth_radius",
  "orbital_period_days",
  "orbit_semi_major_axis_au",
  "insolation_flux_earth_flux",
  "stellar_mass_solar_mass",
  "stellar_effective_temp_k"
)
exodf[biv_vars] <- lapply(exodf[biv_vars], as.numeric)

## -------------------------------
## 1. Planet Mass vs Planet Radius
## -------------------------------
plot(
  exodf$planet_mass_earth_mass,
  exodf$planet_radius_earth_radius,
  xlab = "Planet Mass (Earth Masses)",
  ylab = "Planet Radius (Earth Radii)",
  main = "Planet Mass vs Planet Radius",
  pch = 19, col = rgb(0, 0, 0, 0.4)
)

## -------------------------------
## 2. Planet Mass vs Orbital Period (log-scale)
## -------------------------------
plot(
  log10(exodf$orbital_period_days),
  log10(exodf$planet_mass_earth_mass),
  xlab = "log10 Orbital Period (days)",
  ylab = "log10 Planet Mass (Earth Masses)",
  main = "Planet Mass vs Orbital Period",
  pch = 19, col = rgb(0, 0, 0, 0.4)
)

## -------------------------------
## 3. Semi-Major Axis vs Insolation Flux
## -------------------------------
plot(
  log10(exodf$orbit_semi_major_axis_au),
  log10(exodf$insolation_flux_earth_flux),
  xlab = "log10 Semi-Major Axis (AU)",
  ylab = "log10 Insolation Flux (Earth Units)",
  main = "Orbital Distance vs Insolation",
  pch = 19, col = rgb(0, 0, 0, 0.4)
)

## -------------------------------
## 4. Planet Radius vs Insolation Flux
## -------------------------------
plot(
  log10(exodf$planet_radius_earth_radius),
  log10(exodf$insolation_flux_earth_flux),
  xlab = "log10 Planet Radius (Earth Radii)",
  ylab = "log10 Insolation Flux",
  main = "Planet Radius vs Insolation",
  pch = 19, col = rgb(0, 0, 0, 0.4)
)

## -------------------------------
## 5. Stellar Mass vs Planet Mass
## -------------------------------
plot(
  exodf$stellar_mass_solar_mass,
  exodf$planet_mass_earth_mass,
  xlab = "Stellar Mass (Solar Masses)",
  ylab = "Planet Mass (Earth Masses)",
  main = "Stellar Mass vs Planet Mass",
  pch = 19, col = rgb(0, 0, 0, 0.4)
)

## -------------------------------
## 6. Detection Method vs Orbital Period
## -------------------------------
boxplot(
  orbital_period_days ~ detected_by_transit,
  data = exodf,
  log = "y",
  xlab = "Detected by Transit",
  ylab = "Orbital Period (days)",
  main = "Orbital Period by Detection Method"
)

## -------------------------------
## 7. Detection Method vs Planet Mass
## -------------------------------
boxplot(
  planet_mass_earth_mass ~ detected_by_radial_velocity,
  data = exodf,
  log = "y",
  xlab = "Detected by Radial Velocity",
  ylab = "Planet Mass (Earth Masses)",
  main = "Planet Mass by Detection Method"
)

##----------------------------
## Planets closer 
##---------------------------
# Ensure numeric
exodf$orbit_semi_major_axis_au <- as.numeric(exodf$orbit_semi_major_axis_au)
# Sort planets closest to their host star
closest_to_star <- exodf[order(exodf$orbit_semi_major_axis_au), ]

head(closest_to_star[, c("planet_name", "host_star_name", "orbit_semi_major_axis_au")])

# Ensure numeric
exodf$distance_pc <- as.numeric(exodf$distance_pc)

# Sort planets closest to Earth
closest_to_earth <- exodf[order(exodf$distance_pc), ]

head(closest_to_earth[, c("planet_name", "host_star_name", "distance_pc")])

##-------------------------
## Correlation Analysis
##------------------------
# Select numeric variables
corr_vars <- c(
  "planet_mass_earth_mass",
  "planet_radius_earth_radius",
  "planet_density_gcm3",
  "orbital_period_days",
  "orbit_semi_major_axis_au",
  "eccentricity",
  "insolation_flux_earth_flux",
  "equilibrium_temperature_k",
  "stellar_mass_solar_mass",
  "stellar_effective_temp_k",
  "stellar_luminosity_log_solar"
)

# Ensure numeric
exodf[corr_vars] <- lapply(exodf[corr_vars], as.numeric)

# Correlation matrix
corr_matrix <- cor(exodf[corr_vars], use = "complete.obs")

# View correlations
round(corr_matrix, 2)

##-------------------------
## PCA
##------------------------

# Select variables for PCA
pca_vars <- c(
  "planet_mass_earth_mass",
  "planet_radius_earth_radius",
  "orbital_period_days",
  "orbit_semi_major_axis_au",
  "insolation_flux_earth_flux",
  "stellar_mass_solar_mass",
  "stellar_effective_temp_k"
)

# Remove rows with missing values
pca_data <- na.omit(exodf[pca_vars])
# Standardize + PCA
pca_res <- prcomp(pca_data, scale. = TRUE)
# PCA summary
summary(pca_res)
# Scree plot
plot(pca_res, type = "l", main = "PCA Scree Plot")
# Loadings
round(pca_res$rotation, 3)


table(exodf$num_planets_in_system)
boxplot(
  orbital_period_days ~ system_type,
  data = exodf,
  log = "y",
  main = "Orbital Period by System Type",
  ylab = "Orbital Period (days)"
)
head(
  exodf[order(exodf$host_star_name, exodf$orbit_semi_major_axis_au),
        c("host_star_name", "planet_name", "orbit_semi_major_axis_au")]
)

##-------------------------------------
##Model Building
##-------------------------------------
temp_lm <- lm(
  equilibrium_temperature_k ~
    orbit_semi_major_axis_au +
    stellar_mass_solar_mass +
    stellar_effective_temp_k +
    stellar_luminosity_log_solar +
    eccentricity,
  data = exodf
)

summary(temp_lm)
# Cluster Analysislibrary(ggplot2)
library(factoextra)

cluster_vars <- c(
  "planet_mass_earth_mass",
  "planet_radius_earth_radius",
  "orbit_semi_major_axis_au",
  "insolation_flux_earth_flux",
  "equilibrium_temperature_k"
)

cluster_df <- na.omit(exodf[cluster_vars])
cluster_df[] <- lapply(cluster_df, as.numeric)

cluster_scaled <- scale(cluster_df)

set.seed(123)
k3 <- kmeans(cluster_scaled, centers = 3, nstart = 50)

cluster_df$cluster <- factor(k3$cluster)


pca_cluster <- prcomp(cluster_scaled, scale. = FALSE)

fviz_pca_ind(
  pca_cluster,
  geom.ind = "point",
  col.ind = cluster_df$cluster,
  palette = c("maroon", "blue", "darkgreen"),
  addEllipses = TRUE,
  ellipse.level = 0.95,
  legend.title = "Cluster",
  pointsize = 2,
  alpha.ind = 0.7
) +
  labs(
    title = "PCA Projection of Exoplanet Clusters",
    subtitle = "Clusters based on planetary size and stellar energy balance"
  ) +
  theme_minimal()

ggplot(cluster_df, aes(
  x = insolation_flux_earth_flux,
  y = equilibrium_temperature_k,
  color = cluster
)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10() +
  labs(
    title = "Cluster Distribution by Stellar Energy and Temperature",
    x = "Insolation Flux (Earth units, log scale)",
    y = "Equilibrium Temperature (K)",
    color = "Cluster"
  ) +
  theme_minimal()

ggplot(cluster_df, aes(
  x = planet_radius_earth_radius,
  y = planet_mass_earth_mass,
  color = cluster
)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Planetary Mass–Radius Distribution by Cluster",
    x = "Planet Radius (Earth radii)",
    y = "Planet Mass (Earth masses)",
    color = "Cluster"
  ) +
  theme_minimal()


cluster_summary <- aggregate(
  cluster_df[, -ncol(cluster_df)],
  by = list(Cluster = cluster_df$cluster),
  FUN = median
)

cluster_summary

earth_ref <- c(
  mass = 1,
  radius = 1,
  insolation = 1,
  temp = 288
)

esi_df <- na.omit(cbind(
  mass = as.numeric(exodf$planet_mass_earth_mass),
  radius = as.numeric(exodf$planet_radius_earth_radius),
  insolation = as.numeric(exodf$insolation_flux_earth_flux),
  temp = as.numeric(exodf$equilibrium_temperature_k)
))

esi_scaled <- scale(esi_df)

earth_scaled <- (earth_ref - attr(esi_scaled, "scaled:center")) /
  attr(esi_scaled, "scaled:scale")

esi_score <- apply(esi_scaled, 1, function(x)
  sqrt(sum((x - earth_scaled)^2))
)

summary(esi_score)

temp_model <- lm(
  equilibrium_temperature_k ~
    orbit_semi_major_axis_au +
    stellar_luminosity_log_solar +
    stellar_effective_temp_k +
    eccentricity,
  data = exodf
)

summary(temp_model)

count_candidates <- function(Tmin, Tmax) {
  sum(
    exodf$equilibrium_temperature_k >= Tmin &
      exodf$equilibrium_temperature_k <= Tmax,
    na.rm = TRUE
  )
}

count_candidates(200, 350)
count_candidates(240, 320)
count_candidates(260, 300)


sun_like <- subset(exodf, stellar_mass_solar_mass >= 0.8 &
                     stellar_mass_solar_mass <= 1.2)

low_mass <- subset(exodf, stellar_mass_solar_mass < 0.8)
summary(sun_like$equilibrium_temperature_k)
summary(low_mass$equilibrium_temperature_k)

