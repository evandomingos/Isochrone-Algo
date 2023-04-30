library(ggplot2)
library(jsonlite)
install.packages("reshape2")
library(reshape2)
install.packages("plotly")
library(plotly)
install.packages("kernlab")
library(kernlab)
install.packages("gstat")
install.packages("sp")
library(gstat)
library(sp)
install.packages("rgl")
library(rgl)
library(mgcv)
install.packages("mgcv")

url <- "https://raw.githubusercontent.com/mak08/BitsailorImage/d7b6cef9223311ac1e28ae19d3d38d1991c98efa/polars/3.json"
json_list <- fromJSON(url, flatten = FALSE)
str(json_list)

json_list$scriptData$polar$sail$speed

#get length
length(json_list$scriptData$polar$sail$speed)
str(json_list$scriptData$polar$sail$speed)

# Create a list to store data frames
df_list <- list()

# Loop through each sail configuration
for (i in seq_along(json_list$scriptData$polar$sail$speed)) {
  
  # Extract sail ID and name
  sail_id <- json_list$scriptData$polar$sail$id[i]
  sail_name <- json_list$scriptData$polar$sail$name[i]
  
  # Convert matrix to data frame
  sail_df <- data.frame(json_list$scriptData$polar$sail$speed[[i]])
  
  # Add wind angles from TWA as row names
  rownames(sail_df) <- json_list$scriptData$polar$twa
  
  # Rename columns with wind speeds
  colnames(sail_df) <- paste0(json_list$scriptData$polar$tws)
  
  # Add sail ID and name as columns
  sail_df$sail_id <- sail_id
  sail_df$sail_name <- sail_name
  sail_df$wind_angle <- rownames(sail_df)
  
  # Reorder columns
  sail_df <- sail_df[, c("sail_id", "sail_name", "wind_angle", paste0(json_list$scriptData$polar$tws))]
  
  # Add data frame to list
  df_list[[i]] <- sail_df
}

# Combine data frames into a single data frame
sails_df <- do.call(rbind, df_list)

sails_df
# Reset row names
rownames(sails_df) <- NULL

# Preview the resulting data frame
head(sails_df)

# Reorder columns
sails_df <- sails_df[, c("wind_angle","sail_id", "sail_name", paste0(json_list$scriptData$polar$tws))]
sails_df

#rotate
# Reshape the data frame to long format
sails_df_long <- sails_df %>%
  pivot_longer(cols = -c(wind_angle, sail_id, sail_name),
               names_to = "wind_speed",
               values_to = "boat_speed") %>%
  mutate(wind_speed = as.numeric(wind_speed),
         boat_speed = as.numeric(boat_speed),
         wind_angle = as.numeric(wind_angle))

sails_df_long
# Filter data for each sail_id and create a list of data frames
sails_df_list <- sails_df_long %>%
  group_by(sail_id) %>%
  filter(!is.na(boat_speed)) %>%
  group_split()

sails_df_list[[1]]
head(sails_df_list)

# Filter data for sail_id == 1
sails_df_sail1 <- sails_df_list[[1]]
#<- subset(sails_df_list, sail_id == 1)
sails_df_sail1

#Fitting a model for boat speed
library(tidyr)
library(dplyr)

# Reshape the data frame to long format

sails_df_sail1_long <- sails_df_sail1 %>%
  pivot_longer(cols = -c(wind_angle, sail_id, sail_name),
               names_to = "wind_speed",
               values_to = "boat_speed") %>%
  mutate(wind_speed = as.numeric(wind_speed), wind_angle = as.numeric(wind_angle))

# Create mirrored angles column
sails_df_sail1_long$mirrored_angle <- 360 - sails_df_sail1_long$wind_angle
# Combine original and mirrored angles
sails_df_sail1_long <- rbind(sails_df_sail1_long, transform(sails_df_sail1_long, wind_angle = mirrored_angle))

sails_df_sail1_long






#fit model
#model_poly <- lm(boat_speed ~ wind_angle + wind_speed + I(wind_angle^2), data = sails_df_sail1_long)
#model_poly <- lm(boat_speed ~ wind_angle + wind_speed + I(wind_angle^2) + I(wind_speed^2) + wind_angle*wind_speed + I(wind_angle^3) + I(wind_speed^3) + I(wind_angle^2)*wind_speed + wind_angle*I(wind_speed^2) + I(wind_angle^4) , data = sails_df_sail1_long)


#Method to fit surface: 

# Define the formula
formula <- boat_speed ~ s(wind_angle, wind_speed, bs = "tp")
# Fit the GAM model
model_gam <- gam(formula, data = sails_df_sail1_long)
# Create a grid of wind angles and wind speeds
wind_angles <- seq(0, 360, length.out = 50)
wind_speeds <- seq(0, 70, length.out = 50)
grid <- expand.grid(wind_angle = wind_angles, wind_speed = wind_speeds)

# Use the GAM model to predict boat speeds on the grid
grid$pred_gam <- predict(model_gam, newdata = grid)
grid$pred_gam <- pmax(0, pmin(grid$pred_gam, 50))


# Define the formula
formula <- boatspeed ~ s(wind_angle, wind_speed, bs = "tp")

# Fit the GAM model
model_gam <- gam(formula, data = sails_df_sail1_long)

# Create a grid of wind angles and wind speeds
wind_angles <- seq(0, 360, length.out = 50)
wind_speeds <- seq(0, 70, length.out = 50)
grid <- expand.grid(wind_angle = wind_angles, wind_speed = wind_speeds)

# Use the GAM model to predict boat speeds on the grid
grid$pred_gam <- predict(model_gam, newdata = grid)

# Limit the predicted boat speeds to be between 0 and 50
grid$pred_gam <- pmax(0, pmin(grid$pred_gam, 50))

# Create a 3D surface plot of the predicted boat speeds
plot_ly(grid, x = ~wind_angle, y = ~wind_speed, z = ~pred_gam, type = "surface") %>% 
  layout(scene = list(xaxis = list(title = "Wind Angle"),
                      yaxis = list(title = "Wind Speed"),
                      zaxis = list(title = "Boat Speed")))

#generlaized n degree
degree <- 4
poly_formula <- as.formula(paste("boat_speed ~", paste(paste0("poly(wind_angle, degree=", degree, ", raw=TRUE)"), paste0(" + poly(wind_speed, degree=", degree-1, ", raw=TRUE)"), collapse = " + ")))
model_poly <- lm(poly_formula, data = sails_df_sail1_long)
summary(model_poly)

sails_df_sail1_long$pred_poly <- predict(model_poly, newdata = sails_df_sail1_long)
sails_df_sail1_long$resid_poly <- sails_df_sail1_long$boat_speed - sails_df_sail1_long$pred_poly
mse_poly <- mean(sails_df_sail1_long$resid_poly^2)
mse_poly

#dudnt work
#model_log <- nls(boat_speed ~ a + b*log(wind_speed) + c*wind_angle, data = sails_df_sail1_long, start = list(a = 1, b = 1, c = 1))
#summary(model_log)


#Plot Models

#Cartesian Surface Plot
wind_angles <- seq(0, 360, length.out = 50)
wind_speeds <- seq(0, 70, length.out = 50)
grid <- expand.grid(wind_angle = wind_angles, wind_speed = wind_speeds)

# Make predictions using the polynomial model
grid$pred_poly <- predict(model_poly, newdata = grid)
grid$pred_poly[grid$wind_speed > 70] <- 0
grid$pred_poly[grid$pred_poly > 50] <- 50

# Reshape data for plotly
z_poly <- acast(grid, wind_speed ~ wind_angle, value.var = "pred_poly")

# Create 3D plot of the polynomial model
plot_ly(x = ~wind_angles, y = ~wind_speeds, z = ~z_poly, type = "surface") %>% 
  add_surface(color = I("blue"), opacity = 0.7) %>%
  layout(scene = list(xaxis = list(title = "Wind Angle"),
                      yaxis = list(title = "Wind Speed"),
                      zaxis = list(title = "Boat Speed")))

# Polar Surface Plot
library(plotly)

# Define data
wind_angles <- seq(0, 360, length.out = 50)
wind_speeds <- seq(0, 100, length.out = 50)
grid <- expand.grid(wind_angle = wind_angles, wind_speed = wind_speeds)
grid$pred_poly <- predict(model_poly, newdata = grid)
z_poly <- acast(grid, wind_speed ~ wind_angle, value.var = "pred_poly")

# Reshape data for polar plot
polar_df <- data.frame(
  r = as.vector(z_poly),
  theta = rep(wind_angles, each = length(wind_speeds)),
  wind_speed = rep(wind_speeds, times = length(wind_angles))
)

# Create polar plot

theta <- grid$wind_angle
r <- grid$wind_speed
z <- grid$pred_poly

plot_ly(
  polar_df,
  type = "scatterpolar",
  mode = "markers",
  r = ~r,
  theta = ~theta,
  color = ~wind_speed,
  colors = "Blues"
) %>% layout(polar = list(radialaxis = list(title = "Boat Speed")))


#Polar Charts


