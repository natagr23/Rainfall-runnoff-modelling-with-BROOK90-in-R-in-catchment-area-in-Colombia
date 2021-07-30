library(readxl)
climatic_info <- read_excel("Input_constant/meteo_bogota_his.xlsx",
                            col_types = c("text", "text", "text",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric"))

climatic_info_fut <- read_excel("Input_constant/meteo_bogota.xlsx",
                            col_types = c("text", "text", "text",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric"))


View(climatic_info)

#sapply(climatic_info, class) revisar el tipo de clase de una tabla por año

# calcula porcentaje de faltantes de TMIN
climatic_info %>%
  group_by(Year)  %>%
  summarize(
    total=n(),
    total_nan_tmin = sum(is.na(`TMIN(°C)`)),
    percentage = (total_nan_tmin / total) * 100
  )

# calcula porcentaje de faltantes de TMIN agrupado por año y mes
climatic_info %>%
  group_by(Year, Month)  %>%
  summarize(
    total=n(),
    total_nan_tmin = sum(is.na(`TMIN(°C)`)),
    percentage = (total_nan_tmin / total) * 100
  )

# temperature mean, max, min
climatic_info %>%
  group_by(Month)  %>%
  summarize(
    average_temp = mean(`TMIN(°C)`, na.rm=TRUE),
    max_temp = max (`TMIN(°C)`, na.rm=TRUE ),
    min_temp=  min (`TMIN(°C)`, na.rm=TRUE)
  )

library(dplyr)

summary_table <- climatic_info %>%
  group_by(Year)  %>%
  summarize(
    total=n(),
    total_nan_param = sum(is.na(`TMIN(°C)`)
                          | is.na(`Precipitation (mm)`)
                          | is.na(`TMAX(°C)`)
                          ),
    percentage = (total_nan_param / total) * 100,

    total_nan_tmin = sum(is.na(`TMIN(°C)`)),
    percentage_tmin = (total_nan_tmin / total) * 100
  )

View(summary_table)


#missing data Tmin, Tprec, Tmax per year
library(dplyr)

summary_missing_data <- climatic_info %>%
  group_by(Year)  %>%
  summarize(
    total=n(),
    total_nan_param = sum(is.na(`TMIN(°C)`)
                          | is.na(`Precipitation (mm)`)
                          | is.na(`TMAX(°C)`)
    ),
    percentage = (total_nan_param / total) * 100,
    total_nan_tmin = sum(is.na(`TMIN(°C)`)),
    percentage_tmin = (total_nan_tmin / total) * 100,
    total_nan_prec = sum(is.na(`Precipitation (mm)`)),
    percentage_prec = (total_nan_prec/ total) * 100,
    total_nan_tmax = sum(is.na(`TMAX(°C)`)),
    percentage_tmax = (total_nan_tmax/ total) * 100,
    total_nan_wind_sp = sum(is.na(`Wind speed (m/s)`)),
    percentage_wind_sp = (total_nan_wind_sp/ total) * 100,
    total_nan_vapor_p = sum(is.na(`Vapor pressure (kPa)`)),
    percentage_vapor_p= (total_nan_vapor_p/ total) * 100,
    total_nan_solar_r = sum(is.na(`Solar radiation (MJ/m2)`)),
    percentage_solar_r= (total_nan_solar_r/ total) * 100

  )

View(summary_missing_data)


# mean diary per parameter
library(dplyr)
mean_daily_data <- climatic_info %>%
  group_by(Month, Day)  %>%
  summarize(
    mean_temp_min = mean(`TMIN(°C)`, na.rm=TRUE),
    mean_temp_max = mean (`TMAX(°C)`, na.rm=TRUE ),
    mean_precip=  mean(`Precipitation (mm)`, na.rm=TRUE),
    mean_solar_rad=  mean( `Solar radiation (MJ/m2)`, na.rm=TRUE),
    mean_vapor_pres=  mean( `Vapor pressure (kPa)`, na.rm=TRUE),
    mean_wind_sp=  mean( `Wind speed (m/s)`, na.rm=TRUE),
    mean_obs_flow=  mean( `Measured streamflow (m3/s)`, na.rm=TRUE)
  )

# Fix wind speed ----------------------------------------------------------------------------------------
replace_na_wind <- function(value_wind_speed, param_month, param_day)
{

  if(is.na(value_wind_speed) || value_wind_speed == 0 )
  {
    # update missing value with monthly-daily mean
    value_wind_speed <- mean_daily_data[which(mean_daily_data$Month == param_month & mean_daily_data$Day == param_day),]$mean_wind_sp
  }

  return(value_wind_speed)
}

# use mapply para ejecutar la funcion [replace_na_wind] sobre los elementos de la tabla [table_wind_speed]
climatic_info_fut$wind_corrected <- mapply(replace_na_wind,
                                       climatic_info_fut$`Wind speed (m/s)`,
                                       climatic_info_fut$Month,
                                       climatic_info_fut$Day)

# Fix temp min -----------------------------------------------------------------------------------------------------
replace_na_tmin <- function(value_tmin, param_month, param_day)
{

  if(is.na(value_tmin) || value_tmin == 0 )
  {
    # update missing value with monthly-daily mean
    value_tmin <- mean_daily_data[which(mean_daily_data$Month == param_month & mean_daily_data$Day == param_day),]$mean_temp_min
  }

  return(value_tmin)
}

climatic_info$tmin_corrected <- mapply(replace_na_tmin,
                                       climatic_info$`TMIN(°C)`,
                                       climatic_info$Month,
                                       climatic_info$Day)

View(climatic_info)


# Fix temp max -----------------------------------------------------------------------------------------------------
replace_na_tmax<- function(value_tmax, param_month, param_day)
{

  if(is.na(value_tmax) || value_tmax == 0 )
  {
    # update missing value with monthly-daily mean
    value_tmax <- mean_daily_data[which(mean_daily_data$Month == param_month & mean_daily_data$Day == param_day),]$mean_temp_max
  }

  return(value_tmax)
}

climatic_info$tmax_corrected <- mapply(replace_na_tmax,
                                       climatic_info$`TMAX(°C)`,
                                       climatic_info$Month,
                                       climatic_info$Day)

# Fix temp wind speed -----------------------------------------------------------------------------------------------------
replace_na_wspeed<- function(value_wspeed, param_month, param_day)
{

  if(is.na(value_wspeed) || value_wspeed == 0 )
  {
    # update missing value with monthly-daily mean
    value_wspeed <- mean_daily_data[which(mean_daily_data$Month == param_month & mean_daily_data$Day == param_day),]$mean_wind_sp
  }

  return(value_wspeed)
}

climatic_info$wspeed_corrected <- mapply(replace_na_wspeed,
                                       climatic_info$`Wind speed (m/s)`,
                                       climatic_info$Month,
                                       climatic_info$Day)


View(climatic_info)

# Fix  vapor pressure -----------------------------------------------------------------------------------------------------
replace_na_vapourp<- function(value_vapourp, param_month, param_day)
{

  if(is.na(value_vapourp) || value_vapourp == 0 )
  {
    # update missing value with monthly-daily mean
    value_vapourp <- mean_daily_data[which(mean_daily_data$Month == param_month & mean_daily_data$Day == param_day),]$mean_vapor_pres
  }

  return(value_vapourp)
}

climatic_info_fut$vapourp_corrected <- mapply(replace_na_vapourp,
                                         climatic_info_fut$`Vapor pressure (kPa)`,
                                         climatic_info_fut$Month,
                                         climatic_info_fut$Day)

# Fix solar radiation -----------------------------------------------------------------------------------------------------
replace_na_solar_r<- function(value_solar_r, param_month, param_day)
{

  if(is.na(value_solar_r) || value_solar_r == 0 )
  {
    # update missing value with monthly-daily mean
    value_solar_r <- mean_daily_data[which(mean_daily_data$Month == param_month & mean_daily_data$Day == param_day),]$mean_solar_rad
  }

  return(value_solar_r)
}

climatic_info_fut$solar_r_corrected <- mapply(replace_na_solar_r,
                                          climatic_info_fut$`Solar radiation (MJ/m2)`,
                                          climatic_info_fut$Month,
                                          climatic_info_fut$Day)


View(climatic_info)



# mapply(nombre_de_funcion, parametro_1, parametro_2, ....)
#for i = 0, i<len(table_wind_speed), i++:
#  climatic_info$wind_corrected[i] = replace_na_wind(table_wind_speed[i])

write.table(climatic_info_fut, file = "Input_constant/param_corrected_fut.csv")


