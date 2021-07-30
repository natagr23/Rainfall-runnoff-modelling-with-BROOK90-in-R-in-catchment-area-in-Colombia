
library(readxl)
#historical
climate <- read_excel("input/climate.xlsx")

climate$Year = format(climate$Date, "%Y")
climate$Year = as.numeric(climate$Year)

climate$Month = format(climate$Date, "%m")
climate$Month = as.numeric(climate$Month)

climate$Day = format(climate$Date, "%d")
climate$Day = as.numeric(climate$Day)

#future

climate2 <- read_excel("input/climate_future.xlsx")

climate2$Year = format(climate2$Date, "%Y")
climate2$Year = as.numeric(climate2$Year)

climate2$Month = format(climate2$Date, "%m")
climate2$Month = as.numeric(climate2$Month)

climate2$Day = format(climate2$Date, "%d")
climate2$Day = as.numeric(climate2$Day)


#qqplot
-----------------------------------------------------------------------

par(mfrow=c(1,1))
qqplot(climate$`Measured streamflow (mm)`,climate$sim_flow, xlab = "Observed Streamflow (mm/day)", ylab = "Simulated Streamflow (mm/day) (1980-2014)")
abline(0,1,col="red", grid())

qqplot(climate$`Measured streamflow (mm)`,climate2$`Sim flow CanESM2 (RCP 8.5)`, xlab = "Observed Streamflow (mm/day)", ylab = "CanESM2 (RCP 8.5) Simulated Streamflow (mm/day) (2021-2099)")
abline(0,1,col="red", grid())

qqplot(climate$`Measured streamflow (mm)`,climate2$`Sim flow CanESM2 (RCP 2.6)`, xlab = "Observed Streamflow (mm/day)", ylab = "CANESM2 (RCP 2.6) Simulated Streamflow (mm/day) (2021-2099)")
abline(0,1,col="red", grid())

qqplot(climate$`Measured streamflow (mm)`,climate2$`Sim flow IPSL-CM5A-MR (RCP 8.5)`, xlab = "Observed Streamflow (mm/day)", ylab = "IPSL-CM5A-MR (RCP 8.5) Simulated Streamflow (mm/day) (2021-2099)")
abline(0,1,col="red", grid())

qqplot(climate$`Measured streamflow (mm)`,climate2$`Sim flow IPSL-CM5A-MR (RCP 2.6)`,xlab = "Observed Streamflow (mm/day)", ylab = "IPSL-CM5A-MR (RCP 2.6) Simulated Streamflow (mm/day) (2021-2099)")
abline(0,1,col="red", grid())



-----------------------------------------------------------------------

#boxplot
----------------------------------------------------------------------
library(UsingR)
library(xts)
par(mfrow=c(2,1))


qplot(as.factor(Year),`Precipitation (mm)`,data=climate,geom="boxplot" ,
      xlab = "Year", ylab = "Observed Precipitation (mm/mo)")



qplot(as.factor(Year),`Measured streamflow (mm)`,data=climate,geom="boxplot" ,
      xlab = "Year", ylab = "Streamflow (mm/day)")

qplot(as.factor(Year),sim_flow,data=climate,geom="boxplot",
      xlab = "Year" , ylab = "Streamflow (mm/day)")

boxplot_year_fut1 <- qplot(as.factor(Year),climate2$`Sim flow CanESM2 (RCP 8.5)`,data=climate2,
      geom="boxplot" ,       xlab = "Year",
      ylab ="Streamflow (mm/day) (CANESM2 RCP 8.5)" )

boxplot_year_fut1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))


boxplot_year_fut2 <- qplot(as.factor(Year),climate2$`Sim flow CanESM2 (RCP 2.6)`,data=climate2,
                           geom="boxplot" ,       xlab = "Year",
                           ylab ="Streamflow (mm/day) (CANESM2 RCP 2.6)" )

boxplot_year_fut2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))


boxplot_year_fut3 <- qplot(as.factor(Year),climate2$`Sim flow IPSL-CM5A-MR (RCP 8.5)`,data=climate2,
                           geom="boxplot" ,       xlab = "Year",
                           ylab ="Streamflow (mm/day) (IPSL-CM5A-MR RCP 8.5)" )

boxplot_year_fut3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))


boxplot_year_fut4 <- qplot(as.factor(Year),climate2$`Sim flow IPSL-CM5A-MR (RCP 2.6)`,data=climate2,
                           geom="boxplot" ,       xlab = "Year",
                           ylab ="Streamflow (mm/day) (IPSL-CM5A-MR RCP 2.6)" )

boxplot_year_fut4 + theme(axis.text.x = element_text(angle = 90, hjust = 1))



par(mfrow=c(2,1))
qplot(as.factor(Month),`Measured streamflow (mm)`,
      data=climate,geom="boxplot", xlab = "Month" ,
      ylab = "Streamflow (mm/day)")

qplot(as.factor(Month),sim_flow,data=climate,geom="boxplot",
      xlab = "Month" , ylab = "Streamflow (mm/day)")


qplot(as.factor(Month),climate2$`Sim flow CanESM2 (RCP 8.5)`,data=climate2,geom="boxplot",
      xlab = "Month" , ylab = "CanESM2 (RCP 8.5)-Streamflow (mm/day)")

qplot(as.factor(Month),climate2$`Sim flow CanESM2 (RCP 2.6)`,data=climate2,geom="boxplot",
      xlab = "Month" , ylab = "CanESM2 (RCP 2.6)-Streamflow (mm/day)")

qplot(as.factor(Month),climate2$`Sim flow IPSL-CM5A-MR (RCP 8.5)`,data=climate2,geom="boxplot",
      xlab = "Month" , ylab = "IPSL-CM5A-MR (RCP 8.5)-Streamflow (mm/day)")

qplot(as.factor(Month),climate2$`Sim flow IPSL-CM5A-MR (RCP 2.6)`,data=climate2,geom="boxplot",
      xlab = "Month" , ylab = "IPSL-CM5A-MR (RCP 2.6)-Streamflow (mm/day)")





qplot(as.factor(Day),`Measured streamflow (mm)`,data=climate,geom="boxplot")
qplot(as.factor(Day),sim_flow,data=climate,geom="boxplot")

qplot(as.factor(Day),climate2$`Sim flow CanESM2 (RCP 8.5)`,data=climate2,geom="boxplot")
qplot(as.factor(Day),climate2$`Sim flow CanESM2 (RCP 2.6)`,data=climate2,geom="boxplot")

qplot(as.factor(Day),climate2$`Sim flow IPSL-CM5A-MR (RCP 8.5)`,data=climate2,geom="boxplot")
qplot(as.factor(Day),climate2$`Sim flow IPSL-CM5A-MR (RCP 2.6)`,data=climate2,geom="boxplot")

#by groups
--------------------------------------------------------------------------------

  #one
library(car)

qplot(data=climate, x=climate$Date, y=climate$`Measured streamflow (mm)`, geom="boxplot")
qplot(data=climate, x=climate$Date, y=climate$sim_flow, geom="boxplot")



#historical

table_boxplot <- data.frame(matrix(ncol = 2, nrow = 0))
table_measured <- cbind(rep("Observed", nrow(climate)), climate$`Measured streamflow (mm)`)
table_sim_flow <- cbind(rep("Simulated", nrow(climate)), climate$sim_flow)
table_boxplot <- rbind(table_boxplot, table_measured, table_sim_flow)
colnames(table_boxplot) <- c("type", "value")

table_boxplot$value <- as.numeric(as.character(table_boxplot$value))

qplot(data=table_boxplot, x=table_boxplot$type,
      y=table_boxplot$value, geom="boxplot", ylab = "Streamflow (mm/day)")

qplot(data=table_boxplot, x=table_boxplot$type,
      y=table_boxplot$value, geom="boxplot", log = "y", ylab = "Streamflow (mm/day)")

-------------------------------------------------------------------------------------

 #future

table_boxplot2 <- data.frame(matrix(ncol = 2, nrow = 0))
table_sim_flow_can85 <- cbind(rep("Sim. flow CanESM2 (RCP 8.5)", nrow(climate2)), climate2$`Sim flow CanESM2 (RCP 8.5)`)
table_sim_flow_can26 <- cbind(rep("Sim. flow CanESM2 (RCP 2.6)", nrow(climate2)), climate2$`Sim flow CanESM2 (RCP 2.6)`)
table_sim_flow_ipls85 <- cbind(rep("Sim. flow IPSL-CM5A-MR (RCP 8.5)", nrow(climate2)), climate2$`Sim flow IPSL-CM5A-MR (RCP 8.5)`)
table_sim_flow_ipls26 <- cbind(rep("Sim. flow IPSL-CM5A-MR (RCP 2.6)", nrow(climate2)), climate2$`Sim flow IPSL-CM5A-MR (RCP 2.6)`)


table_boxplot2 <- rbind(table_boxplot2, table_sim_flow_can85,
                        table_sim_flow_can26,table_sim_flow_ipls85, table_sim_flow_ipls26 )
colnames(table_boxplot2) <- c("type", "value")

table_boxplot2$value <- as.numeric(as.character(table_boxplot2$value))

qplot(data=table_boxplot2, x=table_boxplot2$type, y=table_boxplot2$value,
      geom="boxplot", ylab = "Streamflow (mm/day)")


qplot(data=table_boxplot2, x=table_boxplot2$type, y=table_boxplot2$value,
      geom="boxplot", log = "y", ylab = "Streamflow (mm/day)")


  #barplot
-----------------------------------------------------------------------------
library(ggplot2)

library(dplyr)

mgroup <- climate %>%
          group_by( Month)%>%
          summarize( mean=mean(`Measured streamflow (mm)`),
                    max=max(`Measured streamflow (mm)`),
                    min=min(`Measured streamflow (mm)`))

ggplot()+ geom_bar(data=mgroup,aes(x=Month, y=mean),stat="identity")

n_mgroup=reshape2::melt(mgroup,id.var=("Month"))

#mean, max, min bar
ggplot()+ geom_bar(data=n_mgroup,aes(x=Month, y=value,fill=variable),
           stat="identity",position="dodge")

#lines: 4 grahics in the same window
par(mfrow=c(2,2))
plot(climate$`Measured streamflow (mm)` ~ climate$Date, type="l",
     col="red", main="Fig 1")
plot(climate$sim_flow ~ climate$Date, type="l", col="red", main="Fig 2")
plot(climate$`Precipitation (mm)` ~ climate$Date, type="l", col="red",
     main="Fig 3")

#hydrological result
par(mfrow=c(1,1))
Date_format = as.POSIXct(Date)


par(mar=c(5,5,2,5))

plot(Date_format,climate$sim_evp , ylab="Streamflow [mm/day]", xlab="Date", type =
       "l", col="green", ylim=(c(0,20)))
lines(Date_format, climate$`Measured streamflow (mm)`, col="red")
lines(Date_format, climate$sim_flow, col="blue")
par(new=T)
plot(Date_format, climate$`Precipitation (mm)`, xlab="", ylab="", col="darkred", type="n", axes=F,
     ylim=rev(c(0,150)))
lines(Date_format, climate$`Precipitation (mm)`, col="darkred",lty=3)
axis(4)
mtext("Rain (mm)", side=4, line=3 )


#comparing climate in differents periods

library(dplyr)

library(ggplot2)


pgroup <- climate %>%
  group_by( Month, Year)%>%
  summarize( max_pt=sum(`Precipitation (mm)`))

pgroup  %>%
  mutate(Month2 = as.Date(paste0("2014-", Month,"-01"),"%Y-%m-%d")) %>%
  ggplot(aes(x = Month2, y = max_pt)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ Year, ncol = 3) +
  labs(title = "Total Monthly Precipitation",
       subtitle = "Data plotted by year",
       y = "Daily precipitation (inches)",
       x = "Month") + theme_bw(base_size = 5) +
scale_x_date(date_labels = "%b")

#comparing 2





