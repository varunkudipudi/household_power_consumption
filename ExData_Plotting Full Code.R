#Part1 

#Remove All Variables From Environment
rm(list = ls())

#Setup Working Directory
#setwd('/Users///datasciencecoursera/')

#Read In File
p1dat = read.table('household_power_consumption.txt', header = T,
                   sep = ';', na.strings = c('NA', '?', ''))

#Convert "Date" Column Into Date Using Lubridate
library(lubridate)
p1dat$Date = dmy(p1dat$Date)

#Filter Out Data For Only Two Days
selected_dates = ymd(c('2007-02-01', '2007-02-02'))
filtered_p1dat = p1dat[p1dat$Date %in% selected_dates, ]

#Create Date Time Field
filtered_p1dat$DateTime = with(filtered_p1dat, ymd_hms(paste(Date, Time)))

#Remove full data frame as we only need data for two days
rm(p1dat)

#Part 2
#Plot 1

#Draw Plot and Send to PNG
hist(filtered_p1dat$Global_active_power, main = "Global Active Power", 
     xlab = 'Global Active Power (kilowatts)', col = 'red')

#Copy To JPEG

dev.copy(jpeg, file = 'varunplot1.jpg',width=1024, height=610)
dev.off()

#Plot 2

with(filtered_p1dat, 
     plot(DateTime, Global_active_power, 
          ylab =  'Global Active Power (kilowatts)',
          pch = NA,
          lines(DateTime, Global_active_power,)
     )
)

#Copy To JPEG

dev.copy(jpeg, file = 'varunplot2.jpg',width=1024, height=610)
dev.off()

#Since there is so much code involved with creating Plot 3
#I made a function so I can call it later for other plots
CreatePlot3 = function(){
  #Add first line and baseline structure
  with(filtered_p1dat, {
    plot(DateTime, Sub_metering_1, 
         ylab =  'Energy sub metering',
         xlab = '',
         pch = NA,
         lines(DateTime, Sub_metering_1)) 
    #Add Second Line
    lines(DateTime, Sub_metering_2, 
          col = 'red')
    #Add Third Line
    lines(DateTime, Sub_metering_3, 
          col = 'blue')
    #Add Legend
    legend('topright', lty = 1, lwd = 2, col = c('black', 'red', 'blue'),
           legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')
    )
  })
}

#Copy To JPEG
CreatePlot3()
dev.copy(jpeg, file = 'varunplot3.jpg',width=1024, height=610)
dev.off()


#setup canvas
par(mfrow = c(2, 2))


with(filtered_p1dat,{ 
  #First Plot
  plot(DateTime, Global_active_power, 
       ylab =  'Global Active Power',
       pch = NA,
       lines(DateTime, Global_active_power))
  #Second Plot
  plot(DateTime, Voltage, 
       ylab =  'Voltage',
       xlab = 'datetime',
       pch = NA,
       lines(DateTime, Voltage))
  #Third Plot (Created a Function To Do This)
  CreatePlot3()
  #Fourth Plot
  plot(DateTime, Global_reactive_power, 
       xlab = 'datetime',
       pch = NA,
       lines(DateTime, Global_reactive_power))
})

#Copy To JPEG
dev.copy(jpeg, file = 'varunplot4.jpg',width=1024, height=610)
dev.off()
