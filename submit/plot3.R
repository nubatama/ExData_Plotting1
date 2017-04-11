#Create plot3 : Multi line graph 
create_plot3 <- function(dataFile) {
    
    # Load convenient libaries
    library(dplyr)
    library(readr)

    # Set locale
    currentLocale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "English_US")
    
    # Read data file
    hpc_data <- read_delim("household_power_consumption.txt", delim = ";")
    hpc_data <- tbl_df(hpc_data)
    print(head(hpc_data))
    
    # convert date data
    hpc_data <- mutate(hpc_data, 
                       Date = as.Date(strptime(Date, 
                                               format = "%d/%m/%Y")))
    
    # filter necessary data
    filter_date <- c(as.Date("2007/02/01"), as.Date("2007/02/02"))
    hpc_data <- filter(hpc_data, Date %in% filter_date)
    
    # Merge date and time
    hpc_data <- mutate(hpc_data, 
                       DateTime = as.POSIXct(paste(Date, Time), 
                                             format="%Y-%m-%d %H:%M:%S"))
    
    # open png file
    png(filename = "plot03.png",
        width = 480,
        height = 480
        )
    
    # plot histgram
    x_list <- c(hpc_data$DateTime, hpc_data$DateTime, hpc_data$DateTime)
    y_list <- c(hpc_data$Sub_metering_1, hpc_data$Sub_metering_2, hpc_data$Sub_metering_3)
    plot(x_list, y_list,
         type = "n",
         main = "",
         xlab = "",
         ylab = "Global Active Power(kilowatts)",
         bg = "transparent"
         )
    lines(hpc_data$DateTime, hpc_data$Sub_metering_1, col = "black")
    lines(hpc_data$DateTime, hpc_data$Sub_metering_2, col = "red")
    lines(hpc_data$DateTime, hpc_data$Sub_metering_3, col = "blue")
    
    legend("topright", 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           lty = c(1,1,1),
           col = c("black", "red", "blue")
           )
    
    # close file
    dev.off()

    # Set original locale
    Sys.setlocale("LC_TIME", currentLocale)
}