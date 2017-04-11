#Create plot2 : Line graph 
create_plot2 <- function(dataFile) {
    
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
    png(filename = "plot02.png",
        width = 480,
        height = 480
        )
    
    # plot histgram
    plot(x = hpc_data$DateTime, 
         y = hpc_data$Global_active_power,
         type = "l",
         main = "Globl Active Power",
         xlab = "",
         ylab = "Global Active Power(kilowatts)",
         bg = "transparent",
         col = "black"
         )
    
    # close file
    dev.off()

    # Set original locale
    Sys.setlocale("LC_TIME", currentLocale)
}