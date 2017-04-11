#Create plot1 : Histgram 
create_plot1 <- function(dataFile) {
    
    # Load convenient libaries
    library(dplyr)
    library(readr)
    
    # Read data file
    hpc_data <- read_delim("household_power_consumption.txt", delim = ";")
    hpc_data <- tbl_df(hpc_data)
    print(head(hpc_data))
    
    # convert date data
    hpc_data <- mutate(hpc_data, Date = as.Date(strptime(Date, format = "%d/%m/%Y")))
    
    # filter necessary data
    hpc_data <- filter(hpc_data, Date == as.Date("2007/02/01"))
    
    # open png file
    png(filename = "plot01.png",
        width = 480,
        height = 480
        )
    
    # plot histgram
    hist(hpc_data$Global_active_power,
         main="Globl Active Power",
         xlab="Global Active Power(kilowatts)",
         ylab="Frequency",
         bg = "transparent",
         col = "red"
         )
    
    # close file
    dev.off()
}