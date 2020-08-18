#  Takes roster.csv and Operator Login Logout.csv
# Version 3 - rm and restored somehow

adherence <- function(Weekday){
    
    # setwd("~/Desktop/U Drive Replicated/Adherence Report by R")
    
    adherence <- read.csv("Operator Login Logout.csv")
    library(chron)
    
    adherence <- subset(adherence, substr(Operator, 1, 5) == "COMPANY -")
    Date <- substr(adherence$Login, 1, 10)
    adherence <- cbind(adherence, Date)
    adherence$Login <- chron(times = substr(strptime(substr(adherence$Login,12, 22), "%I:%M:%S %p"), 12, 19))
    adherence$Logout <- chron(times = substr(strptime(substr(adherence$Logout,12, 22), "%I:%M:%S %p"), 12, 19))
    adherence <- adherence[order(adherence[,2]),]
    adherence <- adherence[order(adherence[,1]),]
    rownames(adherence) <- NULL
    
    name_vector <- unique(adherence[,1])
    
    
    all_data <- data.frame()
    
    for (name in name_vector){
        
        advisor <- subset(adherence, Operator == name)
        
        if (nrow(advisor) >= 2){
            Break <-c("00:00:00")
            for (i in 1:(nrow(advisor)-1)){
                
                if (chron(times =advisor[i+1, "Login"]) >= chron(times = advisor[i, "Logout"])){
                    
                    time <- as.character(chron(times =advisor[i+1, "Login"]) - chron(times = advisor[i, "Logout"]))
                    Break <- c(Break, time)
                    Break <- chron(times = Break)
                }
                
                else {
                    time <- "00:00:00"
                    Break <- c(Break, time)
                    Break <- chron(times = Break)
                }
            } 
            
            advisor_new <- data.frame(advisor, Break)
            
            # create a one-row matrix the same length as advisor_new
            temprow <-matrix(c(rep.int(NA, 6)), nrow = 1, ncol = 6)
            
            # make it a data.frame and give cols the same names as data
            newrow <- data.frame(temprow)
            colnames(newrow) <- colnames(advisor_new)
            
            newrow$Advisor <- advisor_new[1, "Operator"]
            newrow$Login <- as.character(advisor_new[1, "Login"])
            newrow$Logout <- as.character(advisor_new[nrow(advisor_new), "Logout"])
            newrow$Session.Length <- as.character(sum(chron(times = advisor_new$Session.Length)))
            newrow$Date <- advisor_new[1, "Date"]
            newrow$Break <- as.character(sum(chron(time = advisor_new$Break)))
            
            # without this step, time becomes decimal numbers when rbind all.
            as.character(newrow)
            all_data <-rbind(all_data, newrow)
            
        } #close for the 1st if
        
        else {
            Break <-c("00:00:00")
            Break <- chron(times = Break)
            advisor_new <- data.frame(advisor, Break)
            
            # create a one-row matrix the same length as advisor_new
            temprow <-matrix(c(rep.int(NA, 6)), nrow = 1, ncol = 6)
            
            # make it a data.frame and give cols the same names as data
            newrow <- data.frame(temprow)
            colnames(newrow) <- colnames(advisor_new)
            
            newrow$Advisor <- advisor_new[1, "Operator"]
            newrow$Login <- as.character(advisor_new[1, "Login"])
            newrow$Logout <- as.character(advisor_new[nrow(advisor_new), "Logout"])
            newrow$Session.Length <- as.character(sum(chron(times = advisor_new$Session.Length)))
            newrow$Date <- advisor_new[1, "Date"]
            newrow$Break <- as.character(sum(chron(time = advisor_new$Break)))
            
            as.character(newrow) 
            all_data <-rbind(all_data, newrow)
        }
    } 
    
    all_data$Advisor <- substring(all_data$Advisor, 7)
    all_data$Advisor[all_data$Advisor == "Daisy May"] <- "Daisy-May Carty-Cowling"
    
    all_data$Operator <- NULL
    all_data$Session.Length <- NULL
    
    roster <- read.csv("roster.csv")
    
    Start_Date <- as.Date(roster[1, "WEEK"], "%d/%m/%Y")
    
    if (Weekday == "Monday"){
        
        date_string <- as.character(Start_Date)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
        
        
        roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
        roster$Date <- as.character(substr(roster$DAY_0_START, 1, 10))
    
        roster$Start <- strptime(substr(roster$DAY_0_START, 12, 22), "%I:%M:%S %p")
        roster$Start <- as.character(substr(roster$Start, 12, 16))
    
        roster$End <- strptime(substr(roster$DAY_0_STOP, 12, 22), "%I:%M:%S %p")
        roster$End <- as.character(substr(roster$End, 12, 16))
        roster$Shift <- paste(roster$Start, roster$End, sep = "-")
        roster$Leave <- as.character(roster$DAY_0_GEN)
    # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
        new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)
        } else if (date_string2 == 0) {
        
        roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
        roster$Date <- as.character(substr(roster$DAY_0_START, 1, 9))
        
        roster$Start <- strptime(substr(roster$DAY_0_START, 11, 21), "%I:%M:%S %p")
        roster$Start <- as.character(substr(roster$Start, 12, 16))
        
        roster$End <- strptime(substr(roster$DAY_0_STOP, 11, 21), "%I:%M:%S %p")
        roster$End <- as.character(substr(roster$End, 12, 16))
        roster$Shift <- paste(roster$Start, roster$End, sep = "-")
        roster$Leave <- as.character(roster$DAY_0_GEN)
        new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }   
    } else if (Weekday == "Tuesday"){
        
        date_string <- as.character(Start_Date + 1)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
        
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_1_START, 1, 10))
        
            roster$Start <- strptime(substr(roster$DAY_1_START, 12, 22), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
        
            roster$End <- strptime(substr(roster$DAY_1_STOP, 12, 22), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_1_GEN)
        
            # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE) 
        } else if (date_string2 == 0) {
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_1_START, 1, 9))
            
            roster$Start <- strptime(substr(roster$DAY_1_START, 11, 21), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_1_STOP, 11, 21), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_1_GEN)
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }   
        
    }   else if (Weekday == "Wednesday"){
        
        date_string <- as.character(Start_Date + 2)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_2_START, 1, 10))
            
            roster$Start <- strptime(substr(roster$DAY_2_START, 12, 22), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_2_STOP, 12, 22), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_2_GEN)
            
            # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE) 
        } else if (date_string2 == 0) {
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_2_START, 1, 9))
            
            roster$Start <- strptime(substr(roster$DAY_2_START, 11, 21), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_2_STOP, 11, 21), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_2_GEN)
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }  
    } else if (Weekday == "Thursday"){
        date_string <- as.character(Start_Date + 3)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_3_START, 1, 10))
            
            roster$Start <- strptime(substr(roster$DAY_3_START, 12, 22), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_3_STOP, 12, 22), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_3_GEN)
            
            # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE) 
        } else if (date_string2 == 0) {
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_3_START, 1, 9))
            
            roster$Start <- strptime(substr(roster$DAY_3_START, 11, 21), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_3_STOP, 11, 21), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_3_GEN)
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }  
    } else if (Weekday == "Friday"){
        date_string <- as.character(Start_Date + 4)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_4_START, 1, 10))
            
            roster$Start <- strptime(substr(roster$DAY_4_START, 12, 22), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_4_STOP, 12, 22), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_4_GEN)
            
            # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE) 
        } else if (date_string2 == 0) {
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_4_START, 1, 9))
            
            roster$Start <- strptime(substr(roster$DAY_4_START, 11, 21), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_4_STOP, 11, 21), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_4_GEN)
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }  
    } else if (Weekday == "Saturday"){
        date_string <- as.character(Start_Date + 5)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_5_START, 1, 10))
            
            roster$Start <- strptime(substr(roster$DAY_5_START, 12, 22), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_5_STOP, 12, 22), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_5_GEN)
            
            # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE) 
        } else if (date_string2 == 0) {
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_5_START, 1, 9))
            
            roster$Start <- strptime(substr(roster$DAY_5_START, 11, 21), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_5_STOP, 11, 21), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_5_GEN)
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }  
    } else if (Weekday == "Sunday"){
        date_string <- as.character(Start_Date + 6)
        date_string2 <- as.numeric(substr(date_string, 9,9))
        
        if (date_string2 >= 1){
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_6_START, 1, 10))
            
            roster$Start <- strptime(substr(roster$DAY_6_START, 12, 22), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_6_STOP, 12, 22), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_6_GEN)
            
            # stringsAsFactors=FALSE is necessary for new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE) 
        } else if (date_string2 == 0) {
            
            roster$Operator <- paste(roster$EMP_FIRST_NAME, roster$EMP_LAST_NAME)
            roster$Date <- as.character(substr(roster$DAY_6_START, 1, 9))
            
            roster$Start <- strptime(substr(roster$DAY_6_START, 11, 21), "%I:%M:%S %p")
            roster$Start <- as.character(substr(roster$Start, 12, 16))
            
            roster$End <- strptime(substr(roster$DAY_6_STOP, 11, 21), "%I:%M:%S %p")
            roster$End <- as.character(substr(roster$End, 12, 16))
            roster$Shift <- paste(roster$Start, roster$End, sep = "-")
            roster$Leave <- as.character(roster$DAY_6_GEN)
            new_roster <- data.frame(roster$Date, roster$Operator, roster$Shift, roster$Leave, stringsAsFactors=FALSE)     
        }  
    }
    
    #Renaming columns - colnames become roster.Date etc after data.frame()
    colnames(new_roster) <- c("Date", "Advisor", "Shift", "Leave")
    new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
    
    # Just in case, if you do not use stringsAsFactors=FALSE...
    # For the columns that are factors, you can only assign values that are factor levels. 
    # If you wanted to assign a value that wasn't currently a factor level, 
    # you would need to create the additional level first
    
    #levels(new_roster$Shift) <- c(levels(new_roster$Shift), "OFF") 
    #new_roster$Shift[new_roster$Shift == "NA-NA"] <- "OFF"
    
    new_roster <- new_roster[order(new_roster[, 2]),]
    
    row.names(new_roster) <- NULL
    
    
        
    outcome <- merge(new_roster, all_data, by = "Advisor", all = TRUE)
    dates_from_roster <- date_string
    dates_from_loginlogout <- as.character(all_data[1,3])
    
    print("Roster Date: ")
    print(date_string)
    print("Login Logout Date: ")
    print(dates_from_loginlogout)
    
    outcome$Date.y <- NULL
     
    
    # How to rename a single column: without [2], it doesn't work
    
    colnames(outcome)[2] <- "Date"
    
    outcome <- outcome[order(outcome[,1]),]
    
    rownames(outcome) <- NULL
    
    # Replacing NA with empty value
    outcome[is.na(outcome)] <- " "
    
    write.csv(outcome, "adherence.csv", row.names=FALSE)
    
    loginlogout("Operator Login Logout.csv")
    
    outcome
}

loginlogout <- function(directory){
    
    file <- read.csv(directory)
    library(chron)
    
    file1 <- subset(file, substr(Operator, 1, 3) != "BMS")
    Date <- substr(file1$Login, 1, 10)
    file2 <- cbind(file1, Date)
    file2$Login <- chron(times = substr(strptime(substr(file2$Login,12, 22), "%I:%M:%S %p"), 12, 19))
    file2$Logout <- chron(times = substr(strptime(substr(file2$Logout,12, 22), "%I:%M:%S %p"), 12, 19))
    file2_ranked <- file2[order(file2[,2]),]
    file2_ranked_ranked <- file2_ranked[order(file2_ranked[,1]),]
    rownames(file2_ranked_ranked) <- NULL
    
    name_vector <- unique(file2_ranked_ranked[,1])

    all <- data.frame()
    data <- data.frame()
    
    for (name in name_vector){
        
        advisor <- subset(file2_ranked_ranked, Operator == name)
        
        if (nrow(advisor) >= 2){
            Break <-c("00:00:00")
            for (i in 1:(nrow(advisor)-1)){
                
                if (chron(times =advisor[i+1, "Login"]) >= chron(times = advisor[i, "Logout"])){
                    
                    time <- as.character(chron(times =advisor[i+1, "Login"]) - chron(times = advisor[i, "Logout"]))
                    Break <- c(Break, time)
                    Break <- chron(times = Break)
                }
                
                else {
                    time <- "00:00:00"
                    Break <- c(Break, time)
                    Break <- chron(times = Break)
                }
                
            } 
            
            advisor_new <- data.frame(advisor, Break)
            
            # create a one-row matrix the same length as advisor_new
            temprow <-matrix(c(rep.int(NA, 6)), nrow = 1, ncol = 6)
            
            # make it a data.frame and give cols the same names as data
            newrow <- data.frame(temprow)
            colnames(newrow) <- colnames(advisor_new)
            
            newrow$Operator <- "Summary"
            newrow$Login <- advisor_new[1, "Login"]
            newrow$Logout <- advisor_new[nrow(advisor_new), "Logout"]
            newrow$Session.Length <- as.character(sum(chron(times = advisor_new$Session.Length)))
            newrow$Date <- advisor_new[1, "Date"]
            newrow$Break <- sum(chron(time = advisor_new$Break))
            
            data <- rbind(advisor_new,newrow)
            # new_data_frame <-rbind(all, data)
            
            # without this step, time becomes decimal numbers when rbind all.
            # as.character(new_data_frame)
            
            all <-rbind(all, data)
            
        } #close for the 1st if
        
        else {
            Break <-c("00:00:00")
            Break <- chron(times = Break)
            advisor_new <- data.frame(advisor, Break)
            
            # create a one-row matrix the same length as advisor_new
            temprow <-matrix(c(rep.int(NA, 6)), nrow = 1, ncol = 6)
            
            # make it a data.frame and give cols the same names as data
            newrow <- data.frame(temprow)
            colnames(newrow) <- colnames(advisor_new)
            
            newrow$Operator <- "Summary"
            newrow$Login <- advisor_new[1, "Login"]
            newrow$Logout <- advisor_new[nrow(advisor_new), "Logout"]
            newrow$Session.Length <- as.character(sum(chron(times = advisor_new$Session.Length)))
            newrow$Date <- advisor_new[1, "Date"]
            newrow$Break <- sum(chron(time = advisor_new$Break))
            
            data <- rbind(advisor_new,newrow)
            
            # new_data_frame <-rbind(all, data)
            
            # without this step, time becomes decimal numbers when rbind all.
            # as.character(new_data_frame)
            
            all <-rbind(all, data)
        }
    } 
    
    write.csv(all, "breaks.csv", row.names=FALSE)
}