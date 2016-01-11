##########################
#This is the R code used in order to analize the previous filtered data with the python script.
#The order of the code is the following:
#		· Define the functions that we will use in order 
#		  to modify some of the data and to get the csv outputs.
#		· Load the csv as data.frame().
#		· Get the variables where we crossed the data.
#		· Get the csv output for the visualization.
##########################





################     Functions    #############
#
#
	#This function will be used to round the Arrival and Departure times to hours o'clock.
	revtrunc <- function(x) { x - floor(x) } 

	#This function will be used to calculate the minimum, the mean and the maximum inside a tapply() function.
	multi.fun <- function(x) { 
	  c(min = min(x, na.rm = TRUE), mean = mean(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
	}

	# Functions to get the csv outputs
	    #Form a variable where we choose a column conditioned by other column
	    getcsv <- function(variable,header1,header2,outputname){
	      a<-data.frame( header1 = names(variable), header2 = variable)
	      names(a)<- c(header1,header2)
	      write.csv(a, file = outputname, row.names = FALSE)
	    }

	    #This kind of csv is adding two variables as used in "getcsv()", with the same row.names, 
	    # to get both variables in one csv.
	    appendcsv <- function(variable1,variable2,header1,header2,header3,outputname){
	      a<-data.frame( header1 = names(variable1), header2 = variable1)
	      names(a)<- c(header1,header2)
	      b<-data.frame(header3 = variable2)
	      names(b)<- c(header3)
	      row.names(b) <- NULL
	      a<-cbind(a,b)
	      write.csv(a, file = outputname, row.names = FALSE)
	      return(a)
	    }

	    #Example to get csv for a variable obtained from one column and two conditional columns
	    # cf_by_fromstate_byday is the name of the variable, "cf_by_fromstate_byday.csv" 
	    # is the name of the target output.
	  	write.csv(cf_by_fromstate_byday, file = "cf_by_fromstate_byday.csv", row.names = TRUE)

	  	#Function used to obtain a csv from the special variable where we use the function multi.func()
	    t_to_df_csv <- function(x,outputname,colname){
	      a<-rownames(x)
	      x <- as.data.frame(t(as.data.frame.list(x)))
	      x <- cbind(a, x)
	      row.names(x) <- NULL
	      names(x)[1] <- colname
	      write.csv(x, file = outputname, row.names = FALSE)
	    }
#
#
################     Functions    #############





################     Load csv and modifications of columns   #############
#
#
	#Load
	flightproj <- read.csv("filtered.csv", header = T, sep = ",", na.strings = "NA")

	#Add Date column to the data frame using the columns of Month Day and Year.
	flightproj$Date <- as.Date( paste(flightproj$Year, flightproj$Month , flightproj$DayofMonth , sep = "." )  , format = "%Y.%m.%d" )
	
	#Add Arrival Hour and Departure Hour columns, using revtrunc() to round the values
	flightproj$ArrH <- ifelse(revtrunc(flightproj$ArrTime/100)<0.3,floor(flightproj$ArrTime/100),ceiling(flightproj$ArrTime/100))
	flightproj$DepH <- ifelse(revtrunc(flightproj$DepTime/100)<0.3,floor(flightproj$DepTime/100),ceiling(flightproj$DepTime/100))
	
	#Convert the 00:00 to 24:00  in Arrival Hour and Departure Hour columns
	flightproj$ArrH[flightproj$ArrH==0] <- 24 
	flightproj$DepH[flightproj$DepH==0] <- 24 

	#Add velocity of flights taking in account the airtime and the distance (Km/h)
	flightproj$Airvel <- (flightproj$Distance/flightproj$AirTime)*60
#
#
################     Load csv and modifications of columns   #############






############### Generate variables #############
#
#
	#The number of days for each month
	day_by_month <- tapply(flightproj$DayofMonth,flightproj$Month,function(x){length(unique(x))})

	#Number of cancelled and not cancelled flights 
	# (by month, by day, by day of week, by arrival and departure time, 
	#	by arrival(to) and departure(from) state, by arrival(to) and departure(from) state for each day)
	
	f_by_month <-tapply(flightproj$Cancelled == 0, flightproj$Month, sum)
	cf_by_month <-tapply(flightproj$Cancelled == 1, flightproj$Month, sum)
	cf_by_month_perc <- cf_by_month/(cf_by_month+f_by_month)*100 # Percent of cancelations

	f_by_day <-tapply(flightproj$Cancelled == 0, flightproj$Date, sum)
	cf_by_day <-tapply(flightproj$Cancelled == 1, flightproj$Date, sum)

	f_by_dayofweek <-tapply(flightproj$Cancelled == 0, flightproj$DayOfWeek, sum)
	cf_by_dayofweek <-tapply(flightproj$Cancelled == 1, flightproj$DayOfWeek, sum)

	f_by_deptime <-tapply(flightproj$Cancelled == 0, flightproj$DepH, sum)
	cf_by_deptime <-tapply(flightproj$Cancelled == 1, flightproj$DepH, sum)

	f_by_arrtime <-tapply(flightproj$Cancelled == 0, flightproj$ArrH, sum)
	cf_by_arrtime <-tapply(flightproj$Cancelled == 1, flightproj$ArrH, sum)

	f_by_fromstate <-tapply(flightproj$Cancelled == 0, flightproj$STATE.FROM, sum)
	cf_by_fromstate <-tapply(flightproj$Cancelled == 1, flightproj$STATE.FROM, sum)

	f_by_tostate <-tapply(flightproj$Cancelled == 0, flightproj$STATE.TO, sum)
	cf_by_tostate <-tapply(flightproj$Cancelled == 1, flightproj$STATE.TO, sum)

	cf_by_fromstate_byday <-tapply(flightproj$Cancelled == 1, list(flightproj$STATE.FROM,flightproj$Date), sum)
	cf_by_tostate_byday <-tapply(flightproj$Cancelled == 1, list(flightproj$STATE.TO,flightproj$Date), sum)

	#Number of departure delays and arrival delays by day
	Depdelay_by_day <- tapply(flightproj$DepDelay, flightproj$Date, sum, na.rm = TRUE)
	Arrdelay_by_day <- tapply(flightproj$ArrDelay , flightproj$Date, sum, na.rm = TRUE)

	#Mean of departure delays and arrival delays (minutes) by day
	Depdelay_mean_by_day <- tapply(flightproj$DepDelay, flightproj$Date, mean, na.rm = TRUE)
	
	#Delay by departure state over flights
	Depdelay_mean_by_state_flights <- tapply(flightproj$DepDelay, flightproj$STATE.FROM, mean, na.rm = TRUE)/f_by_fromstate

	#Delay by  arrival state over flights
	Depdelay_mean_by_to_state_flights <- tapply(flightproj$ArrDelay, flightproj$STATE.TO, mean, na.rm = TRUE)/f_by_tostate

	#Minimum, mean and maximum Arrival and Departure delays by state and city
	arrdel_mmm_anual_state <- tapply(flightproj$ArrDelay, flightproj$STATE.TO, multi.fun)
	depdel_mmm_anual_state <- tapply(flightproj$DepDelay, flightproj$STATE.FROM, multi.fun)

	arrdel_mmm_anual_city <- tapply(flightproj$ArrDelay, flightproj$CODE.TO, multi.fun)
	depdel_mmm_anual_city <- tapply(flightproj$DepDelay, flightproj$CODE.FROM, multi.fun)

	#Flux(flights not cancelled / day) by month and state
	f_by_month_state <- tapply(flightproj$Cancelled == 0, list(flightproj$STATE.FROM,flightproj$Month),sum)
	flux_by_month_state <- apply(f_by_month_state,1,function(x){x/day_by_month})

	# Mean of velocity of flights by departure and arrival state
	airvel_mean_fromstate <-tapply(flightproj$Airvel[flightproj$Cancelled == 0], flightproj$STATE.FROM[flightproj$Cancelled == 0], mean, na.rm = TRUE)
	airvel_mean_tostate <-tapply(flightproj$Airvel[flightproj$Cancelled == 0], flightproj$STATE.TO[flightproj$Cancelled == 0], mean, na.rm = TRUE)

	# Mean of flight distances by state
	dist_mean_fromstate <- tapply(flightproj$Distance[flightproj$Cancelled == 0], flightproj$STATE.FROM[flightproj$Cancelled == 0], mean, na.rm = TRUE)
	dist_mean_tostate <-tapply(flightproj$Distance[flightproj$Cancelled == 0], flightproj$STATE.TO[flightproj$Cancelled == 0], mean, na.rm = TRUE)

	# Number of flights by origin and destination
	f_by_ori_dest <-data.frame(table(flightproj$Origin,flightproj$Dest))
	names(f_by_ori_dest)<-c("Origin","Dest","Flights")

	#Analysis of 5th of March and 6th of March 
	 # 5th march data.frame
		m5_from_to_state <- flightproj[flightproj$Date == c("2001-03-05"), c(18,20,22)]  
		#Analysis
		m5_f_by_fromstate <-tapply(m5_from_to_state$Cancelled == 0, m5_from_to_state$STATE.FROM, sum)
		m5_f_by_tostate <- tapply(m5_from_to_state$Cancelled == 0, m5_from_to_state$STATE.TO, sum)
	    m5_cf_by_tostate <- tapply(m5_from_to_state$Cancelled == 1, m5_from_to_state$STATE.TO, sum)
		m5_cf_by_fromstate <-tapply(m5_from_to_state$Cancelled == 1, m5_from_to_state$STATE.FROM, sum)

	 # 6th march data.frame
		m6_from_to_state <- flightproj[flightproj$Date == "2001-03-06", c(18,20,22)]
		#Analysis
		m6_f_by_fromstate <-tapply(m6_from_to_state$Cancelled == 0, m6_from_to_state$STATE.FROM, sum)
		m6_f_by_tostate <- tapply(m6_from_to_state$Cancelled == 0, m6_from_to_state$STATE.TO, sum)
		m6_cf_by_fromstate <-tapply(m6_from_to_state$Cancelled == 1, m6_from_to_state$STATE.FROM, sum)
		m6_cf_by_tostate <- tapply(m6_from_to_state$Cancelled == 1, m6_from_to_state$STATE.TO, sum)
#
#
############### Generate variables #############




###############    Get csv    ##############
#
#
    getcsv(cf_by_day, "day", "cancellations","cf_day.csv")
    getcsv(f_by_day, "day", "flights","f_day.csv")
    getcsv(cf_by_day/(cf_by_day+f_by_day), "day", "Cancel/Total","per_cf_by_day.csv") 
    
    getcsv(cf_by_arrtime, "Hour", "cancellations","cf_by_arrtime.csv")
    getcsv(f_by_arrtime, "Hour", "flights","f_by_arrtime.csv")
    getcsv(cf_by_arrtime/(cf_by_arrtime+f_by_arrtime), "Hour", "Cancel/Total","per_cf_by_arrtime.csv") 
    
    
    getcsv(cf_by_deptime, "Hour", "cancellations","cf_by_deptime.csv")
    getcsv(f_by_deptime, "Hour", "flights","f_by_deptime.csv")
    getcsv(cf_by_deptime/(cf_by_deptime+f_by_deptime), "Hour", "Cancel/Total","per_cf_by_deptime.csv")

    getcsv(cf_by_dayofweek, "day", "cancellations","cf_by_dayofweek.csv")
    getcsv(f_by_dayofweek, "day", "flights","f_by_dayofweek.csv")
    getcsv(cf_by_dayofweek/(cf_by_dayofweek+f_by_dayofweek), "day", "Cancel/Total","per_cf_by_dayofweek.csv") 
    
    getcsv(Depdelay_mean_by_state_flights, "state", "mean/flights","Depdelay_mean_by_state_flights.csv") 
    getcsv(Depdelay_mean_by_to_state_flights, "state", "mean/flights","Depdelay_mean_by_to_state_flights.csv") 
  
    getcsv(f_by_fromstate, "state", "flights","f_by_fromstate.csv")
    getcsv(f_by_tostate, "state", "flights","f_by_tostate.csv")
    
    getcsv(cf_by_fromstate, "state", "cancellations","cf_by_fromstate.csv") 
    getcsv(cf_by_tostate, "state", "cancellations","cf_by_tostate.csv") 
    
    getcsv(cf_by_fromstate/(cf_by_fromstate+f_by_fromstate), "state", "Cancel/Total","per_cf_by_fromstate.csv") 
    getcsv(cf_by_tostate/(cf_by_tostate+f_by_tostate), "state", "Cancel/Total","per_cf_by_tostate.csv")
    appendcsv(cf_by_fromstate/(cf_by_fromstate+f_by_fromstate),cf_by_tostate/(cf_by_tostate+f_by_tostate), "state", "Cancel_from/Total","Cancel_to/Total","per_cf_by_fromstate.csv")
    
    getcsv(m5_cf_by_fromstate/(m5_cf_by_fromstate+m5_f_by_fromstate), "state", "Cancel/Total","per_m5_cf_by_fromstate.csv")
    getcsv(m5_cf_by_tostate/(m5_cf_by_tostate+m5_f_by_tostate), "state", "Cancel/Total","per_m5_cf_by_tostate.csv")
    appendcsv(m5_cf_by_fromstate/(m5_cf_by_fromstate+m5_f_by_fromstate),m5_cf_by_tostate/(m5_cf_by_tostate+m5_f_by_tostate), "state", "Cancel_from/Total","Cancel_to/Total","per_m5_cf.csv")

    
    getcsv(m6_cf_by_fromstate/(m6_cf_by_fromstate+m6_f_by_fromstate), "state", "Cancel/Total","per_m6_cf_by_fromstate.csv") 
    getcsv(m6_cf_by_tostate/(m6_cf_by_tostate+m6_f_by_tostate), "state", "Cancel/Total","per_m6_cf_by_tostate.csv")
    appendcsv(m6_cf_by_fromstate/(m6_cf_by_fromstate+m6_f_by_fromstate),m6_cf_by_tostate/(m6_cf_by_tostate+m6_f_by_tostate), "state", "Cancel_from/Total","Cancel_to/Total","per_m6_cf.csv")
   
    getcsv(airvel_mean_tostate, "state", "mean_airvel(Km/h)","airvel_mean_tostate.csv")
    getcsv(airvel_mean_fromstate, "state", "mean_airvel(Km/h)","airvel_mean_fromstate.csv")
    
    getcsv(dist_mean_fromstate, "state", "mean_dist","dist_mean_tostate.csv")
    getcsv(dist_mean_tostate, "state", "mean_dist","dist_mean_fromstate.csv")    
    appendcsv(dist_mean_fromstate,dist_mean_tostate, "state", "Dist_from","Dist_to","dist_mean_by_state_fromto.csv")
    
    
    write.csv(cf_by_fromstate_byday, file = "cf_by_fromstate_byday.csv", row.names = TRUE)
    
    write.csv(cf_by_tostate_byday, file = "cf_by_tostate_byday.csv", row.names = TRUE)

    write.csv(f_by_ori_dest[f_by_ori_dest$Flights != 0, c("Origin","Dest","Flights")], file = "f_by_ori_dest.csv", row.names = FALSE)
  
   	write.csv(f_by_month_state, file = "f_by_month_state.csv", row.names = TRUE)
    
    t_to_df_csv(arrdel_mmm_anual_state,"arrdel_mmm_anual_state.csv","state")
    t_to_df_csv(depdel_mmm_anual_state,"depdel_mmm_anual_state.csv","state")
    
    t_to_df_csv(arrdel_mmm_anual_city,"arrdel_mmm_anual_city.csv","city")
    t_to_df_csv(depdel_mmm_anual_city,"depdel_mmm_anual_city.csv","city")
#
#
###############    Get csv    ##############