library(foreign)
library(Benchmarking)
library(ggplot2)
library(dplyr)

## Variables: 
# Unternehmensbezeichnung		company
# Jahr		year
# id		id
# Beschäftigte Kopfzahl	employees	                                  x1
# Fahrzeug- bestand Stadt-/ Straßenbahn Fahrzeuge	number of trams	  x2
# Fahrzeug- bestand Bus inkl. Obus	number of busses	              x3
# Wagen- kilometer Stadt-/ Straßenbahn (in 1000 km)	vehicle km tram	y1
# Wagen- kilometer Bus inkl. Obus in 1000 km	vehicle km bus	      y2
# Platz- kilometer Stadt-/ Straßenbahn (in 1000 km)	seat km tram  	y3
# Platz- kilometer Bus inkl. Obus in 1000 km	seat km bus	          y4
# Average number of seats trams	number of seats tram              	y5
# Average number of seats bus	number of seats bus	                  y6
# Einwohner im Einfluss-gebiet	inhabitants	                        z1
# Strecken-länge Stadt-/ Straßenbahn in km	networklength tram	    z2
# Linienlänge Bus gesamt in km	networklength busses	              z3
# Fläche in km2	area size	                                          z4
# Zeittrend	time trend                                            	T

data <- read.dta("German_public_transport_data.dta", convert.dates = TRUE, convert.factors = TRUE,
                   missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE)

# trim whitespaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
companies = unique(trim(data$company))

data$company <- trim(data$company)

data_cologne <- data[data$company == 'Köln (KVB)',]
data_bonn <- data[data$company == 'Bonn (SWBV / SWB und SSB)',]

datalist = list()
datalist_avg = list()

# get length of bus/tram network in current year
tram_km = data$z2
bus_km = data$z3
  
# calc tram efficency for every company in cur year with dea
x_dea <- cbind(data$x1, data$x2)
efficencyValues_tram <- dea(x_dea,data$y3,ORIENTATION="in",RTS="crs")
efficencyValues_tram <- efficencyValues_tram$eff
  
# calc bus efficency for every company in cur year with dea
x_dea <- cbind(data$x1, data$x3)
efficencyValues_bus <- dea(x_dea,data$y4,ORIENTATION="in",RTS="crs")
efficencyValues_bus <- efficencyValues_bus$eff

# calc the total efficency of every company in current year weighted on lentgh of bus and tram network
efficencyValues = (efficencyValues_tram * tram_km + efficencyValues_bus * bus_km) / (tram_km + bus_km)

data$tramefficency <- efficencyValues_tram
data$busefficency <- efficencyValues_bus
data$totalefficency <- efficencyValues
  
company = data$company
year = data$year
inhabitans = data$z1
  
# calc adj efficencies based on inhabitans
data$adj_eff_tot <- data$totalefficency * data$z1
data$adj_eff_tram <- data$tramefficency * data$z1
data$adj_eff_bus <- data$busefficency * data$z1
  
# make table
eff_table <- cbind(company = data$company, year = data$year, inhabitans = data$z1, tram_efficency = data$tramefficency, bus_efficency = data$busefficency, total_efficency = data$totalefficency, adj_tram_eff = data$adj_eff_tram, adj_bus_eff = data$adj_eff_bus, adj_total_eff = data$adj_eff_tot)
eff_table <- data.frame(eff_table)

# extract Cologne/Bonn data
table_cologne <- eff_table[eff_table$company == 'Köln (KVB)',]
table_bonn <- eff_table[eff_table$company == 'Bonn (SWBV / SWB und SSB)',]
table_cleared <- eff_table[eff_table$company != 'Köln (KVB)',]
table_cleared <- table_cleared[table_cleared$company != 'Bonn (SWBV / SWB und SSB)',]

consid_years = unique(eff_table$year)
n = length(consid_years)

# go year by year
for (i in 1:n){
  
  table_cleared_cur_year <- table_cleared[table_cleared$year == consid_years[i],]
  
  table_cologne_cur_year <- table_cologne[table_cologne$year == consid_years[i],]
  table_bonn_cur_year <- table_bonn[table_bonn$year == consid_years[i],]

  # read efficieny values Cologne and Bonn
  eff_tram_cologne_cur_year <- as.numeric(as.character(table_cologne_cur_year$tram_efficency))
  eff_bus_cologne_cur_year <- as.numeric(as.character(table_cologne_cur_year$bus_efficency))
  eff_cologne_cur_year <- as.numeric(as.character(table_cologne_cur_year$total_efficency))
  eff_tram_bonn_cur_year <- as.numeric(as.character(table_bonn_cur_year$tram_efficency))
  eff_bus_bonn_cur_year <- as.numeric(as.character(table_bonn_cur_year$bus_efficency))
  eff_bonn_cur_year <- as.numeric(as.character(table_bonn_cur_year$total_efficency))
  
  
  # calc inhab total
  inhab_total <- table_cleared_cur_year$inhabitans
  inhab_total <- sum(as.numeric(as.character(inhab_total)))
  
  # calc adjusted efficency total
  adj_eff_total <- table_cleared_cur_year$adj_total_eff
  adj_eff_tram <- table_cleared_cur_year$adj_tram_eff
  adj_eff_bus <- table_cleared_cur_year$adj_bus_eff
  
  adj_eff_total <- sum(as.numeric(as.character(adj_eff_total)))
  adj_eff_tram_total <- sum(as.numeric(as.character(adj_eff_tram)))
  adj_eff_bus_total <- sum(as.numeric(as.character(adj_eff_bus)))
  
  # calc adjusted average efficency
  avg_adj_eff_cur_year = adj_eff_total / inhab_total
  avg_adj_eff_tram_cur_year = adj_eff_tram_total / inhab_total
  avg_adj_eff_bus_cur_year = adj_eff_bus_total / inhab_total
  
  # append to table
  cur_year <- as.numeric(as.character(consid_years[i]))
  table_all_avg <- cbind("Year"=cur_year, "Average total efficiency"=avg_adj_eff_cur_year, "Average tram efficiency"=avg_adj_eff_tram_cur_year, "Cologne total efficiency"=eff_cologne_cur_year, "Cologne tram efficiency"=eff_tram_cologne_cur_year, "Bonn total efficiency"=eff_bonn_cur_year, "Bonn tram efficiency"=eff_tram_bonn_cur_year)
  datalist_avg[[i]] <- data.frame(table_all_avg)
}

# bind list of tables to one table and save as csv
all_avg = do.call(rbind, datalist_avg)

table_hc_bonn <- cbind("Year"=data_bonn$year, "Employees Bonn"=data_bonn$x1, "Employees Cologne"=data_cologne$x1)

# write to csv excel plotting
write.table(table_hc_bonn, file = "head_counts_results.csv", sep=";", dec=",")
write.table(eff_table, file = "eff_results.csv", sep=";", dec=",")
write.table(all_avg, file = "avg_eff_results.csv", sep=";", dec=",")
