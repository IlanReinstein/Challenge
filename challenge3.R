require(plyr)
require(dplyr)
require(ggplot2)
require(reshape)


flights <- read.csv('flights2.csv',header = T)

flights <- arrange(flights, year, month, airport_name, carrier_name)

airlines <- ddply(flights, year ~ carrier + airport, summarise, 
                  carrier_ratio = carrier_delay/arr_delay,
                  weather_ratio = weather_delay/arr_delay,
                  nas_ratio = nas_delay/arr_delay,
                  security_ratio = security_delay/arr_delay,
                  late_ratio = late_aircraft_delay/arr_delay)

airlines2 <- filter(airlines, year == 2015, airport == 'ATL' | airport == 'LAX' | airport == 'MCO', carrier == 'WN')

airlines_melt <- melt(airlines2, id = c('year','carrier', 'airport'))


g <- ggplot(airlines_melt, aes(x = airport, fill = variable, y = value)) 
g <- g + geom_bar(stat = 'identity') + theme_bw() + labs(title = 'Southwest Air Delay per Airport', x = 'Airport', y = '% of Total Delay Time (Minutes)')
g <- g + scale_fill_brewer(name="Delay Type",
                           labels=c("Carrier", 
                                    "Weather", 
                                    "National Aviation System", 'Security', 'Late Arrivals'), palette = 'Set1')
g



airports <- filter(airlines, year == 2015, airport == 'ORD', carrier == 'MQ' | carrier == 'AA' | carrier == 'DL')
airport_melt <- melt(airports, id = c('year', 'airport','carrier'))


p <- ggplot(airport_melt, aes(x = carrier, fill = variable, y = value)) 
p <- p + geom_bar(stat = 'identity') + theme_bw() + labs(title = "Chicago O'Hare Delays per Carrier", x = 'Airline', y = '% of Total Delay Time (Minutes)')
p <- p + scale_fill_brewer(name="Delay Type",
                           labels=c("Carrier", 
                                    "Weather", 
                                    "National Aviation System", 'Security', 'Late Arrivals'), palette = 'Set1')
p
