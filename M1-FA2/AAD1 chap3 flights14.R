# ========================================================================================================
# Purpose:      Rscript in AAD1 Chap 3 Flights14
# Author:       Chew C.H.
# DOC:          10 Sep 2014
# Topics:       Data Exploration, Data Summaries
# Packages:     data.table
# Data:         flights14.csv
#=========================================================================================================


library(data.table)

# Set a working directory to store all the related datasets and files.
setwd("D:/ds")

# Import using data.frame read.csv() function and check the time elapsed
system.time(flights.df <- read.csv("flights14.csv"))

# Import using data.table fread() function and check the time elapsed
system.time(flights.dt <- fread("flights14.csv"))

dim(flights.dt)

# Compare the data structure of flights.df vs flights.dt
class(flights.df)

class(flights.dt)


# Subset Rows ------------------------------------------------------------------
# df way:
jfk.jun.df <- subset(flights.df, origin == "JFK" & month == 6)

# Another df way: Need to use dataset name$col within [] and comma,
jfk.jun.df2 <- flights.df[flights.df$origin == "JFK" & flights.df$month == 6,]

identical(jfk.jun.df, jfk.jun.df2)

# dt way:
jfk.jun.dt <- flights.dt[origin == "JFK" & month == 6]


# df way: Remember the comma
flights.df[1:3,]


# dt way:
flights.dt[1:3]


# Sort by Columns -------------------------------------------------------------

# df way: Remember the dataset name$col and comma
# From Quick-R turorial: https://www.statmethods.net/management/sorting.html
ans.df <- flights.df[order(flights.df$origin, -flights.df$dest),]

# dt way:
ans.dt <- flights.dt[order(origin, -dest)]


# Select and rename columns ---------------------------------------------------
# df way: remember the quotation marks
ans.df <- flights.df[, c("arr_delay", "dep_delay")]
names(ans.df) <- c("delay_arr", "delay_dep")

# dt way:
ans.dt <- flights.dt[, .(delay_arr = arr_delay, delay_dep = dep_delay)]



# Question: How many trips have had total delay < 0? ---------------------------
# df way: Use two functions nrow() and subset()
nrow(subset(flights.df, (arr_delay + dep_delay) < 0))

# dt way: j can take expressions.
flights.dt[, sum((arr_delay + dep_delay) < 0)]


# Question: What is the average arrival and departure delay for all flights with "JFK" as the origin airport in the month of June?
# df way: subset and sapply
jfk.jun.delay.df <- subset(flights.df, origin == "JFK" & month == 6, 
                           select = c(arr_delay, dep_delay))
sapply(jfk.jun.delay.df, mean)

# dt way:
flights.dt[origin == "JFK" & month == 6, .(avg_arr_delay = mean(arr_delay), 
                                           avg_dep_delay = mean(dep_delay))]


# Question: How many trips have been made in 2014 from JFK airport in the month of June?
# df way:
nrow(subset(flights.df, origin == "JFK" & month == 6))

# dt way: .N is a special in-built variable that holds the number of obs in the group
flights.dt[origin == "JFK" & month == 6, .N]


# Question: Number of trips corresponding to each origin airport?
# df way:
summary(flights.df$origin)

# dt way:
flights.dt[, .N, by = origin]


# Question: total number of trips for each origin, dest pair for carrier code AA?
ans.df <- subset(flights.df, carrier == "AA", select = c(origin,dest))
table(ans.df$origin, ans.df$dest)
## Many zeros. Not a good output. Use data.table.

# dt way:
flights.dt[carrier == "AA", .N, by = .(origin,dest)]


# Question: average arrival, departure delay and number of flights for each orig, dest pair for each month for carrier code AA?
ans.dt <- flights.dt[carrier == "AA", .(avg.arr.delay = mean(arr_delay), avg.dep.delay = mean(dep_delay), .N), by = .(origin, dest, month)]

# Same as above and in addition, to sort results by the 3 grouping variables via keyby.
ans.dt <- flights.dt[carrier == "AA", .(avg.arr.delay = mean(arr_delay), avg.dep.delay = mean(dep_delay), .N), keyby = .(origin, dest, month)]


# Question: total number of trips for each origin, dest pair for carrier AA, and sort origin by ascending order and then dest by descending order
# Use data table chaining to avoid creating intermediate data strructures to hold temporary results.
flights.dt[carrier == "AA", .N, by = .(origin, dest)][order(origin, -dest)]


# Question: how many flights started late but arrived early (or on time), started and arrived late etc...
# by Grouping Variables can also be expressions.
flights.dt[, .N, .(dep_delay>0, arr_delay>0)]


# ========== END ===============================================================

