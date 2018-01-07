## script to analyze wells fargo data
## pre-req: 1. export the transactions from all 3 accounts in csv, 2. add an extra column identifying the
## last 4 digits of the account, 3. collate all the records of each account in one file, 4. create a header with a legend of:
## Account	Date	Transaction	JunkRow01	CheckNumber	Description
## export this file as csv and name it wfs-Table.csv (or change the name of the input file below)
## 
## I only keep the script here, all the data is obviously not uploaded to git, and is kept locally
## I symlink to this file over to the local data directory..
##    Omer Ansari 12.9.17

library(tidyverse)
library(lubridate)

wfsTransactions <- read_csv("WFS-Table.csv",
                            col_types = cols(
                              .default = col_character(),
                              Date = col_date(format = "%m/%d/%Y"),
                              Transaction = col_double()
                              )
                            )
# creating new columns for year, month and day for easier manipulation
wfsTransactions$Year <- year(wfsTransactions$Date)
wfsTransactions$Month <- month(wfsTransactions$Date)
wfsTransactions$Day <- day(wfsTransactions$Date)


### ANALYSIS #1 : were we net positive or negative during the aforementioned month.

# this function drops transactions below 5% or 95% quantile in order to print graphs 
# without extreme values skewing the graph. (it actually replaces the value with N/A, 
# which are then omitted by ggplot, which tells us how many it omitted: 
# e.g "Removed 25 rows containing missing values (geom_point)" and this is telling
# since in this example thats a lot of transactions its chucking out

low_percentile <- 0.01
high_percentile <- 0.996

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(low_percentile, high_percentile), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# show points and cash flow, this is not that useful as the outliers must be printed here, and they really skew
# the plot
TransactionsAndCashFlowCOMBINEDGraph <- ggplot(data = wfsTransactions, mapping = aes(x = Month, y = Transaction)) +
  geom_point() +
  geom_smooth()


allTransactionsPlot <- ggplot(data = wfsTransactions) +
  geom_point(mapping = aes(x = Month, y = Transaction))

#allTransactionsPlot

wfsTransactionsOutliersRemoved <- wfsTransactions

wfsTransactionsOutliersRemoved$Transaction <- remove_outliers(wfsTransactionsOutliersRemoved$Transaction)

TransactionsWithOutliersRemovedPlot <- ggplot(data = wfsTransactionsOutliersRemoved) +
  geom_point(mapping = aes(x = Month, y = Transaction))


TransactionsWithOutliersRemovedPlot

# actions:
# - need to convert this to RMD
# - print complete graph
# - print the transaction graph with outliers removed
# - print the actual % of upper and lower percentile removed, and the description and amount of the outliers
# - do a regular expression on GAP and CAPITAL ONE transactions, put them as a separate column and plot 
# different color for those
# - print a curve showing trend of positive vs negative cumulative sum:


CashFlowGraph <- ggplot(data = wfsTransactions) +
  geom_smooth(mapping = aes(x = Month, y = Transaction))

#CashFlowGraph



# doesn't work with y
# BarCashFlowGraph <- ggplot(data = wfsTransactions) +
#  geom_bar(mapping = aes(x = Month, y = Transaction))

#BarCashFlowGraph

# need to create a new tibble with 12 rows (12 months), and a cum sum of all values for each month
