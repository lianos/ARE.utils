stockHistory <- function(stock, start.date, end.date=Sys.Date()) {
  ## msft <- stockHistory('MSFT', '2006-01-12')
  ## See: http://blog.revolution-computing.com/2009/11/charting-time-series-as-calendar-heat-maps-in-r.html
  query <- paste("http://ichart.finance.yahoo.com/table.csv?s=",
                 stock,
                 "&a=", substr(start.date,6,7),
                 "&b=", substr(start.date, 9, 10),
                 "&c=", substr(start.date, 1,4), 
                 "&d=", substr(end.date,6,7),
                 "&e=", substr(end.date, 9, 10),
                 "&f=", substr(end.date, 1,4),
                 "&g=d&ignore=.csv", sep="")
  # cat("Querying:", query, "\n")
  read.csv(query, as.is=TRUE)
}

plotStocks <- function(stock.history) {
  stock.data <- transform(stock.history, week=as.POSIXlt(Date)$yday %/% 7 + 1,
                         wday=as.POSIXlt(Date)$wday,
                         year=as.POSIXlt(Date)$year + 1900)
  qplot(week, Adj.Close, data=stock.data, colour=factor(wday), geom="line") + 
        facet_wrap(~ year, ncol=1)
  # invisible(stock.data)
}
