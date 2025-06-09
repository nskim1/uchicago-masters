# load data
data <- read_csv("data.csv", show_col_types = FALSE)

# convert data to time series objects
jobs <- ts(data$jobs, start=c(2000,12), frequency=12)
sentiment <- ts(data$sentiment, start=c(2000,12), frequency=12)
retail <- ts(data$retail, start=c(2000,12), frequency=12)

# merge into single time series object
data <- ts(cbind(jobs, sentiment, retail), start=c(2000,12), frequency=12)

# focus on time between recessions (June 2009 through October 2019)
data <- window(data, start=c(2009,6), end=c(2019,10))

# plot jobs
autoplot(data[,'jobs']) + 
  ggtitle('Total Nonfarm Job Openings (Monthly, U.S.)') + 
  ylab('Job Openings  (000s)')