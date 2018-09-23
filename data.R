library(tidyverse)
library(plotly)

## Create plant locations

regions <- c("EU", "USA")
numbers <- c(2, 3)

plants <- unlist(sapply(1:length(regions), function(x) rep(regions[x], numbers[x])))
ids <- unlist(sapply(1:length(regions), function(x) seq_len(numbers[x])))
plants <- paste0(plants, ids)


## Create dates
start <- 2015
end <- 2021

year <- seq(start, end)
month <- seq(1:12)
month <- rep(month, length(year))
year <- rep(year, each = 12)
dates <- data.frame(month, year)

## Generate data
createVals <- function(id, nulls = 0, dates, noise = TRUE, avg = 0, sd = 0) {
  n <- dim(dates)[1]
  start <- sample(c(0,1), 1) * noise
  end <- runif(1, 3, 4) * noise + (!noise) * 4
  incr <- (end - start) / n
  value <- sapply(1:(n - nulls * 12), function(x) start + x * incr + noise * rnorm(1, avg, sd = sd))
  value <- c(value, rep(NA, nulls * 12))
  id <- rep(id, n)
  df <- cbind(id, dates, value)
  return(df)
}

head(createVals(plants[1], 2, dates, 0.1))

data <- data.frame()
sdvals <- runif(length(plants), 0.1, 0.3)

for(i in 1:length(plants)) {
  data <- rbind(data, createVals(plants[i], 4, dates, sd = sdvals[i]))
}

data$value <- round(data$value, 1)

target <- createVals("Target", dates = dates, noise = FALSE)
data <- rbind(data, target)

data$date <- paste0("1/", data$month, "/", data$year)
data <- mutate(data, date = as.Date(date, format = "%d/%m/%Y"))
write.csv(data, "lara.csv")
p <- ggplot(data) +
  geom_line(aes(x = date, y = value, group = id, color = id)) +
  ylim(0, 4) +
  scale_y_continuous(name = "level", breaks=c(1, 2, 3, 4), labels = c("BASIC", "DEVELOPMENT",
                                                                         "MATURITY", "EXCELLENCE"))