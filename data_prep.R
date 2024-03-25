library(elastic)
library(ggplot2)
library(keras)
library(tidyverse)
library(lubridate)
library(zoo)

#querying the data from elasticsearch
qqq <- '{"sort": [{"@timestamp": {"order": "desc",
                            "unmapped_type": "boolean"}}],
   "fields": [{"field": "metadata.device.position"},
              {"field": "@timestamp","format": "date_time"}],
   "query": {"bool": {"must": [],
                      "filter": [{"bool": {"minimum_should_match": 1,
    "should": [{"match_phrase": {"metadata.device.position": "shower start"}},
               {"match_phrase": {"metadata.device.position": "shower stop"}}]}},
    {"range": {"@timestamp": {"format": "strict_date_optional_time",
                              "gte": "2022-04-01T00:00:00.457Z",
                              "lte": "2023-11-21T23:59:59.457Z"}}}],
                      "should": [],
                      "must_not": []}}
}'




# Connect to Elasticsearch

x <- connect(transport_schema="https",
             headers = list(Authorization = paste("ApiKey", readLines("apikey.txt"))),
             host="socialdiscoverylab.com/elasticsearch",
             port= NULL, ignore_version = TRUE)
s <- elastic::Search(x, body = qqq, asdf = TRUE, size = 10000,
                     index = "lorawan", source = FALSE)
shower_button <- s$hits$hits[, c("fields.@timestamp","fields.metadata.device.position")]
colnames(shower_button) <- c("timestamp", "status")


# Cleaning the data and converting timestamp to date-time format
shower_button <- tibble(timestamp=ymd_hms(unlist(strsplit(unlist(shower_button$timestamp),
                                                          ".000Z"))), 
                        status = unlist(shower_button$status))
sb_rev <- shower_button %>% map_df(rev) %>% mutate(status = (status == "shower start"))


# Cleaning data and definign rules ro correct wrong data
showers <- tibble(shower_start = POSIXct(), shower_stop = POSIXct())
remainder <- tibble(timestamp = POSIXct(), status = logical())
pos <- 1
nr <- nrow(sb_rev)
status_vector <-  sb_rev$status
timestamp_vector <-  sb_rev$timestamp
while(pos < nr){
  if(pos == nr -1){
    showers <- showers %>% add_row(shower_start = timestamp_vector[pos],
                                   shower_stop = timestamp_vector[pos+1])
    break
  } 
  if(!status_vector[pos+1] && status_vector[pos+2]){# STS
    showers <- showers %>% add_row(shower_start = timestamp_vector[pos],
                                   shower_stop = timestamp_vector[pos+1])
    pos <- pos +2
  }
  else{# here come the problematic parts
    if(!status_vector[pos+1]){# STTS
      gap <- difftime(timestamp_vector[pos+2],timestamp_vector[pos+1], units = "mins")
      if(gap > 30){
        showers <- showers %>% add_row(shower_start = timestamp_vector[pos],
                                       shower_stop = timestamp_vector[pos+1])
        pos <- pos + 3 #Skip second stop
      } else{ # small gap - assume double click of stop
        showers <- showers %>% add_row(shower_start = timestamp_vector[pos],
                                       shower_stop = timestamp_vector[pos+2])
        pos <- pos + 3 #Skip first stop 
      }
    } else if(status_vector[pos+2]){ #SSST
      gap <- difftime(timestamp_vector[pos+2],timestamp_vector[pos+1], units = "mins")
      if(gap > 30){ # most likely person pressed start instead of stop 
        showers <- showers %>% add_row(shower_start = timestamp_vector[pos],
                                       shower_stop = timestamp_vector[pos+1])
        pos <- pos + 2 
      } else{ 
        print(paste0("No rule defined for position ", pos, ". Rather skipping this.")) 
        pos <- pos + 4
      }
    } else if(!status_vector[pos+3]){ #SSTT
      gap1 <- difftime(timestamp_vector[pos+1],timestamp_vector[pos+0], units = "mins")
      gap2 <- difftime(timestamp_vector[pos+3],timestamp_vector[pos+2], units = "mins")
      if (gap1 > 30 && gap2 >30){ # middle ST is separated
        showers <- showers %>% add_row(shower_start = timestamp_vector[pos+1],
                                       shower_stop = timestamp_vector[pos+2])
        pos <- pos + 4
      }  else { # recognize fake signal combination here and real shower
        remainder <- remainder %>% bind_rows(sb_rev[pos + 0:3,])
        pos <- pos + 4
      }
    } else if(status_vector[pos+3]){ #SSTS
      gap <- difftime(timestamp_vector[pos+1],timestamp_vector[pos], units = "mins")
      if(gap > 30){ # random first start
        showers <- showers %>% add_row(shower_start = timestamp_vector[pos+1],
                                       shower_stop = timestamp_vector[pos+2])
        pos <- pos + 3 #skip first start
      } else{ #double start, take first I guess
        showers <- showers %>% add_row(shower_start = timestamp_vector[pos+0],
                                       shower_stop = timestamp_vector[pos+2])
        pos <- pos + 3 #skip first start
      }
    } else{
      print(paste0("No rule defined for position ", pos)) 
    }
  }
}

showers <- showers %>% mutate(diff = difftime(shower_stop,shower_start, units = "secs"))

# Plotting the data
ggplot(showers) + 
  annotate("rect", xmin = ymd_hms("2022-08-04T00:00:00"), xmax = ymd_hms("2022-09-15T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  annotate("rect", xmin = ymd_hms("2022-09-21T00:00:00"), xmax = ymd_hms("2022-11-17T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  annotate("rect", xmin = ymd_hms("2022-11-25T00:00:00"), xmax = ymd_hms("2022-11-30T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  annotate("rect", xmin = ymd_hms("2023-01-09T00:00:00"), xmax = ymd_hms("2023-11-21T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  geom_vline(xintercept = ymd_hms("2022-05-16T00:00:00"), col=3) +
  geom_point(aes(x=shower_start, y=unclass(diff))) + 
  scale_x_datetime(date_breaks = "4 month", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 1200, 100)) +
  labs(x = "Date", y = "Shower Duration (s)") +
  ggtitle("Shower duration over time")



filteredshowers <- showers[320 > showers$diff & 250 < showers$diff,]
model <- lm(diff~shower_start, data = filteredshowers)

for(i in seq_len(nrow(remainder)/4)){
  dddd <- remainder[(1+(i-1)*4):(4*i), ]
  target <- mean(predict(model, data.frame(shower_start=dddd$timestamp))) 
  diffs <- outer(dddd[c(3,4),]$timestamp,dddd[c(1,2),1]$timestamp, FUN = difftime, "secs")
  errorrrr <- abs(diffs - target)
  sol <- which.min(errorrrr)
  if(errorrrr[sol]<10){
    if(sol==1L || sol == 4L){
      showers <- showers %>% add_row(shower_start = dddd[1,]$timestamp,
                                     shower_stop = dddd[3,]$timestamp, diff = diffs[1,1])
      showers <- showers %>% add_row(shower_start = dddd[2,]$timestamp,
                                     shower_stop = dddd[4,]$timestamp, diff = diffs[2,2])
    } else{
      showers <- showers %>% add_row(shower_start = dddd[1,]$timestamp,
                                     shower_stop = dddd[4,]$timestamp, diff = diffs[2,1])
      showers <- showers %>% add_row(shower_start = dddd[2,]$timestamp,
                                     shower_stop = dddd[3,]$timestamp, diff = diffs[1,2])
    }
  }
  # else: rather skip it
}

showers <- showers[order(showers$shower_start),]


cleaned_showers <- showers[showers$diff > predict(model,showers)+10 | showers$diff < predict(model,showers)-10,]
cleaned_showers <- cleaned_showers[cleaned_showers$diff > 150 & cleaned_showers$diff < 1000,]
cleaned_showers <- cleaned_showers %>% mutate(inter = interval(shower_start, shower_stop))


ggplot(cleaned_showers) + 
  annotate("rect", xmin = ymd_hms("2022-08-04T00:00:00"), xmax = ymd_hms("2022-09-15T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  annotate("rect", xmin = ymd_hms("2022-09-21T00:00:00"), xmax = ymd_hms("2022-11-17T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  annotate("rect", xmin = ymd_hms("2022-11-25T00:00:00"), xmax = ymd_hms("2022-11-30T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  annotate("rect", xmin = ymd_hms("2023-01-09T00:00:00"), xmax = ymd_hms("2023-11-21T23:59:59"), ymin = -Inf, ymax = Inf, fill="green", colour = "black", alpha = 0.1) +
  geom_vline(xintercept = ymd_hms("2022-05-16T00:00:00"), col=3) +
  geom_point(aes(x=shower_start, y=unclass(diff))) + 
  scale_x_datetime(date_breaks = "4 month", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 1200, 100)) +
  labs(x = "Date", y = "Cleaned Shower Duration (s)") +
  ggtitle("Cleaned shower duration over time")


I1 <- interval(ymd_hms("2022-08-04T00:00:00"), ymd_hms("2022-09-15T23:59:59"))
I2 <- interval(ymd_hms("2022-09-21T00:00:00"), ymd_hms("2022-11-17T23:59:59"))
I3 <- interval(ymd_hms("2022-11-25T00:00:00"), ymd_hms("2022-11-30T23:59:59"))
I4 <- interval(ymd_hms("2023-01-09T00:00:00"), ymd_hms("2023-11-21T23:59:59"))

ints = c(I1,I2,I3,I4)
cleaned_showers <- cleaned_showers %>% mutate(whichinter = rowSums(sapply(1:4, 
                                                                          function(i) i*int_overlaps(cleaned_showers$inter,ints[i]))))

diff <- 20

start = int_start(I4)
interval_index <- (1:ceiling(int_length(I4)/diff))
intervals <- interval(start + seconds(diff*(interval_index-1)),
                      start + seconds((diff-1) +diff*(interval_index-1)))
whe <- sapply(filter(cleaned_showers, whichinter == 4L)$inter, int_overlaps, intervals)
database4 <- tibble(intervals = intervals, shower  = (rowSums(whe)==1))
rm(whe)





qI4 <- '{
  "sort": [{"@timestamp": {"order": "asc",
                           "unmapped_type": "boolean"}}],
  "fields": [{"field": "sensor.humidity"},
             {"field": "sensor.soundavg"},
             {"field": "sensor.temperature"},
             {"field": "sensor.soundpeak"},
             {"field": "@timestamp", "format": "strict_date_optional_time"}],
  "_source": false,
  "query": {"bool": {"must": [],
                     "filter": [{"range": {"@timestamp": {"format": "strict_date_optional_time",
                                                          "gte": "2023-01-09T00:00:00.457Z",
                                                          "lte": "2023-11-21T23:59:59.457Z"}}},
                                {"bool": {"must": [],
                                          "filter": [{"match_phrase": {"metadata.device.location": "sara_dolnicar"}},
                                                     {"bool": {"should": [{"bool": {"must": [],
                                                                                    "filter": [{"exists": {"field": "sensor.humidity"}}],
                                                                                    "should": [],
                                                                                    "must_not": []}},
                                                                          {"bool": {"must": [],
                                                                                    "filter": [{"exists": {"field": "sensor.temperature"}}],
                                                                                    "should": [],
                                                                                    "must_not": []}},
                                                                          {"bool": {"must": [],
                                                                                    "filter": [{"exists": {"field": "sensor.soundpeak"}}],
                                                                                    "should": [],
                                                                                    "must_not": []}},
                                                                          {"bool": {"must": [],
                                                                                    "filter": [{"exists": {"field": "sensor.soundavg"}}],
                                                                                    "should": [],
                                                                                    "must_not": []}}],
                                                              "minimum_should_match": 1}}],
                                          "should": [],
                                          "must_not": []}}],
                     "should": [],
                     "must_not": []}}
}'



get_data <- function(qqqq){
  s <- elastic::Search(x, body = qqqq, asdf = TRUE, size = 10000, index = "lorawan")
  total <- s$hits$total$value
  runs <- ceiling(total/10000)-1
  ask <- readline(prompt = paste0(runs, " runs. You sure? (1 = yes, 0 = no) \n"))
  dat <- s$hits$hits
  last_sort <- dat$sort[[10000]] 
  res <- list()
  if(ask == "1"){
    for(i in (1:runs)){
      print(paste0(i,", ", runs))
      v <- unlist(strsplit(qqqq,"}"))
      v[length(v)] <- paste0(",\n \"search_after\": [",last_sort,"]")
      qqqq2 <- paste0(paste(v, collapse = "}"), "}")
      res[[i]] <- elastic::Search(x, body = qqqq2, asdf = TRUE, size = 10000, index = "lorawan")$hits$hits
      last_sort <- res[[i]]$sort[[nrow(res[[i]])]]
    }
  } else{
    return(0)
  }
  dat <- c(list(dat), res)
  do.call(rbind, dat)
}


clean_up <- function(dat){
  ts <- ymd_hms(unlist(strsplit(unlist(dat$`fields.@timestamp` ),".000Z")))
  sensors <- colnames(dat)[startsWith(colnames(dat), "fields.sensor")]
  sensors <- sort(sensors)[c(1,4,2,3)]
  dat <- dat[, sensors]
  dat <- dat %>% replace(. == "NULL", NA)
  cleaned <- sapply(sensors , function(x) na.approx(unlist(dat[,x]), rule = 2))
  tibble(data.frame(timestamp = ts, cleaned))
}

match_this <- function(database, ddd, rollmean = TRUE, k = 7, smooth = FALSE,
                       smoother = 1/200, tzone = "Australia/Brisbane"){
  d <- dim(database)[1]
  db <- matrix(NaN, ncol = 4, nrow = d)
  colnames(db) <- colnames(ddd)[-1]
  ddd <- ddd %>% mutate(include = findInterval(timestamp, int_start(database$intervals))) %>% 
    select(-timestamp) %>% group_by(include) %>% summarise_all("mean", na.rm = TRUE)
  inc <- ddd$include
  ddd <- ddd %>% select(- include)
  db[inc,] <- as.matrix(ddd)
  db <- na.approx(db, rule = 2)
  if(rollmean){
    ix <- floor((1 + k)/2):ceiling(d - k/2)
    db <- rollmean(db, k=k)
    database <- database[ix,]
  }
  if(smooth) db <- apply(db,2, function(x) lowess(x,  f = smoother, delta = 0.20 * diff(range(x)))$y)
  database <- database %>% mutate(intervals = interval(with_tz(int_start(intervals), tzone = tzone), 
                                                       with_tz(int_end(intervals), tzone = tzone)))
  as_tibble(data.frame(database, db))
}


d4 <- get_data(qI4)  
d4 <- clean_up(d4)
database4 <- match_this(database4, d4, k=13)
#save(database4, file = "database4_20s.Rda")




                                                                                                                                                                                  