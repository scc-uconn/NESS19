## library(gdata)
## schedule <- read.xls("scheduling.xlsx", sheet = 1, header = TRUE, as.is = TRUE)
schedule <- read.csv("scheduling.csv", header = TRUE, as.is = TRUE)
schedule <- within(
    schedule,
    organizer <- ifelse(organizer == "", proposer, organizer))

schedule <- within(schedule, title <- trimws(title, ))

schd <- schedule[with(schedule, id < 100),
                 c("id", "organizer", "title", "track", "time")]

schd <- schd[with(schd, order(time)), ]
