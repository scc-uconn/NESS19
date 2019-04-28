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

## generate html table
library(xtable)

time.abr <- unique(schd$time)

time <- c("May 16, 11:00-12:40", "May 16, 14:00-15:40", "May 16, 16:00-17:40", 
          "May 17, 08:30-10:10", "May 17, 10:30-12:10", "May 17, 13:10-14:50")

for(k in 1:length(time.abr)) {
  tmp <- subset(schd, time == time.abr[k])
  tmp <- tmp[with(tmp, order(id)), c("title", "organizer", "id")]
  tmp <- within(tmp, id <- paste0("NESS19-IS-", id))
  colnames(tmp) <- c("Session Title", "Organizer", "Session ID")
  tmp[, "Session Title"] <- tools::toTitleCase(tmp[, "Session Title"])
  tmp[, "Organizer"] <- tools::toTitleCase(tmp[, "Organizer"])
  print(paste0("#### ", time[[k]]))
  print.xtable(xtable(tmp), include.rownames = FALSE,
               include.colnames = TRUE, type = "html")
}
