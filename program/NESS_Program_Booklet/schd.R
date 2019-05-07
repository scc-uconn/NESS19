library(magrittr)
library(dplyr)

library(readr)
library(stringr)
library(stringi)
library(stringdist)
library(tools)

## Load invited sessions description
mdfile <- "bak/program.md"
raw <- readChar(mdfile, file.info(mdfile)$size)
md <-
    raw %>%
    ## Remove HTML
    gsub("<br />", "", .) %>%
    ## Split according to session
    strsplit("-----") %>%
    unlist %>%
    ## Remove header text
    .[-1]

## agenda
agenda <- md[1]

## detailed schedule
schedule <- md[2]
schedule <- 
  schedule %>% 
  strsplit('<h4 class=\"mb-3\">') %>% 
  unlist %>% trimws

## get time schedule
se_schd <- schedule[which(sapply(schedule, function(x) grepl("</table>", x)))]
se_time <- se_schd %>% strsplit('</h4>\n') %>% {lapply(., function(x) x[1])} %>% unlist
se_schd <- se_schd %>% strsplit('</h4>\n') %>% {
  lapply(., function(x) {
    x[-1] %>% strsplit('<td>') %>% .[[1]] %>% 
      gsub("(<(/td|tr|/tr)>|(\n|\t))", "", .) %>% 
      gsub("</table>", "", .) %>% trimws %>%
      .[-1] %>% matrix(ncol = 3, byrow = TRUE)
  })
}
names(se_schd) <- se_time

room <- read_csv("bak/room_assign.csv")
se_schd <- lapply(se_schd, 
                  function(x) {
                    cbind(x, room$room[match(x[, 3], room$id)])
                  })

## generate html table
library(xtable)
time <- names(se_schd)
for(k in 1:length(se_schd)) {
  timeslot <- se_schd[[k]]
  colnames(timeslot) <- c("Session Title", "Organizer", "Session ID", "Room")
  cat("\n\n")
  print(paste0("#### ", time[k]))
  print.xtable(xtable(timeslot[, -3]), include.rownames = FALSE,
               include.colnames = TRUE, type = "html")
}
