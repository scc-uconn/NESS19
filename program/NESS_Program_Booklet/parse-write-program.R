write_detailed_program <- TRUE
write_abstracts <- TRUE
write_posters <- TRUE
write_participants <- TRUE

outdir <- "tex/program/out"
outprogram <- file.path(outdir, "detailed-program.tex")
outabstracts <- file.path(outdir, "abstracts.tex")
outposters <- file.path(outdir, "posters.tex")
outparticipants <- file.path(outdir, "participants.tex")

library(magrittr)
library(dplyr)

library(readr)
library(stringr)
library(stringi)
library(stringdist)
library(tools)

capitalizeName <- function(s) {
    s %>%
        tolower %>%
        toTitleCase %>%
        gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", ., perl=TRUE) %>%
        gsub("De ", "de ", .) %>% 
        gsub("Eric Mick, Scd", "Eric Mick, ScD", ., fixed = TRUE) %>%
        gsub("John J Ragland Ii", "John J Ragland II", ., fixed = TRUE)
}

capitalize <- function(s) {
    s %>%
        gsub("&", "\\\\&", .) %>%
    toTitleCase %>%
    gsub("university", "University", .) %>%
    gsub("Xiao-Li MENG", "Xiao-Li Meng", .) %>%
    gsub("tennessee", "Tennessee", .) %>%
    gsub("Sequential Methods for 5g Wireless Communications", "Sequential Methods for 5G Wireless Communications", .)
}

escape_text <- function(s) {
  if(!grepl("\r\n\r\n", s)) {
    s <- gsub("\r\n", "\r\n\r\n", s)
  }
  s %>% 
    gsub("&", "\\\\&", .) %>%
    gsub("\\\\%", "%", .) %>%
    gsub("%", "\\\\%", .) %>% 
    gsub("\f", "", .) %>% 
    gsub("qualitative factors}", "qualitative factors", .) %>% 
    ## capture the invalid strings
    # gsub("\\[", "$[", .) %>%
    # gsub("\\]", "]$", .) %>%
    gsub("$$", "$", ., fixed = TRUE) %>% 
    gsub("\016", "", .) %>% det_math %>%
    gsub("π", "$\\\\pi$", .) %>%
    gsub("±", "$\\\\pm$", .) %>%
    gsub("≥", "$\\\\le$", .) %>%
    gsub("―", "-", .) %>%
    gsub("\"", "''", .) %>% 
    strsplit(., "(\r\n|\n\r|\r|\n)") %>% .[[1]]
}

det_math <- function(s) {
  output <- NULL
  for(x in s) {
    if(!grepl("$", x, fixed = TRUE)) {
      str_reg <- strsplit(x, " ")[[1]]
      rep_str <- str_reg[which(str_reg %>% {(grepl("(_|\\^)", .) | grepl("\\\\sqrt", .)) & ! grepl("$", ., fixed = TRUE)})]
      for(r in rep_str) {
        x <- gsub(r, paste0("$", r, "$"), x, fixed = TRUE)
      }
    }
    output <- c(output, x)
  }
  gsub("$ $", " ", output, fixed = TRUE)
}

escape_str <- function(s) {
  s %>% 
  stri_escape_unicode(.)
}

## Load CSV of abstract submissions

indata <-
    read_csv("bak/abstract.csv")

## Filter out abstracts
df <-
    indata %>%
    filter(!is.na(session))
n <- nrow(df)

# ## Replace some old names
# replace <- list("Bayesian Applications in High-dimensional and Multivariate Modeling (Song)"
#                 = "Song, Bayesian Applications in High-dimensional and Multivariate Modeling")
# 
# for (r in names(replace)) {
#     df$session[df$session == r] <- replace[[r]]
# }

## First regex matches sessions, second matches posters
abstract_regexes <- c("NESS19-IS-{0-9, }", "NESS19-PS")
matches <- vector("list", length(abstract_regexes))

## Loop over all abstracts
for (i in 1:n) {
    a <- df[i,]

    ## Check which regex---session, poster
    re <- sapply(abstract_regexes, . %>% grepl(., a$session))
    if (!any(re)) stop(sprintf("There was no match for the session \"%s\"", a$session))
    
    ix <- which(re)
    if (length(ix) > 1) stop(sprintf("Session \"%s\" matched multiple regexes", a$session))
    
    ## Record regex index
    matches[[ix]] <- c(matches[[ix]], i)
}
names(matches) <- c("Paper", "Poster")

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

# d_schd.1 <- read_csv("bak/scheduling.csv")
# d_schd.1 <- d_schd.1[order(d_schd.1$id), ]
d_schd <- read_csv("bak/SessionProposal.csv")
d_schd <- within(
  d_schd,
  organizer <- ifelse(is.na(organizer), proposer, organizer))
d_schd <- within(d_schd, title <- tools::toTitleCase(trimws(title)))

schd <- d_schd[with(d_schd, id < 100),
                 c("id", "affiliation", "organizer", "chair", "title")]
schd <- schd[order(schd$id), ]
schd <- 
within(schd, {
  # chair <- tools::toTitleCase(
  #   ifelse(is.na(chair) | toupper(chair) %in% c("TBD", "TBA"), organizer, chair)
  # )
  id <- paste0("NESS19-IS-", id)
  }
)

## match details of session with the time schedule
se_schd <- 
lapply(se_schd, function(input) {
  output <- cbind(input, schd[schd$id %in% input[, 3], c("affiliation", "chair")])
  colnames(output)[1:3] <- c("title", "organizer", "id")
  output[, c("id", "title", "affiliation", "organizer", "chair")]
})

## Match abstract to session

df_papers <- df[matches$Paper,]

## Output latex

## Detailed program booklet with a listing of each
lines_program <- ""
sessions <- 
lapply(1:length(se_schd), 
       function(ind) {
         time <- strsplit(names(se_schd)[ind], ",")[[1]][-1]
         list(l = se_schd[[ind]],
              label = paste0("Parallel Sessions:", time[1], ",", time[2]))
       })

## order of speakers
abs_order <- read_csv("bak/order_discussant.csv")
## read room information
room <- read_csv("bak/room_assign.csv")
s_num <- 1
for (timeslot in sessions) {
    l <- as.matrix(timeslot$l)
    lines_program <- c(lines_program, sprintf("\\phantomsection \\subsection*{%s}\n\\addcontentsline{toc}{subsection}{%s}", timeslot$label, timeslot$label), "")
    for (session in split(l, 1:nrow(l))) {
        names(session) <- colnames(timeslot$l)
        ## get room info for the session
        session_room <- room$room[which(room$id == session["id"])]
        # lines_program <- c(lines_program, sprintf("", session_room), "")
        if(all(session == split(l, 1:nrow(l))[[1]])) {
          lines_program <- c(lines_program, sprintf("\\phantomsection \\vbox{\\emph{Location: %s} \\\\ \\subsubsection*{%s}}\n\\addcontentsline{toc}{subsubsection}{%s}", 
                                                    session_room, paste(s_num, session["title"], sep=". "), session["title"]))
        } else{
          lines_program <- c(lines_program, sprintf("\\phantomsection \\vspace{16pt}\\vbox{\\emph{Location: %s} \\\\ \\subsubsection*{%s}}\n\\addcontentsline{toc}{subsubsection}{%s}", 
                                                    session_room, paste(s_num, session["title"], sep=". "), session["title"]))
        }
        lines_program <- c(lines_program, "")
        ## handle session chair
        if(is.na(session["chair"]) | session["chair"] == "TBD" | session["chair"] == "TBA") {
          session["chair"] <- session["organizer"]
        }
        lines_program <- c(lines_program, "\\begin{enumerate}[align=left]")
        orgchair <- grepl(toupper(session["organizer"]), toupper(session["chair"])) | 
          toupper(session["organizer"]) == toupper(session["chair"])
        if(orgchair) {
          if(is.na(session["affiliation"])) {
          lines_program <- c(lines_program, sprintf("\\item [\\emph{Organizer:}] \\textbf{%s}", 
                                                    capitalizeName(session["organizer"])))
          lines_program <- c(lines_program, sprintf("\\item [\\emph{Chair:}] \\textbf{%s}", 
                                                    capitalizeName(session["organizer"])))
          } else {
            lines_program <- c(lines_program, sprintf("\\item [\\emph{Organizer:}] \\textbf{%s}, %s", 
                                                      capitalizeName(session["organizer"]), capitalize(session["affiliation"])))
            lines_program <- c(lines_program, sprintf("\\item [\\emph{Chair:}] \\textbf{%s}, %s", 
                                                      capitalizeName(session["organizer"]), capitalize(session["affiliation"])))
          }
        } else {
          if(is.na(session["affiliation"])) {
            lines_program <- c(lines_program, sprintf("\\item [\\emph{Organizer:}] \\textbf{%s}", 
                                                      capitalizeName(session["organizer"])))
          } else {
            lines_program <- c(lines_program, sprintf("\\item [\\emph{Organizer:}] \\textbf{%s}, %s", 
                                                      capitalizeName(session["organizer"]), capitalize(session["affiliation"])))
          }
          chair_ext <- as.vector(str_split_fixed(session["chair"], ",", n = 2))
          if(chair_ext[2] == "") {
            lines_program <- c(lines_program, sprintf("\\item [\\emph{Chair:}] \\textbf{%s}", capitalizeName(session["chair"])))
          } else {
            lines_program <- c(lines_program, sprintf("\\item [\\emph{Chair:}] \\textbf{%s}, %s", capitalizeName(chair_ext[1]), chair_ext[2]))
          }
        }
        lines_program <- c(lines_program, "\\end{enumerate}", "")
        # lines_program <- c(lines_program, "")
        ## get speakers
        if(session["id"] == "NESS19-IS-41") {
          speakers <- df_papers[which(df_papers$session == "none"), ]
        } else {
          speakers <- df_papers[which(df_papers$session == session["id"]), ]
        }
        ## get the order of speakers
        order_abs <- abs_order[which(paste0("NESS19-IS-", abs_order$id) == session["id"]), ]
        
        if(is.na(order_abs$order)) {
          speakers <- speakers %>% arrange(gsub(".*\\s", "", presenter))
        } else {
          speakers <- speakers[rank(as.numeric(str_split(order_abs$order, "-")[[1]])), ]
          # print(speakers$id == as.numeric(str_split(order_abs$order, "-")[[1]]))
        }
        if(nrow(speakers) > 0) {
          lines_program <- c(lines_program, "\\begin{itemize}")
          for (k in 1:nrow(speakers)) {
            speaker <- speakers[k, ]
            lines_program <- c(lines_program, sprintf("\\item \\textbf{%s}, %s \\\\",
                                                      capitalizeName(speaker$presenter),
                                                      capitalize(speaker$affiliation)))
            lines_program <- c(lines_program, sprintf("%s", capitalize(toTitleCase(speaker$title))))
          }
          lines_program <- c(lines_program, "\\end{itemize}", "")
        } else {
          if(is.na(order_abs$Panelist)) {
            lines_program <- c(lines_program, "To Be Added", "")
          } else {
            panelists <-strsplit(order_abs$Panelist, "(\r|\n)")[[1]]
            lines_program <- c(lines_program, "\\begin{itemize}")
            for (panelist in panelists) {
              panelist <- gsub("&", "\\&", panelist, fixed=TRUE)
              if(length(strsplit(panelist, ";")[[1]]) == 3) {
                lines_program <- c(lines_program, sprintf("\\item \\textbf{%s}, %s \\\\",
                                                          strsplit(panelist, ";")[[1]][1],
                                                          strsplit(panelist, ";")[[1]][2]))
                lines_program <- c(lines_program, sprintf("%s", strsplit(panelist, ";")[[1]][3]))
              } else {
                lines_program <- c(lines_program, sprintf("\\item \\textbf{%s} \\\\",
                                                          strsplit(panelist, ";")[[1]][1]))
                lines_program <- c(lines_program, sprintf("%s", strsplit(panelist, ";")[[1]][2]))
              }

            }
            lines_program <- c(lines_program, "\\end{itemize}", "")
          }
        }
        if(!is.na(order_abs$discussant)) {
          lines_program <- c(lines_program, sprintf("\\emph{Discussant:} %s", order_abs$discussant), "")
        }
        s_num <- s_num + 1
    }
}

lines_program <- gsub("\\item [\\emph{Chair:}] \\textbf{Elizabeth Schifano}, University of Connecticut", 
                      "\\item [\\emph{Chair:}] \\textbf{Elizabeth Schifano}, University of \\\\Connecticut", lines_program, fixed = TRUE)

lines_program <- gsub("\\vspace{16pt}\\vbox{\\emph{Location: Nathan Hale South} \\\\ \\subsubsection*{18. Healthcare Data Analysis for Electronic Health Records}}", 
                      "\\vspace{16pt}\\vbox{\\emph{Location: Nathan Hale South} \\\\ \\subsubsection*{18. Healthcare Data Analysis for Electronic \\\\ Health Records}}", 
                      lines_program, fixed = TRUE)

if (write_detailed_program) {
    f <- file(outprogram)
    writeLines(lines_program, f)
    close(f)
}

## Write abstracts

lines_abstract <- ""
s_num <- 1
for (timeslot in sessions) {
    l <- as.matrix(timeslot$l)
    lines_abstract <- c(lines_abstract, sprintf("\\subsection*{%s}", timeslot$label), "")
    for (session in split(l, 1:nrow(l))) {
      names(session) <- colnames(timeslot$l)
      speakers <- df_papers[which(df_papers$session == session["id"]), ]
      ## get the order of speakers
      order_abs <- abs_order[which(paste0("NESS19-IS-", abs_order$id) == session["id"]), ]
      
      if(is.na(order_abs$order)) {
        speakers <- speakers %>% arrange(gsub(".*\\s", "", presenter))
      } else {
        speakers <- speakers[rank(as.numeric(str_split(order_abs$order, "-")[[1]])), ]
        # print(speakers$id == as.numeric(str_split(order_abs$order, "-")[[1]]))
      }
      
      if (nrow(speakers) > 0) {
        lines_abstract <- c(lines_abstract, sprintf("\\subsubsection*{%s}", paste(s_num, session["title"], sep=". ")), "")
        lines_abstract <- c(lines_abstract, "\\begin{itemize}")
        for (i in 1:nrow(speakers)) {
          p <- speakers[i, ]
          lines_abstract <- c(lines_abstract, sprintf("\\item \\textbf{%s}, %s",
                                                      capitalizeName(p$presenter),
                                                      capitalize(p$affiliation)), "")
          lines_abstract <- c(lines_abstract, sprintf("%s", capitalize(toTitleCase(p$title))), "")
          lines_abstract <- c(lines_abstract, sprintf("\\emph{\\footnotesize %s}", capitalize(escape_str(p$authorlist))), "")
          lines_abstract <- c(lines_abstract, escape_text(p$abstracttext), "")
        }
        lines_abstract <- c(lines_abstract, "\\end{itemize}", "")
      }
      s_num <- s_num + 1
    }
    
}

if (write_abstracts) {
    f <- file(outabstracts)
    writeLines(lines_abstract, f)
    close(f)
}

## Posters
df_posters <- df[matches$Poster,]
n_posters <- nrow(df_posters)
df_posters[which(df_posters$presenter=="John J Ragland II"), ]$presenter <- "John J Ragland"
df_posters <- df_posters %>% arrange(gsub(".*\\s", "", presenter))
df_posters[which(df_posters$presenter=="John J Ragland"), ]$presenter <- "John J Ragland II"

lines_poster <- ""
lines_poster <- c(lines_poster, "\\begin{enumerate}")
for (i in 1:n_posters) {
    p <- df_posters[i,]
    
    lines_poster <- c(lines_poster, sprintf("\\item \\textbf{%s}, %s \\\\", 
                                            capitalizeName(p$presenter), p$affiliation)
                      )
    lines_poster <- c(lines_poster, sprintf("%s", capitalize(toTitleCase(p$title))))
    # lines_poster <- c(lines_poster, sprintf("\\emph{%s}", capitalize(escape_str(p$authorlist)), ""))
}
lines_poster <- c(lines_poster, "\\end{enumerate}", "")

if (write_posters) {
    f <- file(outposters)
    writeLines(lines_poster, f)
    close(f)
}
# 
# save(sessions, morning, afternoon, schedule, file = "parsed.Rdata")

## List of participants

reg <- read_csv("bak/registration.csv")
reg <- reg[order(capitalizeName(reg$lastname), capitalizeName(reg$firstname)), ]

reg$name <- capitalizeName(paste(reg$firstname, reg$lastname))

participants <- reg$name

participants <-
    participants %>%
    gsub("De ", "de ", .) %>% 
    unique

if (write_participants) {
    f <-  file(outparticipants)
    writeLines(participants, f, sep = "\\\\\n")
    close(f)
}
