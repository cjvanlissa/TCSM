tmp <- read.table("clipboard", sep = "\t")[, -c(1, 7, 8)]
names(tmp) <- c("number", "first", "last", "email", "role")
tmp$number <- gsub("[a-z: ]", "", tolower(tmp$number))
tmp <- tmp[tmp$role == "student", ]
tmp[["role"]] <- NULL
filnam <- paste0("students_", format(Sys.Date(), "%Y"), ".csv")
write.csv(tmp, filnam, row.names = FALSE)
usethis::use_git_ignore(filnam)
