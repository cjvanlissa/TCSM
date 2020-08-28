read_bb <- function(df = read.table("clipboard", sep = "\t")){
  head(df)
  df[c(1, 7, 8)] <- NULL
  df$V2 <- gsub("^.+?(\\d+).+?$", "\\1", df$V2)
  df <- df[df$V6 == "student", ]
  df
}

df <- read_bb()
group_size <- 4
ngroup <- nrow(df) %/% group_size
leftovers <- (ngroup %% group_size)

if(leftovers > 2){
  df$group <- sample(c(rep(1:ngroup, each = group_size), rep(ngroup+1, leftovers)))
} else {
  df$group <- table(sample(c(rep(1:ngroup, each = group_size), sample(1:ngroup, leftovers))))
}

write.csv("C:/Users/lissa102/surfdrive/Shared/TCSM_2020/Exams_2020/groups.csv", row.names = FALSE)
