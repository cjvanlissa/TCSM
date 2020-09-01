read_bb <- function(df = read.table("clipboard", sep = "\t")){
  head(df)
  df[c(1, 7, 8)] <- NULL
  df$V2 <- gsub("^.+?(\\d+).+?$", "\\1", df$V2)
  df <- df[df$V6 == "student", -5]
  names(df) <- c("id", "first", "last", "email")
  df
}

df <- read_bb()
exclude_students <- c("6167500", "4370422", "6870066", "4370422", "6870066", "7561652", "3334708")
all(exclude_students %in% df$id)
df <- df[!df$id %in% exclude_students, ]

group_size <- 4
ngroup <- nrow(df) %/% group_size
leftovers <- (nrow(df) %% group_size)

if(leftovers > 2){
  df$group <- sample(c(rep(1:ngroup, each = group_size), rep(ngroup+1, leftovers)))
} else {
  df$group <- sample(c(rep(1:ngroup, each = group_size), sample(1:ngroup, leftovers)))
}

tuts <- rep(c("McCool", "Schuurman"), each = floor(ngroup/2))
if((ngroup %% 2) > 0){
  tuts <- c(tuts, sample(c("McCool", "Schuurman"), 1))
}

df$tutorial <- sample(tuts, ngroup)[df$group]
colSums(table(df$group, df$tutorial))

openxlsx::write.xlsx(df, "C:/Users/lissa102/surfdrive/Shared/TCSM_2020/Exams_2020/groups.xlsx")
