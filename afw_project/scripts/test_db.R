write("DEBUG TEST", file="C:/afw_project/logs/debug_test.txt")
library(DBI)
library(RMySQL)
log_file <- "C:/afw_project/logs/test_run.log"
log_msg <- function(msg) {
line <- paste(Sys.time(), "-", msg)
cat(line, "\n")
cat(line, "\n", file = log_file, append = TRUE)
}
log_msg("Starter test script")
con <- NULL
try({
con <- dbConnect(
RMySQL::MySQL(),
user = "root",
password = "Tfx58kzd.EK.Stud.dk",
host = "127.0.0.1",
dbname = "salling_afw",
port = 3306
)
log_msg("Forbindelse til MySQL lykkedes")
tables <- dbListTables(con)
log_msg(paste("Tabeller fundet:", paste(tables, collapse = ", ")))
}, silent = TRUE)
if (is.null(con)) {
log_msg("FEJL: Kunne ikke forbinde til databasen")
} else {
dbDisconnect(con)
log_msg("Lukker forbindelsen igen")
}