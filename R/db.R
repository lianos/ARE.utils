"
Utility functions to work with databases
"

createQueriesFromScript <- function(sql.file) {
  # Returns vector of queries from the input sql.file
  # 
  # Blank lines and comments are removed.
  queries <- Filter(function (x) x != "", readLines(sql.file))
  comments <- grep("^-", queries)
  if (length(comments) > 0) {
    queries <- queries[-comments]
  }
  queries <- paste(queries, collapse="\n")
  unlist(strsplit(queries, ";"))
}
