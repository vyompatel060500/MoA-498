# Functions for melting data, inserting to database, and unmelting data plus extracting datasets from database.

## General notes on using the databases:
## source: https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
# use dbGetQuery(.) for any query that returns a result
#   NOTE: dbGetQuery returns a data.frame
# You can use dbExecute(.) for any query that you do not expect to return results, but it is not mandatory.

# https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r
IB2.env <- new.env()
IB2.env$default_dbfile <- "GRID_SEARCH_RESULTS.db"
IB2.env$dbfile <- IB2.env$default_dbfile
IB2.env$default_dbstore <- ".GRID_SEARCH_RESULTS.db"
IB2.env$dbstore <- IB2.env$default_dbstore

###############################################################################
### CREATE INITIAL DATABASE ###################################################

#' Initialize the database used for storing experiment results and synthetic datasets.
#'
#' @export
init_db <- function() {
  pdir <- getwd()
  sh.path <- file.path(pdir, 'rebuild-db.sh')
  sql.path <- file.path(pdir, 'init-db.sql')
  cmd_str <- paste(sh.path, sql.path, IB2.env$dbfile)
  system(cmd_str)
}

###############################################################################
### GENERAL DATABASE PROCEDURES ###############################################

open_and_source_db <- function(dbfile) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
    return(con)
}

open_db <- function(dbfile) { open_and_source_db(dbfile) }
close_db <- function(db) { DBI::dbDisconnect(db) }

### DATABASE CONNECTION MANAGER ###############################################
# transparently handle database connection.

#' Database connection manager
#'
#' @param func function that requires access to the database.
#' @export
dbm <- function(func) {
    function(...) {
	db <- open_db(IB2.env$dbfile)
	res <- func(db, ...)
	close_db(db)
	return(res)
    }
}

insert_row_to_table_and_return_id <- function(db, table_name, df) {
  named_placeholders <- map(names(df), function(x) paste0(":", x)) %>% toString
  q_str <- paste0("INSERT INTO ", table_name, "(", toString(names(df)), ") VALUES (", named_placeholders, ")" )

  # RSQLite synchronization: https://github.com/r-dbi/RSQLite/issues/56#issuecomment-70710256
  res <- DBI::dbSendQuery(db, "PRAGMA busy_timeout=5000;")
  DBI::dbClearResult(res)
  
  repeat {
    rv <- try( DBI::dbExecute(db, q_str, params=df) )
    if(!is(rv, 'try-error')) break
  }

  get_most_recent_ID(db) # return the experiment ID
}

get_most_recent_ID <- function(db) {
  # Concurrency proof, gets the rowid of the last insert into the most recent table
  DBI::dbGetQuery(db, "SELECT last_insert_rowid();")[[1]]
}

#' TODO: Func desc
#' 
#' @param
#' @return 
#' @expor
insert_result <- dbm(function(db, res, ...) {
  Robj_path <- glue::glue("{IB2.env$dbstore}/Results/")

  Robj_id <- insert_row_to_table_and_return_id(db, table_name='JiangResults',
	      df=data.frame(..., DirectoryPath=Robj_path))

  dir.create(file.path(getwd(), Robj_path), showWarnings=FALSE, recursive=TRUE)
  saveRDS(res, file=paste0(Robj_path, Robj_id, ".rds"))

  return(Robj_id)
})

#' Get sql table from the project database. Generally for retrieving result objects.
#' 
#' @param table_name Name of sql table to return for inspection.
#' @return tibble with the current contents of the related table
#' @export
db_get_tbl <- dbm(function(db, table_name) {
  dplyr::tbl(db, table_name) %>% as_tibble
})

# TODO: Move out to VALIDATION-* file
VALIDATE_GET_ROBJS <- function(res_df, id, dir_path, single) {
  if(single) {
    assert(is.list(res_df) || (tibble::is_tibble(res_df) && nrow(res_df) == 1), 'If single == TRUE, then res_df must have only one value')
    assert(!is.null(res_df[[id]]) && !is.null(res_df[[dir_path]]), 'with single, res_df must have the "id" and "dir_path" elements.')
  } else {
    assert(tibble::is_tibble(res_df), 'get_Robjs: res_df must be a tibble!')
    assert(nrow(res_df) > 0, "get_Robjs recieved empty data.frame!!")
  }

  # ALWAYS: Check for NA's
  assert(all(!is.na(res_df[id])), glue::glue('res_df {id} has at least one NA value'))
  assert(all(!is.na(res_df[dir_path])), glue::glue('res_df {dir_path} has at least one NA value'))
}

#' Given filtered data.frame of results matching a database table, extract Robjects via the Robj_path and ID fields.
#'
#' @param res_df data.frame describing R objects to extract
#' @return list of Robjects matching res_df
#' @export
get_Robjs <- function(res_df, id='ID', dir_path='DirectoryPath', single=FALSE) {
  VALIDATE_GET_ROBJS(res_df=res_df, id=id, dir_path=dir_path, single=single)

  if(single) {
    row <- res_df
    f = paste0(row[dir_path] %>% trimws, row[id] %>% trimws, ".rds")
    readRDS(f)
  } else {
    apply(res_df, 1, function(row) {
      f = paste0(row[dir_path] %>% trimws, row[id] %>% trimws, ".rds")
      readRDS(f)
    })
  }
}

