# =========================================================
# UniIlorin CBT API
# plumber.R
# =========================================================

library(plumber)
library(DBI)
library(RSQLite)
library(jsonlite)

# =========================
# HELPERS
# =========================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

normalize_email <- function(email) {
  tolower(trimws(email %||% ""))
}

DB_PATH <- Sys.getenv("CBT_DB_PATH", unset = "cbt_admin.sqlite")
ADMIN_API_KEY <- Sys.getenv("ADMIN_API_KEY", unset = "OcheAdminKey123")

get_con <- function() {
  dbConnect(SQLite(), DB_PATH)
}

init_db <- function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      email TEXT UNIQUE NOT NULL,
      has_access INTEGER DEFAULT 0,
      access_expires_at TEXT,
      access_source TEXT,
      note TEXT,
      updated_at TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transfer_claims (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      email TEXT NOT NULL,
      status TEXT DEFAULT 'pending',
      note TEXT,
      created_at TEXT,
      reviewed_at TEXT,
      reviewed_by TEXT
    )
  ")
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS payment_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      email TEXT,
      reference TEXT,
      amount_kobo INTEGER,
      status TEXT,
      source TEXT,
      created_at TEXT
    )
  ")
}

get_user_by_email <- function(email) {
  email <- normalize_email(email)
  if (!nzchar(email)) return(NULL)
  
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  res <- dbGetQuery(
    con,
    "SELECT * FROM users WHERE lower(email) = lower(?) LIMIT 1",
    params = list(email)
  )
  
  if (nrow(res) == 0) return(NULL)
  res[1, , drop = FALSE]
}

upsert_user_access <- function(email, days = 365, source = "manual", note = "") {
  email <- normalize_email(email)
  if (!nzchar(email)) return(FALSE)
  
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  expiry <- as.character(Sys.time() + (days * 24 * 60 * 60))
  now <- as.character(Sys.time())
  
  existing <- dbGetQuery(
    con,
    "SELECT id FROM users WHERE lower(email) = lower(?)",
    params = list(email)
  )
  
  if (nrow(existing) == 0) {
    dbExecute(
      con,
      "INSERT INTO users (email, has_access, access_expires_at, access_source, note, updated_at)
       VALUES (?, 1, ?, ?, ?, ?)",
      params = list(email, expiry, source, note, now)
    )
  } else {
    dbExecute(
      con,
      "UPDATE users
       SET has_access = 1,
           access_expires_at = ?,
           access_source = ?,
           note = ?,
           updated_at = ?
       WHERE lower(email) = lower(?)",
      params = list(expiry, source, note, now, email)
    )
  }
  
  TRUE
}

submit_transfer_claim_db <- function(email, note = "User claimed transfer payment") {
  email <- normalize_email(email)
  if (!nzchar(email)) return(FALSE)
  
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(
    con,
    "INSERT INTO transfer_claims (email, status, note, created_at)
     VALUES (?, 'pending', ?, ?)",
    params = list(email, note, as.character(Sys.time()))
  )
  
  TRUE
}

log_payment <- function(email = NULL, reference = NULL, amount_kobo = NULL,
                        status = NULL, source = "api") {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)
  
  dbExecute(
    con,
    "INSERT INTO payment_logs (email, reference, amount_kobo, status, source, created_at)
     VALUES (?, ?, ?, ?, ?, ?)",
    params = list(
      normalize_email(email %||% ""),
      reference %||% "",
      amount_kobo %||% NA,
      status %||% "",
      source %||% "api",
      as.character(Sys.time())
    )
  )
}

days_left_from_expiry <- function(expiry_text) {
  if (is.null(expiry_text) || !nzchar(expiry_text)) return(NULL)
  
  expiry <- tryCatch(as.POSIXct(expiry_text, tz = "UTC"), error = function(e) NULL)
  if (is.null(expiry)) return(NULL)
  
  diff_secs <- as.numeric(difftime(expiry, Sys.time(), units = "secs"))
  if (is.na(diff_secs) || diff_secs <= 0) return(0L)
  
  as.integer(ceiling(diff_secs / 86400))
}

# Initialize DB on startup
init_db()

# Health check
#* API health check
#* @get /health
function() {
  list(
    success = TRUE,
    message = "UniIlorin CBT API is running"
  )
}

# Check access
#* Check whether a user currently has access
#* @post /check-access
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  email <- normalize_email(body$email %||% "")
  
  if (!nzchar(email)) {
    res$status <- 400
    return(list(
      success = FALSE,
      hasAccess = FALSE,
      message = "Email is required"
    ))
  }
  
  user <- get_user_by_email(email)
  
  if (is.null(user)) {
    return(list(
      success = TRUE,
      hasAccess = FALSE,
      daysLeft = 0L,
      message = "No active access found"
    ))
  }
  
  has_access <- isTRUE(as.logical(user$has_access[[1]]))
  expiry <- user$access_expires_at[[1]] %||% ""
  days_left <- days_left_from_expiry(expiry)
  
  if (!has_access) {
    return(list(
      success = TRUE,
      hasAccess = FALSE,
      daysLeft = 0L,
      message = "Access not active"
    ))
  }
  
  if (!is.null(days_left) && days_left <= 0) {
    return(list(
      success = TRUE,
      hasAccess = FALSE,
      daysLeft = 0L,
      message = "Access expired"
    ))
  }
  
  list(
    success = TRUE,
    hasAccess = TRUE,
    daysLeft = days_left %||% 0L,
    accessSource = user$access_source[[1]] %||% "",
    message = "Access active"
  )
}

# Submit transfer claim
#* Submit a transfer/manual payment claim for admin review
#* @post /submit-transfer-claim
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  
  email <- normalize_email(body$email %||% "")
  note <- trimws(body$note %||% "User claimed transfer payment")
  
  if (!nzchar(email)) {
    res$status <- 400
    return(list(
      success = FALSE,
      message = "Email is required"
    ))
  }
  
  ok <- submit_transfer_claim_db(email, note)
  
  if (!ok) {
    res$status <- 500
    return(list(
      success = FALSE,
      message = "Failed to submit transfer claim"
    ))
  }
  
  list(
    success = TRUE,
    message = "Transfer claim submitted successfully"
  )
}

# Admin grant access
#* Grant access manually from backend
#* @post /grant-access
#* @serializer json
function(req, res) {
  admin_key <- req$HTTP_X_ADMIN_KEY %||% ""
  
  if (!identical(admin_key, ADMIN_API_KEY)) {
    res$status <- 403
    return(list(
      success = FALSE,
      message = "Unauthorized"
    ))
  }
  
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
  
  email <- normalize_email(body$email %||% "")
  days <- as.integer(body$days %||% 365)
  note <- trimws(body$note %||% "Manual backend grant")
  
  if (!nzchar(email)) {
    res$status <- 400
    return(list(
      success = FALSE,
      message = "Email is required"
    ))
  }
  
  if (is.na(days) || days <= 0) {
    days <- 365
  }
  
  ok <- upsert_user_access(
    email = email,
    days = days,
    source = "manual",
    note = note
  )
  
  if (!ok) {
    res$status <- 500
    return(list(
      success = FALSE,
      message = "Failed to grant access"
    ))
  }
  
  list(
    success = TRUE,
    message = "Access granted successfully",
    email = email,
    days = days
  )
}