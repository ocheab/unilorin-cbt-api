# =========================================================
# UniIlorin CBT API
# plumber.R
# =========================================================

library(plumber)
library(DBI)
library(RSQLite)
library(jsonlite)
library(httr2)
library(digest)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

normalize_email <- function(email) {
  tolower(trimws(email %||% ""))
}

DB_PATH <- Sys.getenv("CBT_DB_PATH", unset = "cbt_admin.sqlite")
ADMIN_API_KEY <- Sys.getenv("ADMIN_API_KEY", unset = "")
PAYSTACK_SECRET_KEY <- Sys.getenv("PAYSTACK_SECRET_KEY", unset = "")
APP_BASE_URL <- Sys.getenv("APP_BASE_URL", unset = "https://unilorin-cbt-api.onrender.com")
EXPECTED_AMOUNT_KOBO <- 200000

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

paystack_verify_transaction <- function(reference) {
  req <- request(paste0("https://api.paystack.co/transaction/verify/", reference)) |>
    req_method("GET") |>
    req_headers(
      Authorization = paste("Bearer", PAYSTACK_SECRET_KEY)
    )

  resp <- req_perform(req)
  fromJSON(resp_body_string(resp))
}

init_db()

#* API health check
#* @get /health
#* @serializer json
function() {
  list(success = TRUE, message = "UniIlorin CBT API is running")
}

#* Optional callback landing route
#* @get /payment-callback
#* @serializer json
function(req, res) {
  reference <- req$argsQuery$reference %||% ""
  trxref <- req$argsQuery$trxref %||% ""

  list(
    success = TRUE,
    message = "Payment callback received. Please return to the app and check access.",
    reference = reference,
    trxref = trxref
  )
}

#* Initialize payment on backend
#* @post /initialize-payment
#* @serializer json
function(req, res) {
  if (!nzchar(PAYSTACK_SECRET_KEY)) {
    res$status <- 500
    return(list(success = FALSE, message = "PAYSTACK_SECRET_KEY is not configured"))
  }

  body <- tryCatch(fromJSON(req$postBody), error = function(e) list())
  email <- normalize_email(body$email %||% "")
  amount <- as.integer(body$amount %||% EXPECTED_AMOUNT_KOBO)

  if (!nzchar(email)) {
    res$status <- 400
    return(list(success = FALSE, message = "Email is required"))
  }

  if (is.na(amount) || amount <= 0) {
    amount <- EXPECTED_AMOUNT_KOBO
  }

  reference <- paste0("cbt_", as.integer(Sys.time()), "_", sample(1000:9999, 1))

  pay_req <- request("https://api.paystack.co/transaction/initialize") |>
    req_method("POST") |>
    req_headers(
      Authorization = paste("Bearer", PAYSTACK_SECRET_KEY),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      email = email,
      amount = amount,
      currency = "NGN",
      callback_url = paste0(APP_BASE_URL, "/payment-callback"),
      reference = reference,
      metadata = list(
        app = "unilorin_cbt",
        access_type = "full_access",
        email = email
      )
    ))

  out <- tryCatch({
    resp <- req_perform(pay_req)
    fromJSON(resp_body_string(resp))
  }, error = function(e) {
    log_payment(
      email = email,
      reference = reference,
      amount_kobo = amount,
      status = paste0("initialize_error:", conditionMessage(e)),
      source = "initialize-payment"
    )
    NULL
  })

  if (is.null(out) || !isTRUE(out$status)) {
    res$status <- 500
    return(list(success = FALSE, message = "Payment initialization failed"))
  }

  log_payment(
    email = email,
    reference = reference,
    amount_kobo = amount,
    status = "initialized",
    source = "initialize-payment"
  )

  list(
    success = TRUE,
    data = out$data
  )
}

#* Check whether a user currently has access
#* @post /check-access
#* @serializer json
function(req, res) {
  body <- tryCatch(fromJSON(req$postBody), error = function(e) list())
  email <- normalize_email(body$email %||% "")

  if (!nzchar(email)) {
    res$status <- 400
    return(list(
      success = FALSE,
      hasAccess = FALSE,
      daysLeft = 0L,
      expiresAt = NULL,
      source = NULL,
      message = "Email is required"
    ))
  }

  user <- get_user_by_email(email)

  if (is.null(user)) {
    return(list(
      success = TRUE,
      hasAccess = FALSE,
      daysLeft = 0L,
      expiresAt = NULL,
      source = NULL,
      message = "No active access found"
    ))
  }

  has_access <- isTRUE(as.logical(user$has_access[[1]]))
  expiry <- user$access_expires_at[[1]] %||% ""
  access_source <- user$access_source[[1]] %||% ""
  days_left <- days_left_from_expiry(expiry)

  if (!has_access) {
    return(list(
      success = TRUE,
      hasAccess = FALSE,
      daysLeft = 0L,
      expiresAt = expiry,
      source = access_source,
      message = "Access not active"
    ))
  }

  if (!is.null(days_left) && days_left <= 0) {
    return(list(
      success = TRUE,
      hasAccess = FALSE,
      daysLeft = 0L,
      expiresAt = expiry,
      source = access_source,
      message = "Access expired"
    ))
  }

  list(
    success = TRUE,
    hasAccess = TRUE,
    daysLeft = days_left %||% 0L,
    expiresAt = expiry,
    source = access_source,
    message = "Access active"
  )
}

#* Submit a transfer/manual payment claim for admin review
#* @post /submit-transfer-claim
#* @serializer json
function(req, res) {
  body <- tryCatch(fromJSON(req$postBody), error = function(e) list())

  email <- normalize_email(body$email %||% "")
  note <- trimws(body$note %||% "User claimed transfer payment")

  if (!nzchar(email)) {
    res$status <- 400
    return(list(success = FALSE, message = "Email is required"))
  }

  ok <- submit_transfer_claim_db(email, note)

  if (!ok) {
    res$status <- 500
    return(list(success = FALSE, message = "Failed to submit transfer claim"))
  }

  log_payment(
    email = email,
    reference = "",
    amount_kobo = NA,
    status = "transfer_claim_submitted",
    source = "submit-transfer-claim"
  )

  list(
    success = TRUE,
    message = "Transfer claim submitted successfully",
    email = email
  )
}

#* Grant access manually from backend
#* @post /grant-access
#* @serializer json
function(req, res) {
  admin_key <- req$HTTP_X_ADMIN_KEY %||% ""

  if (!nzchar(ADMIN_API_KEY) || !identical(admin_key, ADMIN_API_KEY)) {
    res$status <- 403
    return(list(success = FALSE, message = "Unauthorized"))
  }

  body <- tryCatch(fromJSON(req$postBody), error = function(e) list())

  email <- normalize_email(body$email %||% "")
  days <- as.integer(body$days %||% 365)
  note <- trimws(body$note %||% "Manual backend grant")

  if (!nzchar(email)) {
    res$status <- 400
    return(list(success = FALSE, message = "Email is required"))
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
    return(list(success = FALSE, message = "Failed to grant access"))
  }

  log_payment(
    email = email,
    reference = "",
    amount_kobo = NA,
    status = "manual_access_granted",
    source = "grant-access"
  )

  list(
    success = TRUE,
    message = "Access granted successfully",
    email = email,
    days = days
  )
}

#* Paystack webhook
#* @post /paystack-webhook
#* @serializer json
function(req, res) {
  if (!nzchar(PAYSTACK_SECRET_KEY)) {
    res$status <- 500
    return(list(success = FALSE, message = "PAYSTACK_SECRET_KEY is not configured"))
  }

  raw_body <- req$postBody %||% ""
  signature <- req$HTTP_X_PAYSTACK_SIGNATURE %||% ""

  computed_sig <- digest(
    object = raw_body,
    algo = "sha512",
    serialize = FALSE,
    key = PAYSTACK_SECRET_KEY
  )

  if (!nzchar(signature) || !identical(tolower(signature), tolower(computed_sig))) {
    log_payment(
      email = "",
      reference = "",
      amount_kobo = NA,
      status = "invalid_signature",
      source = "paystack-webhook"
    )
    res$status <- 401
    return(list(success = FALSE, message = "Invalid signature"))
  }

  event <- tryCatch(fromJSON(raw_body), error = function(e) NULL)

  if (is.null(event)) {
    log_payment(
      email = "",
      reference = "",
      amount_kobo = NA,
      status = "invalid_json_payload",
      source = "paystack-webhook"
    )
    res$status <- 400
    return(list(success = FALSE, message = "Invalid JSON payload"))
  }

  event_name <- event$event %||% ""
  data <- event$data %||% list()

  reference <- data$reference %||% ""
  email <- normalize_email(data$customer$email %||% data$metadata$email %||% "")
  amount <- as.integer(data$amount %||% 0)
  status <- tolower(data$status %||% "")

  log_payment(
    email = email,
    reference = reference,
    amount_kobo = amount,
    status = paste0(status, " | event=", event_name),
    source = paste0("webhook:", event_name)
  )

  if (identical(event_name, "charge.success")) {
    verified <- tryCatch(paystack_verify_transaction(reference), error = function(e) e)

    if (inherits(verified, "error")) {
      log_payment(
        email = email,
        reference = reference,
        amount_kobo = amount,
        status = paste0("verification_failed:", conditionMessage(verified)),
        source = "webhook_verify"
      )
    } else if (is.null(verified)) {
      log_payment(
        email = email,
        reference = reference,
        amount_kobo = amount,
        status = "verification_failed_null",
        source = "webhook_verify"
      )
    } else if (!isTRUE(verified$status)) {
      log_payment(
        email = email,
        reference = reference,
        amount_kobo = amount,
        status = "verify_status_false",
        source = "webhook_verify"
      )
    } else if (is.null(verified$data)) {
      log_payment(
        email = email,
        reference = reference,
        amount_kobo = amount,
        status = "verify_data_missing",
        source = "webhook_verify"
      )
    } else if (tolower(verified$data$status %||% "") != "success") {
      log_payment(
        email = email,
        reference = reference,
        amount_kobo = as.integer(verified$data$amount %||% 0),
        status = paste0("verified_not_success:", verified$data$status %||% ""),
        source = "webhook_verify"
      )
    } else if (as.integer(verified$data$amount %||% 0) != EXPECTED_AMOUNT_KOBO) {
      log_payment(
        email = email,
        reference = reference,
        amount_kobo = as.integer(verified$data$amount %||% 0),
        status = paste0("amount_mismatch_expected_", EXPECTED_AMOUNT_KOBO),
        source = "webhook_verify"
      )
    } else {
      verified_email <- normalize_email(
        verified$data$customer$email %||%
          verified$data$metadata$email %||%
          email
      )

      if (nzchar(verified_email)) {
        upsert_user_access(
          email = verified_email,
          days = 365,
          source = "paystack",
          note = paste("Auto-granted after Paystack webhook:", reference)
        )

        log_payment(
          email = verified_email,
          reference = reference,
          amount_kobo = as.integer(verified$data$amount %||% 0),
          status = "access_granted",
          source = "webhook_verify"
        )
      } else {
        log_payment(
          email = email,
          reference = reference,
          amount_kobo = as.integer(verified$data$amount %||% 0),
          status = "verified_but_email_missing",
          source = "webhook_verify"
        )
      }
    }
  }

  res$status <- 200
  list(success = TRUE)
}

#* Debug: recent payment logs
#* @get /debug-payment-logs
#* @serializer json
function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  logs <- dbGetQuery(
    con,
    "SELECT * FROM payment_logs ORDER BY id DESC LIMIT 50"
  )

  list(success = TRUE, logs = logs)
}

#* Debug: recent users
#* @get /debug-users
#* @serializer json
function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  users <- dbGetQuery(
    con,
    "SELECT * FROM users ORDER BY id DESC LIMIT 50"
  )

  list(success = TRUE, users = users)
}

#* Debug: recent transfer claims
#* @get /debug-transfer-claims
#* @serializer json
function() {
  con <- get_con()
  on.exit(dbDisconnect(con), add = TRUE)

  claims <- dbGetQuery(
    con,
    "SELECT * FROM transfer_claims ORDER BY id DESC LIMIT 50"
  )

  list(success = TRUE, claims = claims)
}
