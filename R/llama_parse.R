# Required libraries (Ensure they are listed in DESCRIPTION Imports)
# library(httr2)
# library(mime)
# library(curl)

#' Parse PDF from Local File or URL to Markdown using LlamaParse API
#'
#' Downloads a PDF if a URL is provided, then uploads the PDF to the
#' LlamaParse API, polls for job completion, and retrieves the parsed
#' result as markdown text. Written by Gemini.
#' Requires free LlamaCloud API key.
#'
#' @param input_source character. Path to a local PDF file OR a URL pointing to a PDF file.
#' @param api_key character. Your LlamaParse API key.
#' @param base_url character. The base URL for the LlamaParse API. Defaults to "https://api.cloud.llamaindex.ai/api/parsing".
#' @param poll_interval integer. Seconds to wait between polling for job status. Defaults to 8.
#' @param download_timeout integer. Seconds to wait for the PDF download if a URL is provided. Defaults to 60.
#'
#' @return A character string containing the parsed markdown content.
#' @export
#'
#' @importFrom httr2 request req_auth_bearer_token req_headers req_body_multipart req_perform resp_status resp_body_json resp_status_desc req_error resp_body_string resp_content_type resp_body_raw req_timeout req_retry url_parse
#' @importFrom mime guess_type
#' @importFrom curl form_file
#'
#' @examples
#' \dontrun{
#'   # Ensure API key is set, e.g., Sys.setenv(LLAMA_CLOUD_API_KEY="your-key")
#'   my_api_key <- Sys.getenv("LLAMA_CLOUD_API_KEY")
#'
#'   # --- Example with a local file ---
#'   # Create a dummy PDF for example if needed, or use a real one
#'   dummy_file <- tempfile(fileext = ".pdf")
#'   writeLines("Dummy PDF Content", dummy_file)
#'   if (nchar(my_api_key) > 0 && file.exists(dummy_file)) {
#'      markdown_output_file <- parse_pdf_to_markdown(dummy_file, my_api_key)
#'      cat("--- File Result (First 500 chars) ---\n")
#'      cat(substring(markdown_output_file, 1, 500), "\n")
#'   } else {
#'      message("API key not found or dummy PDF creation failed.")
#'   }
#'   unlink(dummy_file) # Clean up dummy file
#'
#'   # --- Example with a URL ---
#'   pdf_url <- "https://arxiv.org/pdf/1706.03762.pdf" # Attention is All You Need paper
#'   if (nchar(my_api_key) > 0) {
#'     markdown_output_url <- parse_pdf_to_markdown(pdf_url, my_api_key)
#'     cat("\n--- URL Result (First 500 chars) ---\n")
#'     cat(substring(markdown_output_url, 1, 500), "\n")
#'   } else {
#'     message("API key not found.")
#'   }
#' }
parse_pdf_to_markdown <- function(input_source,
                                  api_key,
                                  base_url = "https://api.cloud.llamaindex.ai/api/parsing",
                                  poll_interval = 8,
                                  download_timeout = 60) {

  # --- Input Validation ---
  if (!is.character(input_source) || length(input_source) != 1 || nchar(input_source) == 0) {
    stop("`input_source` must be a non-empty character string (file path or URL).", call. = FALSE)
  }
  if (!is.character(api_key) || length(api_key) != 1 || nchar(api_key) == 0) {
    stop("`api_key` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.numeric(poll_interval) || length(poll_interval) != 1 || poll_interval <= 0) {
    stop("`poll_interval` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(download_timeout) || length(download_timeout) != 1 || download_timeout <= 0) {
    stop("`download_timeout` must be a positive number.", call. = FALSE)
  }

  # --- Determine Input Type and Prepare File Path ---
  is_url <- grepl("^https?://", input_source, ignore.case = TRUE)
  actual_file_path <- NULL
  temp_file_created <- FALSE
  cleanup_action <- NULL # Initialize cleanup action

  if (is_url) {
    # --- Download the PDF from URL ---
    pdf_url <- input_source
    cat("Input is a URL. Downloading PDF from:", pdf_url, "...\n")
    temp_pdf_path <- tempfile(fileext = ".pdf")
    # Define cleanup action
    cleanup_action <- function() {
      if (temp_file_created && file.exists(temp_pdf_path)) {
        unlink(temp_pdf_path, force = TRUE)
        # cat("Temporary file", temp_pdf_path, "deleted.\n") # Optional: for debugging
      }
    }
    on.exit(cleanup_action(), add = TRUE) # Register cleanup

    req_download <- httr2::request(pdf_url) |>
      httr2::req_timeout(download_timeout) |>
      httr2::req_retry(max_tries = 2) |>
      # Let httr2 handle basic HTTP errors for download
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                       body = function(resp) {
                         body_text <- "(body unavailable)"
                         try({ body_text <- httr2::resp_body_string(resp) }, silent=TRUE)
                         paste("Failed to download PDF from URL.",
                               "Status:", httr2::resp_status_desc(resp),
                               "Body:", body_text)
                       })

    # Perform download - req_error handles HTTP status errors
    # Other errors (network, DNS) will propagate naturally
    resp_download <- httr2::req_perform(req_download)

    # Optional: Check Content-Type
    content_type <- httr2::resp_content_type(resp_download)
    if (!is.null(content_type) && !grepl("application/pdf", content_type, ignore.case = TRUE)) {
      warning("Downloaded file Content-Type is '", content_type, "', not 'application/pdf'. Proceeding anyway.", call. = FALSE)
    }

    # Write the downloaded raw content to the temporary file
    pdf_content <- httr2::resp_body_raw(resp_download)
    tryCatch({
      writeBin(pdf_content, temp_pdf_path)
      temp_file_created <- TRUE # Mark that the file was successfully created for cleanup
    }, error = function(e) {
      # This tryCatch is kept as writeBin can fail for file system reasons
      stop("Failed to write downloaded content to temporary file: ", conditionMessage(e), call. = FALSE)
    })

    cat("PDF downloaded successfully to temporary file:", temp_pdf_path, "\n")
    actual_file_path <- temp_pdf_path

  } else {
    # --- Input is a Local File Path ---
    cat("Input is a local file path:", input_source, "\n")
    if (!file.exists(input_source)) {
      stop("Local file not found: ", input_source, call. = FALSE)
    }
    actual_file_path <- input_source
  }

  # --- Sanity check ---
  if (is.null(actual_file_path) || !nzchar(actual_file_path) || !file.exists(actual_file_path)) {
    stop("Internal error: Could not determine a valid file path to upload.", call. = FALSE)
  }

  # --- 1. Upload the file ---
  upload_url <- paste0(base_url, "/upload")
  file_mime_type <- mime::guess_type(actual_file_path, unknown = "application/pdf")

  req_upload <- httr2::request(upload_url) |>
    httr2::req_auth_bearer_token(token = api_key) |>
    httr2::req_headers(Accept = "application/json") |>
    # Ensure the path used by form_file is valid
    httr2::req_body_multipart(file = curl::form_file(path = actual_file_path, type = file_mime_type)) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3) |>
    # Use httr2's standard error handling for HTTP status codes >= 400
    httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                     body = function(resp) {
                       body_text <- "(body unavailable)"
                       detail <- NULL
                       try({ body_text <- httr2::resp_body_string(resp) }, silent=TRUE)
                       try({ detail <- httr2::resp_body_json(resp)$detail }, silent=TRUE)
                       if (!is.null(detail)) {
                         paste("API Upload Error:", httr2::resp_status_desc(resp), "-", detail)
                       } else {
                         paste("API Upload Error:", httr2::resp_status_desc(resp), "\nBody:", body_text)
                       }
                     })

  cat("Uploading file:", basename(actual_file_path), "...\n")

  # Perform upload - req_error handles HTTP status errors
  # Other errors (network, DNS, curl issues) will propagate naturally
  resp_upload <- httr2::req_perform(req_upload)

  # Parse JSON response for job ID
  upload_result <- tryCatch({
    httr2::resp_body_json(resp_upload)
  }, error = function(e) {
    # Kept this tryCatch as parsing might fail even on 2xx response
    body_str <- httr2::resp_body_string(resp_upload)
    stop("Failed to parse JSON response from upload endpoint. Status: ", httr2::resp_status(resp_upload), " Body: ", body_str, call. = FALSE)
  })

  if (is.null(upload_result$id)) {
    stop("Could not find 'id' (Job ID) in the API response after upload.", call. = FALSE)
  }
  job_id <- upload_result$id
  cat("File uploaded successfully. Job ID:", job_id, "\n")

  # --- 2. Poll for job completion ---
  result_url <- paste0(base_url, "/job/", job_id, "/result/markdown")
  status_url <- paste0(base_url, "/job/", job_id)

  cat("Waiting for parsing job '", job_id, "' to complete (checking every ", poll_interval, " seconds)...", sep="")

  start_time <- Sys.time()
  polling_timeout_seconds <- 60 * 10 # 10 minutes timeout for polling

  while (TRUE) {
    if (difftime(Sys.time(), start_time, units = "secs") > polling_timeout_seconds) {
      stop("Polling timed out after ", polling_timeout_seconds, " seconds for job ID: ", job_id, call. = FALSE)
    }

    req_status <- httr2::request(status_url) |>
      httr2::req_auth_bearer_token(token = api_key) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_retry(max_tries = 3) |>
      # Don't automatically error on status check, handle manually below
      httr2::req_error(is_error = function(resp) FALSE)

    # Perform status check - don't use tryCatch here, let errors propagate if they happen
    # (except maybe transient http errors which req_retry handles)
    # We need to handle the response status manually anyway
    resp_status_check <- httr2::req_perform(req_status)
    current_status_code <- httr2::resp_status(resp_status_check)

    if (current_status_code == 200) {
      status_body <- tryCatch(httr2::resp_body_json(resp_status_check), error = function(e) NULL)

      if (is.null(status_body) || is.null(status_body$status)) {
        cat("?") # Indicate unexpected/unparsable response
        warning("Polling status check returned 200 but failed to parse JSON or find status field.", call.=FALSE)
      } else {
        job_status_lower <- tolower(status_body$status)
        if (job_status_lower == "success") {
          cat("\nJob completed successfully!\n")
          break # Exit polling loop
        } else if (job_status_lower %in% c("pending", "processing")) {
          cat(".") # Indicate successful poll, still processing
        } else {
          # Handle definitive failure status - Stop polling and report error
          error_details_str <- paste(names(status_body), status_body, sep = ": ", collapse = ", ")
          stop("Parsing Job Failed according to API. Status: ", status_body$status, "\nDetails: ", error_details_str, call. = FALSE)
        }
      }
    } else {
      # Handle non-200 status during polling (e.g., 4xx, 5xx)
      cat("h") # Indicate HTTP issue during this poll attempt
      warning("Polling status check returned non-200 status: ", current_status_code, call.=FALSE)
      # Consider adding logic to stop polling after several consecutive non-200s?
    }

    Sys.sleep(poll_interval) # Wait before next poll attempt
  } # End while loop

  # --- 3. Retrieve the markdown result ---
  req_result <- httr2::request(result_url) |>
    httr2::req_auth_bearer_token(token = api_key) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_retry(max_tries = 3) |>
    # Use req_error for final result fetch
    httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400,
                     body = function(resp) {
                       body_text <- "(body unavailable)"
                       try({ body_text <- httr2::resp_body_string(resp) }, silent=TRUE)
                       paste("API Result Error:", httr2::resp_status_desc(resp),
                             "\nBody:", body_text)
                     })

  cat("Fetching markdown result...\n")

  # Perform result fetch - req_error handles HTTP status errors
  resp_result <- httr2::req_perform(req_result)

  # Parse result JSON
  result_data <- tryCatch({
    httr2::resp_body_json(resp_result)
  }, error = function(e) {
    # Kept this tryCatch as parsing might fail
    body_str <- httr2::resp_body_string(resp_result)
    stop("Failed to parse JSON response from result endpoint. Status: ", httr2::resp_status(resp_result), " Body: ", body_str, call. = FALSE)
  })

  markdown_content <- result_data$markdown

  if (is.null(markdown_content)) {
    warning("Markdown content not found or is NULL in the API response.", call. = FALSE)
    return("")
  }

  cat("Markdown retrieved successfully.\n")
  return(markdown_content)
}
