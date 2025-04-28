#' Parse PDF to Markdown using LlamaParse API
#'
#' Uploads a PDF file to the LlamaParse API, polls for job completion,
#' and retrieves the parsed result as markdown text. Written by Gemini.
#' Requires free LlamaCloud API key.
#'
#' @param file_path character Path to the PDF file to parse.
#' @param api_key character Your LlamaParse API key.
#' @param base_url character The base URL for the LlamaParse API.
#' @param poll_interval integer Seconds to wait between polling for job status. Defaults to 8.
#'
#' @return The parsed markdown content as a string.
#' @export
#'
#' @importFrom httr2 request req_auth_bearer_token req_headers req_body_multipart req_perform resp_status resp_body_json resp_status_desc req_error resp_body_string
#' @importFrom mime guess_type
#' @importFrom curl form_file
#'
#' @examples
#' \dontrun{
#'   my_api_key <- "YOUR_LLAMA_CLOUD_API_KEY" # Replace with your actual key
#'   pdf_file <- "/path/to/your/document.pdf" # Replace with your actual file path
#'   markdown_output <- parse_pdf_to_markdown(pdf_file, my_api_key)
#'   cat(markdown_output)
#' }
parse_pdf_to_markdown <- function(file_path,
                                  api_key,
                                  base_url = "https://api.cloud.llamaindex.ai/api/parsing",
                                  poll_interval = 8) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # --- 1. Upload the file ---
  upload_url <- paste0(base_url, "/upload")
  file_mime_type <- mime::guess_type(file_path) # Guess MIME type

  req_upload <- request(upload_url) |>
    req_auth_bearer_token(token = api_key) |>
    req_headers(Accept = "application/json") |>
    # Use req_body_multipart to send the file
    req_body_multipart(file = curl::form_file(file_path, type = file_mime_type)) |>
    req_error(is_error = function(resp) FALSE) # Handle errors manually

  cat("Uploading file:", basename(file_path), "...\n")
  resp_upload <- req_perform(req_upload)

  # Check for upload errors
  if (resp_status(resp_upload) >= 400) {
    error_details <- tryCatch(resp_body_json(resp_upload), error = function(e) resp_body_string(resp_upload))
    stop("API Upload Error: ", resp_status_desc(resp_upload), "\nDetails: ", paste(error_details, collapse="\n"))
  }

  upload_result <- resp_body_json(resp_upload)
  job_id <- upload_result$id
  cat("File uploaded successfully. Job ID:", job_id, "\n") # [cite: 5]

  # --- 2. Poll for job completion ---
  result_url <- paste0(base_url, "/job/", job_id, "/result/markdown") # [cite: 3]
  status_url <- paste0(base_url, "/job/", job_id) # [cite: 2]

  cat("Waiting for parsing job to complete (checking every", poll_interval, "seconds)...\n")

  while (TRUE) {
    req_status <- request(status_url) |>
      req_auth_bearer_token(token = api_key) |>
      req_headers(Accept = "application/json") |>
      req_error(is_error = function(resp) FALSE) # Handle errors manually

    resp_status_check <- req_perform(req_status)
    current_status_code <- resp_status(resp_status_check)

    if (current_status_code == 200) {
      status_body <- resp_body_json(resp_status_check)
      if (tolower(status_body$status) == "success") {
        cat("Job completed successfully!\n")
        break # Exit loop when job is successful
      } else if (tolower(status_body$status) %in% c("pending", "processing")){
        cat(".") # Indicate polling
      } else {
        # Handle unexpected status like 'failure'
        error_details <- tryCatch(resp_body_json(resp_status_check), error = function(e) resp_body_string(resp_status_check))
        stop("Parsing Job Failed. Status: ", status_body$status, "\nDetails: ", paste(error_details, collapse="\n"))
      }
    } else if (current_status_code >= 400) {
      # Handle HTTP errors during polling
      error_details <- tryCatch(resp_body_json(resp_status_check), error = function(e) resp_body_string(resp_status_check))
      stop("API Polling Error: ", resp_status_desc(resp_status_check), "\nDetails: ", paste(error_details, collapse="\n"))
    } else {
      cat(".") # Indicate polling for other non-error, non-200 codes if any
    }

    Sys.sleep(poll_interval) # Wait before polling again
  }

  # --- 3. Retrieve the markdown result ---
  req_result <- request(result_url) |>
    req_auth_bearer_token(token = api_key) |>
    req_headers(Accept = "application/json") |>
    req_error(is_error = function(resp) FALSE) # Handle errors manually

  cat("Fetching markdown result...\n")
  resp_result <- req_perform(req_result)

  # Check for result retrieval errors
  if (resp_status(resp_result) >= 400) {
    error_details <- tryCatch(resp_body_json(resp_result), error = function(e) resp_body_string(resp_result))
    stop("API Result Error: ", resp_status_desc(resp_result), "\nDetails: ", paste(error_details, collapse="\n"))
  }

  # Assuming the result is directly in the body as JSON {"markdown": "..."}
  result_data <- resp_body_json(resp_result)
  markdown_content <- result_data$markdown # Adjust if the key name is different

  cat("Markdown retrieved successfully.\n")
  return(markdown_content)
}
