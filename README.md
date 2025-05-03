
# rAIutils

<!-- badges: start -->
<!-- badges: end -->

rAIutils is a way for me collect various functions for working with generative AI that exist in Python but not (yet) in R. These functions will likely be written mostly by state-of-the-art LLMs with guidance and editing by me. It currently has just one function, `parse_pdf_to_markdown()`, which is an R wrapper around using LlamaCloud's LLamaParse API to turn a PDF file into markdown. It requires an API key.

LlamaParse is a paid service, but there's a generous free tier of 10,000 credits per month. And it's extremely capable for dealing with complex PDF structures.

The standard parser uses 3 credits per page (there's a faster one that's for text only and not tables), which means more than 3,300 pages per month -- enough for me. The major drawback to LlamaParse is if you are working with sensitive data you want to keep private (I've been using it for PDFs already published on the public Web, so not an issue).

See details on how to get a free LlamaCloud API key at [https://docs.cloud.llamaindex.ai/llamacloud/getting_started/api_key](https://docs.cloud.llamaindex.ai/llamacloud/getting_started/api_key).

## Package Installation

rAIutils is currently not on CRAN. However, you can install it from GitHub with the GitHub-install package of your choice, such as `remotes::install_github("smach/rAIutils")` or `pak::pak("smach/rAIutils")`. 


## Example

Here's how you might use rAIutils::markdown_from_file() to parse a PDF on the Web. Parsing a local file is similar, just put the path to the file.

This assumes you've saved your API key in an environment variable called `LLAMA_PARSE_API_KEY`:

```
library(rAIutils)
my_api_key <- Sys.getenv("LLAMA_PARSE_API_KEY")
my_file  <- "https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf"
markdown_from_file <- parse_pdf_to_markdown(input_source = my_file, api_key = my_api_key)
```

Note that the request may take awhile to process. The function checks every 8 seconds to see if the job has finished.

If you want to save results to a file, you can run something like:

`cat(markdown_from_file, file = "RColorBrewer.md")`




