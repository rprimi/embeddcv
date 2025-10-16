library(httr)
library(jsonlite)

# Replace with your actual API token and chosen model
hf_token <- "YOUR_HUGGING_FACE_API_TOKEN"
model_id <- "sentence-transformers/all-MiniLM-L6-v2" # Example embedding model

# Define the API endpoint
api_url <- paste0("https://api-inference.huggingface.co/models/", model_id)

# Text you want to embed
text_to_embed <- "This is a sample sentence for embedding."

# Prepare the request body (JSON format)
request_body <- list(inputs = text_to_embed)

# Make the POST request
response <- POST(
 url = api_url,
 add_headers(Authorization = paste("Bearer", hf_token)),
 body = toJSON(request_body, auto_unbox = TRUE),
 encode = "json"
)

# Check for successful response
if (http_status(response)$category == "Success") {
 embeddings <- content(response, "parsed")
 print("Embeddings received successfully:")
 print(head(embeddings)) # Display the first few elements of the embedding vector
} else {
 stop("API request failed with status: ", http_status(response)$reason)
}