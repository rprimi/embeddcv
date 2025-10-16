### How to Use Google Gemini API
# https://www.listendata.com/2023/12/google-gemini-r.html

### Packages====
require(httr)
require(jsonlite)

### List of all the available models====
api_key <- "XXXXXXX"# Include your API key here! Check: https://aistudio.google.com/app/apikey

models <- GET(url = "https://generativelanguage.googleapis.com/v1beta/models",
              query = list(key = api_key))

lapply(content(models)[["models"]], function(model) c(description = model$description,
                                                      displayName = model$displayName,
                                                      name = model$name,
                                                      method = model$supportedGenerationMethods[1]))

### Function for embedding extraction====
get_embedding_google <- function(prompt, api_key=Sys.getenv("GEMINI_API_KEY"), model = "embedding-001") {

  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }

  model_query <- paste0(model, ":embedContent")

  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      model = paste0("models/",model),
      content = list(
        parts = list(
          list(text = prompt)
        ))
    )
  )

  if(response$status_code>200) {
    stop(paste("Status Code - ", response$status_code))
  }

  return(unlist(content(response)))

}

### Example of texts====
DOCUMENT1 = "AI is like a smart helper in healthcare. It can find problems early by looking at lots of information, help doctors make plans just for you, and even make new medicines faster."
DOCUMENT2 = "AI needs to be open and fair. Sometimes, it can learn things that aren't right. We need to be careful and make sure it's fair for everyone. If AI makes a mistake, someone needs to take responsibility."
DOCUMENT3 = "AI is making school exciting. It can make learning fit you better, help teachers make fun lessons, and show when you need more help."
df <-  data.frame(Text = c(DOCUMENT1, DOCUMENT2, DOCUMENT3))

# Get the embeddings of each text
#embedding_out <- list()
embedding_out <- matrix(NA, nrow=nrow(df), ncol=768)
for(i in 1:nrow(df)) {
  result <- embedding_gemini(prompt = df[i,"Text"])
  #embedding_out[[i]] <- result
  embedding_out[i,] <- result
}

# Identify Most relevant document
query <-  "AI can generate misleading results many times."
scores_query <- embedding_gemini(query)

# Calculate the dot products
#dot_products <- sapply(embedding_out, function(x) sum(x * scores_query))
# Calculate the matrix product
dot_products <- c(embedding_out %*% scores_query)

# Find the index of the maximum product to view the most relevant document
idx <- which.max(dot_products)
df$Text[idx]
