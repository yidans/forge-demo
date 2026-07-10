#!/usr/bin/env Rscript
# Stage I LLM Library Evaluation
# Builds admissible libraries, queries an LLM for proposed terms, and scores precision/recall/off-menu.

suppressPackageStartupMessages({
  library(network)
  library(sna)
  library(jsonlite)
  library(httr)
})

source("stage1_candidate_library.R")
source("consolidated_guardrails.R")
source("benchmark_datasets.R")

# -----------------------------------------------------------------------------
# Helper: attribute metadata summary
# -----------------------------------------------------------------------------
summarize_attributes <- function(net) {
  attrs <- setdiff(list.vertex.attributes(net), c("na", "vertex.names"))
  if (length(attrs) == 0) return("No attributes")
  
  summary_lines <- c()
  for (attr in attrs) {
    values <- get.vertex.attribute(net, attr)
    if (is.numeric(values)) {
      summary_lines <- c(summary_lines, sprintf("%s: numeric (range: %.2f-%.2f)", 
                                               attr, min(values, na.rm=TRUE), max(values, na.rm=TRUE)))
    } else {
      unique_vals <- unique(values[!is.na(values)])
      if (length(unique_vals) <= 10) {
        summary_lines <- c(summary_lines, sprintf("%s: categorical (%s)", 
                                                 attr, paste(unique_vals, collapse=", ")))
      } else {
        summary_lines <- c(summary_lines, sprintf("%s: categorical (%d levels)", 
                                                 attr, length(unique_vals)))
      }
    }
  }
  paste(summary_lines, collapse="; ")
}

# -----------------------------------------------------------------------------
# Helper: structural diagnostics used for prompt context
# -----------------------------------------------------------------------------
get_structural_diagnostics <- function(net) {
  n <- network.size(net)
  m <- network.edgecount(net)
  is_dir <- is.directed(net)
  is_bip <- is.bipartite(net)
  
  density <- if (is_bip) {
    n1 <- sum(get.vertex.attribute(net, "na") == FALSE)
    n2 <- n - n1
    m / (n1 * n2)
  } else {
    m / (n * (n - 1) / (if (is_dir) 1 else 2))
  }
  
  sprintf("Network: %d nodes, %d edges, %s, density=%.3f", 
          n, m, 
          if (is_bip) "bipartite" else if (is_dir) "directed" else "undirected",
          density)
}

# -----------------------------------------------------------------------------
# Prompt builder (system + user)
# -----------------------------------------------------------------------------
build_llm_prompt <- function(net, dyad_covariates = NULL) {
  # Network diagnostics
  struct_diag <- get_structural_diagnostics(net)
  attr_summary <- summarize_attributes(net)
  
  # System prompt
  system_prompt <- paste(
    "You are an expert in Exponential Random Graph Models (ERGMs) for social network analysis.",
    "Your task is to select appropriate ERGM terms for a given network.",
    "",
    "IMPORTANT OUTPUT FORMAT:",
    "You must respond with ONLY valid JSON in this exact format:",
    '{"terms": ["term1", "term2", "term3", ...]}',
    "",
    "Do not include any explanations, comments, or markdown formatting.",
    "Do not wrap the JSON in code blocks or backticks.",
    "Output only the raw JSON object.",
    "",
    "ERGM TERMS REFERENCE:",
    "",
    "STRUCTURAL TERMS:",
    "- edges: basic edge count",
    "- mutual: mutual ties (directed networks only)",
    "- gwesp(decay=X, fixed=TRUE): geometrically weighted edgewise shared partners",
    "- gwdsp(decay=X, fixed=TRUE): geometrically weighted dyadwise shared partners", 
    "- gwdegree(decay=X, fixed=TRUE): geometrically weighted degree",
    "- gwidegree(decay=X, fixed=TRUE): geometrically weighted in-degree (directed)",
    "- gwodegree(decay=X, fixed=TRUE): geometrically weighted out-degree (directed)",
    "- triangle: triangle count",
    "- twopath: two-path count",
    "- ttriple: transitive triple count",
    "- ctriple: cyclic triple count",
    "- kstar(k): k-star count",
    "",
    "ATTRIBUTE TERMS:",
    "- nodematch(\"attr\"): homophily on categorical attribute",
    "- nodefactor(\"attr\"): main effects for categorical attribute",
    "- nodemix(\"attr\"): mixing matrix for categorical attribute",
    "- nodecov(\"attr\"): main effect for numeric attribute",
    "- absdiff(\"attr\"): absolute difference for numeric attribute",
    "- nodeicov(\"attr\"): in-covariate effect (directed)",
    "- nodeocov(\"attr\"): out-covariate effect (directed)",
    "- nodeifactor(\"attr\"): in-factor effect (directed)",
    "- nodeofactor(\"attr\"): out-factor effect (directed)",
    "",
    "GUIDELINES:",
    "- Use decay values of 0.25 and 0.5 for GW terms",
    "- Include both decay values for important GW terms",
    "- For categorical attributes, prefer nodematch over nodefactor",
    "- For numeric attributes, use nodecov and absdiff",
    "- Consider network size and density when selecting terms",
    "- Avoid over-parameterization (too many terms)",
    sep = "\n"
  )
  
  # User prompt
  user_prompt <- paste(
    "Please select appropriate ERGM terms for this network:",
    "",
    struct_diag,
    "Attributes:", attr_summary,
    "",
    "Return your selection as JSON with a 'terms' array containing the ERGM term names.",
    "Be selective and choose terms that are most appropriate for this network structure and attributes.",
    sep = "\n"
  )
  
  list(system = system_prompt, user = user_prompt)
}

# -----------------------------------------------------------------------------
# LLM call helper (OpenRouter example). Returns character vector of terms.
# -----------------------------------------------------------------------------
call_llm_terms <- function(prompt,
                           model,
                           timeout_seconds = 60,
                           temperature = 0.0) {
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  if (!nzchar(api_key)) {
    stop("OPENROUTER_API_KEY not set; cannot call LLM.")
  }

  req_body <- list(
    model = model,
    messages = list(
      list(role = "system", content = prompt$system),
      list(role = "user",   content = prompt$user)
    ),
    temperature = temperature
  )

  resp <- httr::POST(
    url = "https://openrouter.ai/api/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(req_body, auto_unbox = TRUE),
    encode = "raw",
    timeout(timeout_seconds)
  )

  httr::stop_for_status(resp)
  payload <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(payload, simplifyVector = FALSE)

  if (!is.null(parsed$error)) {
    err_msg <- parsed$error$message
    if (is.null(err_msg)) {
      err_msg <- jsonlite::toJSON(parsed$error, auto_unbox = TRUE)
    }
    stop(sprintf("LLM API error: %s", err_msg))
  }

  if (is.null(parsed$choices) || length(parsed$choices) == 0) {
    stop("LLM response missing 'choices'.")
  }

  content <- parsed$choices[[1]]$message$content
  if (is.null(content) || !is.character(content)) {
    stop("LLM response missing message content.")
  }

  # Clean up content for JSON parsing (handle markdown code blocks)
  content <- gsub("```json\\s*", "", content, ignore.case = TRUE)  # Remove ```json prefix
  content <- gsub("```\\s*$", "", content)                        # Remove trailing ```
  content <- gsub("^```\\s*", "", content)                        # Remove leading ```
  content <- gsub("```", "", content)                             # Remove any remaining ```
  content <- trimws(content)                                      # Remove whitespace

  terms_obj <- jsonlite::fromJSON(content, simplifyVector = TRUE)
  if (is.null(terms_obj$terms)) {
    stop("LLM response missing 'terms' field.")
  }
  unique(trimws(terms_obj$terms))
}

# -----------------------------------------------------------------------------
# Normalise LLM terms (quoting, canonical attribute names)
# -----------------------------------------------------------------------------
normalize_llm_terms <- function(terms, net) {
  # Get canonical attribute names
  attrs <- setdiff(list.vertex.attributes(net), c("na", "vertex.names"))
  
  normalized <- character(length(terms))
  for (i in seq_along(terms)) {
    term <- terms[i]
    
    # Handle attribute terms that need quoting
    if (grepl("nodematch\\(|nodefactor\\(|nodemix\\(|nodecov\\(|absdiff\\(|nodeicov\\(|nodeocov\\(|nodeifactor\\(|nodeofactor\\(", term)) {
      # Extract attribute name and quote it
      attr_match <- regexpr("\"[^\"]*\"", term)
      if (attr_match > 0) {
        # Already quoted, keep as is
        normalized[i] <- term
      } else {
        # Extract unquoted attribute name
        unquoted_match <- regexpr("\\([^)]+\\)", term)
        if (unquoted_match > 0) {
          attr_name <- regmatches(term, unquoted_match)
          attr_name <- gsub("[()]", "", attr_name)
          
          # Find matching canonical attribute name (case-insensitive)
          canonical_attr <- NULL
          for (canon_attr in attrs) {
            if (tolower(attr_name) == tolower(canon_attr)) {
              canonical_attr <- canon_attr
              break
            }
          }
          
          if (!is.null(canonical_attr)) {
            # Replace with quoted canonical name
            normalized[i] <- gsub(attr_name, paste0('"', canonical_attr, '"'), term)
          } else {
            # Keep original if no match found
            normalized[i] <- term
          }
        } else {
          normalized[i] <- term
        }
      }
    } else {
      normalized[i] <- term
    }
  }
  
  normalized
}

# -----------------------------------------------------------------------------
# Term canonicalization for robust comparison
# -----------------------------------------------------------------------------
canonicalize_term <- function(s) {
  # Drop all whitespace
  s <- gsub("\\s+", "", s)
  
  # Normalize logical values to uppercase
  s <- gsub("=true\\)", "=TRUE)", s, ignore.case = TRUE)
  s <- gsub("=false\\)", "=FALSE)", s, ignore.case = TRUE)
  
  # Ensure canonical argument order for GW families: decay then fixed
  if (grepl("^gw(esp|dsp|degree|idegree|odegree)\\(", s)) {
    # Extract function name and arguments
    fn_match <- regexpr("^gw(esp|dsp|degree|idegree|odegree)", s)
    fn_name <- regmatches(s, fn_match)
    
    args_match <- regexpr("\\(.*\\)", s)
    args_str <- regmatches(s, args_match)
    args_str <- gsub("[()]", "", args_str)
    
    # Parse arguments
    parts <- strsplit(args_str, ",")[[1]]
    kv <- setNames(sub(".*=", "", parts), tolower(sub("=.*", "", parts)))
    decay <- kv[["decay"]]
    fixed <- kv[["fixed"]]
    if (is.na(fixed)) fixed <- "TRUE"
    
    s <- sprintf("%s(decay=%s,fixed=%s)", fn_name, decay, fixed)
  }
  
  s
}

canonicalize_vec <- function(v) {
  unique(vapply(v, canonicalize_term, "", USE.NAMES = FALSE))
}

# -----------------------------------------------------------------------------
# Evaluation metrics: precision, recall, off-menu rate
# -----------------------------------------------------------------------------
evaluate_library <- function(net, llm_terms, lambda_values = c(0.25, 0.5)) {
  gold <- build_admissible_library(net, lambda_values = lambda_values)
  ref_terms <- unique(gold$terms)

  llm_terms_u <- unique(llm_terms)
  
  # Apply canonicalization to both reference and LLM terms
  ref_terms_norm <- canonicalize_vec(ref_terms)
  llm_terms_norm <- canonicalize_vec(llm_terms_u)

  tp <- sum(llm_terms_norm %in% ref_terms_norm)
  precision <- if (length(llm_terms_norm) == 0) NA_real_ else tp / length(llm_terms_norm)
  recall <- if (length(ref_terms_norm) == 0) NA_real_ else tp / length(ref_terms_norm)
  off_menu <- if (length(llm_terms_norm) == 0) NA_real_ else
    sum(!(llm_terms_norm %in% ref_terms_norm)) / length(llm_terms_norm)

  list(
    precision = precision,
    recall = recall,
    off_menu_rate = off_menu,
    counts = list(
      true_positive = tp,
      false_positive = length(llm_terms_norm) - tp,
      reference_size = length(ref_terms),
      llm_size = length(llm_terms_norm)
    ),
    audit = data.frame(
      term = llm_terms_u,
      in_reference = llm_terms_norm %in% ref_terms_norm,
      stringsAsFactors = FALSE
    ),
    reference_terms = ref_terms
  )
}

# -----------------------------------------------------------------------------
# Main experiment driver
# -----------------------------------------------------------------------------
run_experiment <- function(dataset_names,
                           models,
                           call_llm = TRUE,
                           lambda_values = c(0.25, 0.5)) {

  results <- list()
  llm_available <- call_llm && nzchar(Sys.getenv("OPENROUTER_API_KEY"))
  if (call_llm && !llm_available) {
    message("OPENROUTER_API_KEY not set; proceeding without live LLM calls.")
  }

  for (dataset_name in dataset_names) {
    cat(sprintf("\n=== %s ===\n", toupper(dataset_name)))

    ds <- load_benchmark_dataset(dataset_name)
    net <- ds$network

    reference <- build_admissible_library(net, lambda_values = lambda_values)
    prompt <- build_llm_prompt(net)

    results[[dataset_name]] <- list(
      network = list(
        name = ds$metadata$name,
        nodes = network.size(net),
        edges = network.edgecount(net),
        directed = is.directed(net),
        bipartite = is.bipartite(net),
        attributes = setdiff(list.vertex.attributes(net), c("na", "vertex.names"))
      ),
      reference_library = list(
        terms = reference$terms,
        n_terms = length(reference$terms)
      ),
      models = list()
    )

    for (model in models) {
      cat(sprintf("Model: %s\n", model))
      
      if (llm_available) {
        tryCatch({
          llm_terms_raw <- call_llm_terms(prompt, model)
          llm_terms_norm <- normalize_llm_terms(llm_terms_raw, net)
          eval_result <- evaluate_library(net, llm_terms_norm, lambda_values)
          
          results[[dataset_name]]$models[[model]] <- list(
            llm = list(
              success = TRUE,
              error = NULL,
              model = model
            ),
            terms_raw = llm_terms_raw,
            terms_normalized = llm_terms_norm,
            metrics = eval_result[c("precision", "recall", "off_menu_rate", "counts")],
            audit = eval_result$audit
          )
          
          cat(sprintf("  Received %d terms.\n", length(llm_terms_norm)))
          
        }, error = function(e) {
          results[[dataset_name]]$models[[model]] <- list(
            llm = list(
              success = FALSE,
              error = e$message,
              model = model
            ),
            terms_raw = character(0),
            terms_normalized = character(0),
            metrics = list(
              precision = NA_real_,
              recall = 0.0,
              off_menu_rate = NA_real_,
              counts = list(
                true_positive = 0,
                false_positive = 0,
                reference_size = length(reference$terms),
                llm_size = 0
              )
            ),
            audit = data.frame(
              term = character(0),
              in_reference = logical(0),
              stringsAsFactors = FALSE
            )
          )
          cat(sprintf("  LLM call failed: %s\n", e$message))
        })
      } else {
        # Mock results when LLM not available
        results[[dataset_name]]$models[[model]] <- list(
          llm = list(
            success = FALSE,
            error = "LLM not available",
            model = model
          ),
          terms_raw = character(0),
          terms_normalized = character(0),
          metrics = list(
            precision = NA_real_,
            recall = 0.0,
            off_menu_rate = NA_real_,
            counts = list(
              true_positive = 0,
              false_positive = 0,
              reference_size = length(reference$terms),
              llm_size = 0
            )
          ),
          audit = data.frame(
            term = character(0),
            in_reference = logical(0),
            stringsAsFactors = FALSE
          )
        )
        cat("  LLM not available.\n")
      }
    }
  }

  results
}

# -----------------------------------------------------------------------------
# Main execution
# -----------------------------------------------------------------------------
if (!interactive()) {
  datasets <- c(
    "faux_mesa",
    "faux_dixon", 
    "faux_magnolia",
    "kapferer",
    "lazega",
    "krackhardt",
    "glasgow_s50",
    "manufacturing_emails",
    "enron_emails",
    "florentine",
    "noordin_top",
    "caltech_36"
  )
  
  models <- c(
    "openai/gpt-4o-mini",
    "anthropic/claude-3.5-sonnet",
    "openai/gpt-4o",
    "google/gemini-2.5-pro",
    "meta-llama/llama-3.1-70b-instruct"
  )
  
  results <- run_experiment(datasets, models = models)
  
  # Save results
  jsonlite::write_json(results, "results/admissible_libraries_results.json", 
                       pretty = TRUE, auto_unbox = TRUE)
  saveRDS(results, "results/admissible_libraries_results.rds")
  
  cat("\n✅ Evaluation complete! Results saved to:\n")
  cat("  - results/admissible_libraries_results.json\n")
  cat("  - results/admissible_libraries_results.rds\n")
}
