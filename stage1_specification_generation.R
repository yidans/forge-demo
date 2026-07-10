#!/usr/bin/env Rscript
# Stage I Specification Generation (Strategies M4-M6)
# Uses admissible libraries and diagnostics to prompt multiple LLMs for ERGM specifications

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
# Helpers copied/adapted from stage1_llm_library_evaluation.R
# -----------------------------------------------------------------------------

summarize_attributes <- function(net) {
  attrs <- setdiff(list.vertex.attributes(net), c("na", "vertex.names"))
  summary <- lapply(attrs, function(attr) {
    values <- get.vertex.attribute(net, attr)
    info <- classify_attribute(values)
    list(
      raw_type = info$type,
      classification = info$classification,
      unique = info$unique
    )
  })
  names(summary) <- attrs
  summary
}

format_attribute_summary <- function(meta) {
  entries <- vapply(names(meta), function(name) {
    details <- meta[[name]]
    sprintf("%s: classification=%s, unique=%d", name, details$classification, details$unique)
  }, character(1))
  paste(entries, collapse = "; ")
}

diagnostics_summary <- function(net) {
  directed <- is.directed(net)
  gmode <- if (directed) "digraph" else "graph"
  degrees <- sna::degree(net, gmode = gmode)
  list(
    density = network.density(net),
    isolates = sum(degrees == 0, na.rm = TRUE),
    degree_quantiles = as.numeric(stats::quantile(degrees, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)),
    reciprocity = if (directed) sna::grecip(net, measure = "dyadic") else NA_real_,
    clustering = tryCatch(sna::gtrans(net, mode = if (directed) "digraph" else "graph"),
                          error = function(e) NA_real_),
    triangles = tryCatch(summary(net ~ triangle), error = function(e) NA_real_)
  )
}

# -----------------------------------------------------------------------------
# Prompt builders
# -----------------------------------------------------------------------------

build_strategy_prompt <- function(net,
                                  admissible_terms,
                                  meta,
                                  diags,
                                  strategy,
                                  example_spec = NULL,
                                  dyad_covariates = NULL,
                                  system_brief = NULL) {

  directed <- is.directed(net)
  bipartite <- isTRUE(is.bipartite(net))
  n <- network.size(net)

  attr_plain <- format_attribute_summary(meta)
  deg_summary <- paste0("[", paste(format(diags$degree_quantiles, digits = 4), collapse = ", "), "]")
  reciprocity_str <- if (!is.null(diags$reciprocity) && !is.na(diags$reciprocity)) sprintf("%.6f", diags$reciprocity) else "NA"
  clustering_str <- if (!is.null(diags$clustering) && !is.na(diags$clustering)) sprintf("%.6f", diags$clustering) else "NA"

  dyad_list <- if (!is.null(dyad_covariates) && length(dyad_covariates) > 0) {
    paste0("[", paste(sprintf('\"%s\"', names(dyad_covariates)), collapse = ", "), "]")
  } else {
    "[]"
  }

  library_str <- paste(admissible_terms, collapse = ", ")
  strategy_text <- switch(strategy,
    M4 = "Return exactly 4 terms.",
    M5 = "Return exactly 4 terms.",
    M6 = "Return between 3 and 8 terms.",
    stop("Unknown strategy")
  )

  example_block <- if (!is.null(example_spec) && strategy == "M5") {
    paste0("Example specification (JSON, follow structure but choose new terms):\n",
           jsonlite::toJSON(example_spec, auto_unbox = TRUE, pretty = TRUE),
           "\n")
  } else {
    ""
  }

  brief_block <- if (!is.null(system_brief)) {
    sprintf("System brief: %s %s %s\n\n",
            system_brief$actors, system_brief$tie_meaning, system_brief$constraint)
  } else {
    ""
  }

  user_block <- sprintf(
"**Inputs**\n• Network: directed = %s, bipartite = %s, |V| = %d\n• Diagnostics: edges=%d, density=%.6f, degree_quantiles=%s, isolates=%d, reciprocity=%s, clustering=%s\n• Admissible library (use these names exactly): %s\n• Attribute types: %s\n• Dyad covariates for edgecov(): %s\n\n%s**Task**\n- Strategy %s: %s All terms must come from the admissible library.\n\n**Rules**\n- Include edges.\n- Prefer GW families; do not use triangle.\n- Respect directed/undirected constraints and attribute types.\n- Use nodemix only if group-size gate is satisfied.\n- Avoid redundant terms unless diagnostics justify them.\n- Provide expected sign (+/-) for each non-edge term.\n- Do not copy attribute names from the example; use only names listed in the admissible library.\n\n%s**Output JSON ONLY**\n{\n  \"specifications\": [{\n    \"formula\": [ ... ],\n    \"expected_effects\": { ... },\n    \"rationale\": { ... }\n  }]\n}\n",
    ifelse(directed, "true", "false"),
    ifelse(bipartite, "true", "false"),
    n,
    network.edgecount(net),
    diags$density,
    deg_summary,
    diags$isolates,
    reciprocity_str,
    clustering_str,
    library_str,
    attr_plain,
    dyad_list,
    brief_block,
    strategy,
    strategy_text,
    example_block
  )

  list(
    system = 'You are an expert network scientist for ERGM specification. You have an admissible term library and diagnostics. Produce valid JSON only.',
    user = user_block
  )
}

# -----------------------------------------------------------------------------
# LLM call helper for specifications
# -----------------------------------------------------------------------------

call_llm_specifications <- function(prompt,
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

  content <- gsub("```json\\s*", "", content, ignore.case = TRUE)
  content <- gsub("```\\s*$", "", content)
  content <- gsub("^```\\s*", "", content)
  content <- gsub("```", "", content)
  content <- trimws(content)

  spec_obj <- jsonlite::fromJSON(content, simplifyVector = FALSE)
  specs <- spec_obj$specifications
  if (is.null(specs) || length(specs) == 0) {
    stop("LLM response missing 'specifications'.")
  }

  spec <- specs[[1]]
  list(
    formula = unlist(spec$formula),
    expected_effects = spec$expected_effects,
    rationale = spec$rationale,
    raw = spec
  )
}

# -----------------------------------------------------------------------------
# Canonicalisation utilities
# -----------------------------------------------------------------------------

canonicalize_term <- function(s) {
  s <- gsub("\\s+", "", s)
  s <- sub("^\\+", "", s)
  s <- gsub("=true\\)", "=TRUE)", s, ignore.case = TRUE)
  s <- gsub("=false\\)", "=FALSE)", s, ignore.case = TRUE)

  if (grepl("^gw(esp|dsp|degree|idegree|odegree|b1degree|b2degree)\\(", s)) {
    fn_match <- regexpr("^gw(esp|dsp|degree|idegree|odegree|b1degree|b2degree)", s)
    fn_name <- regmatches(s, fn_match)

    args_match <- regexpr("\\(.*\\)", s)
    args_str <- regmatches(s, args_match)
    args_str <- gsub("[()]", "", args_str)

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
# Specification parsing helpers
# -----------------------------------------------------------------------------

normalize_formula_terms <- function(terms, attr_names) {
  if (length(terms) == 0) return(terms)
  attr_lower <- tolower(attr_names)

  normalize_single <- function(term) {
    t <- trimws(term)
    t <- gsub("\\s+", "", t)
    t <- sub("^\\+", "", t)

    pattern <- '^([a-z_]+)\\((.*)\\)$'
    if (!grepl(pattern, t, ignore.case = TRUE)) return(t)
    matches <- regexec(pattern, t, ignore.case = TRUE)
    parts <- regmatches(t, matches)[[1]]
    if (length(parts) < 3) return(t)
    fn <- tolower(parts[2])
    args <- parts[3]

    attr_functions <- c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor",
                        "nodecov", "nodeicov", "nodeocov", "absdiff", "edgecov")
    if (!(fn %in% attr_functions)) return(t)
    if (grepl("=", args)) return(t)

    arg_clean <- gsub("^['\"]|['\"]$", "", args)
    match_idx <- match(tolower(arg_clean), attr_lower)
    if (!is.na(match_idx)) {
      canonical <- attr_names[match_idx]
    } else {
      canonical <- arg_clean
    }
    sprintf("%s(\"%s\")", fn, canonical)
  }

  vapply(terms, normalize_single, character(1), USE.NAMES = FALSE)
}

extract_attribute_from_term <- function(term) {
  pattern <- "^([a-z_]+)\\(\\\"([^\\\"]+)\\\".*\\)$"
  if (!grepl(pattern, term, ignore.case = TRUE)) return(NA_character_)
  matches <- regexec(pattern, term, ignore.case = TRUE)
  parts <- regmatches(term, matches)[[1]]
  if (length(parts) < 3) return(NA_character_)
  parts[3]
}

extract_nodemix_attr <- function(term) {
  first_quote <- regexpr('"', term, fixed = TRUE)
  if (first_quote == -1) return(term)
  remainder <- substr(term, first_quote + 1, nchar(term))
  second_quote <- regexpr('"', remainder, fixed = TRUE)
  if (second_quote == -1) return(term)
  substr(remainder, 1, second_quote - 1)
}

get_term_function <- function(term) {
  sub("\\(.*", "", term)
}

# -----------------------------------------------------------------------------
# System briefs per dataset
# -----------------------------------------------------------------------------

system_briefs_map <- list(
  "faux_mesa" = list(
    actors = "Actors are students.",
    tie_meaning = "A tie means mutual friendship.",
    constraint = "Shared classes and clubs encourage local connections."
  ),
  "faux_dixon" = list(
    actors = "Actors are students.",
    tie_meaning = "A tie means mutual friendship.",
    constraint = "Grade cohorts and activities shape who meets."
  ),
  "faux_magnolia" = list(
    actors = "Actors are students.",
    tie_meaning = "A tie means mutual friendship.",
    constraint = "Most ties form within grade and social groups."
  ),
  "kapferer" = list(
    actors = "Actors are workers in a tailor shop.",
    tie_meaning = "A tie means sustained work interaction.",
    constraint = "The released network has limited node attributes, so structural mechanisms dominate."
  ),
  "lazega" = list(
    actors = "Actors are lawyers in one firm.",
    tie_meaning = "A tie means A seeks advice from B.",
    constraint = "Office and practice teams coordinate work."
  ),
  "krackhardt" = list(
    actors = "Actors are managers in a tech firm.",
    tie_meaning = "A tie means A seeks advice from B.",
    constraint = "Status and tenure create hubs."
  ),
  "glasgow_s50" = list(
    actors = "Actors are pupils in one school.",
    tie_meaning = "A tie means a friendship nomination (A→B).",
    constraint = "Classes and activities raise closure."
  ),
  "manufacturing_emails" = list(
    actors = "Actors are employees in a manufacturing company.",
    tie_meaning = "A tie means A sent email to B.",
    constraint = "Communication is directed and the released network has no substantive node attributes."
  ),
  "enron_emails" = list(
    actors = "Actors are employees.",
    tie_meaning = "A tie means A sent an email to B.",
    constraint = "Reply habits and hierarchy affect traffic."
  ),
  "florentine" = list(
    actors = "Actors are patrician families.",
    tie_meaning = "A tie means a marriage alliance.",
    constraint = "Wealth and offices guide alliances."
  ),
  "noordin_top" = list(
    actors = "Actors are operatives and associates.",
    tie_meaning = "A tie means association or collaboration.",
    constraint = "Cell structure and shared training increase within-group ties."
  ),
  "caltech_36" = list(
    actors = "Actors are students.",
    tie_meaning = "A tie means Facebook friendship.",
    constraint = "Residential houses and year groups cluster ties."
  )
)

get_system_brief <- function(dataset_name) {
  key <- tolower(dataset_name)
  if (key %in% names(system_briefs_map)) system_briefs_map[[key]] else NULL
}

# -----------------------------------------------------------------------------
# Smell test evaluation
# -----------------------------------------------------------------------------

smell_test <- function(spec_terms,
                       strategy,
                       library_terms,
                       attr_meta,
                       diags,
                       expected_effects) {
  reasons <- character(0)
  terms_norm <- canonicalize_vec(spec_terms)
  library_norm <- canonicalize_vec(library_terms)

  # Rule 1: size constraints
  count <- length(terms_norm)
  if (strategy %in% c("M4", "M5")) {
    if (count != 4) reasons <- c(reasons, sprintf("Expected 4 terms, got %d", count))
  } else if (strategy == "M6") {
    if (count < 3 || count > 8) reasons <- c(reasons, sprintf("Expected 3-8 terms, got %d", count))
  }

  # Rule 2: edges included
  if (!any(tolower(terms_norm) == "edges")) {
    reasons <- c(reasons, "Missing edges term")
  }

  # Rule 3: terms inside library
  if (any(!(terms_norm %in% library_norm))) {
    invalid <- terms_norm[!(terms_norm %in% library_norm)]
    reasons <- c(reasons, sprintf("Off-library terms: %s", paste(invalid, collapse = ", ")))
  }

  # Rule 4: duplicates
  if (any(duplicated(terms_norm))) {
    dups <- unique(terms_norm[duplicated(terms_norm)])
    reasons <- c(reasons, sprintf("Duplicate terms: %s", paste(dups, collapse = ", ")))
  }

  # Rule 5: structural coverage
  structural_terms <- c("gwesp", "gwdsp", "gwdegree", "gwidegree", "gwodegree", "gwb1degree", "gwb2degree", "mutual", "twopath")
  has_structural <- any(vapply(structural_terms, function(prefix) {
    any(startsWith(tolower(terms_norm), prefix))
  }, logical(1)))
  if (!has_structural) {
    reasons <- c(reasons, "No structural term beyond edges")
  }

  # Rule 6: closure expectation when clustering high
  closure_needed <- FALSE
  if (!is.null(diags$clustering) && !is.na(diags$clustering) && diags$clustering >= 0.2) {
    closure_needed <- TRUE
  }
  if (!is.null(diags$triangles) && !is.na(diags$triangles) && diags$triangles > 0) {
    closure_needed <- TRUE
  }
  if (closure_needed) {
    has_closure <- any(startsWith(tolower(terms_norm), c("gwesp", "gwdsp")))
    if (!has_closure) {
      reasons <- c(reasons, "Diagnostics suggest clustering but no gwesp/gwdsp term present")
    }
  }

  # Rule 7: degree-shape expectation for heavy tails
  heavy_tail <- FALSE
  if (!is.null(diags$degree_quantiles) && !any(is.na(diags$degree_quantiles))) {
    dq <- diags$degree_quantiles
    if (length(dq) >= 5) {
      heavy_tail <- dq[5] >= dq[4] + 2 && dq[5] >= 4
    }
  }
  if (heavy_tail) {
    degree_present <- any(startsWith(tolower(terms_norm), c("gwdegree", "gwidegree", "gwodegree")))
    if (!degree_present) {
      reasons <- c(reasons, "Heavy degree tail detected but no gwdegree/gwidegree term included")
    }
  }

  # Rule 8: attribute type correctness
  attr_functions <- c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor",
                      "nodecov", "nodeicov", "nodeocov", "absdiff")
  for (term in terms_norm) {
    fn <- tolower(get_term_function(term))
    if (fn %in% attr_functions) {
      attr <- extract_attribute_from_term(term)
      if (is.na(attr)) next
      attr_info <- attr_meta[[attr]]
      if (is.null(attr_info)) {
        reasons <- c(reasons, sprintf("Unknown attribute in term %s", term))
      } else {
        if (fn %in% c("nodecov", "nodeicov", "nodeocov", "absdiff") && attr_info$classification != "numeric") {
          reasons <- c(reasons, sprintf("Numeric term %s used with categorical attribute", term))
        }
        if (fn %in% c("nodematch", "nodemix", "nodefactor", "nodeifactor", "nodeofactor") && attr_info$classification != "categorical") {
          reasons <- c(reasons, sprintf("Categorical term %s used with numeric attribute", term))
        }
      }
    }
  }

  # Rule 9: expected effects presence
  if (is.null(expected_effects)) {
    reasons <- c(reasons, "Missing expected effects block")
  } else {
    effect_names_raw <- names(expected_effects)
    effect_names <- if (is.null(effect_names_raw)) character(0) else canonicalize_vec(effect_names_raw)
    effects_valid <- all(vapply(expected_effects, function(x) x %in% c("+", "-"), logical(1)))
    if (!effects_valid) {
      reasons <- c(reasons, "Expected effects must be '+' or '-'")
    }
    non_edge_terms <- setdiff(terms_norm, "edges")
    missing_effects <- setdiff(non_edge_terms, effect_names)
    if (length(missing_effects) > 0) {
      reasons <- c(reasons, sprintf("Missing expected effect for: %s", paste(missing_effects, collapse = ", ")))
    }
  }

  list(pass = length(reasons) == 0, reasons = reasons)
}

# -----------------------------------------------------------------------------
# Main specification generation driver
# -----------------------------------------------------------------------------
generate_specifications <- function(dataset_names,
                                    models,
                                    strategies = c("M4", "M5", "M6"),
                                    call_llm = TRUE,
                                    lambda_values = c(0.25, 0.5)) {

  if (!file.exists("results/admissible_libraries_results.rds")) {
    stop("Missing results/admissible_libraries_results.rds. Run stage1 candidate library pipeline first.")
  }
  admissible_results <- readRDS("results/admissible_libraries_results.rds")
  
  # Extract candidate libraries from the results
  candidate_libraries <- lapply(admissible_results, function(x) {
    list(terms = x$reference_library$terms)
  })

  if (!file.exists("results/dataset_diagnostics_summary.rds")) {
    warning("Missing results/dataset_diagnostics_summary.rds. Diagnostics will be recomputed on the fly.")
    diagnostics_cached <- list()
  } else {
    diagnostics_cached <- readRDS("results/dataset_diagnostics_summary.rds")
  }

  results <- list()
  llm_available <- call_llm && nzchar(Sys.getenv("OPENROUTER_API_KEY"))
  if (call_llm && !llm_available) {
    message("OPENROUTER_API_KEY not set; proceeding without live LLM calls.")
  }

  for (dataset_name in dataset_names) {
    cat(sprintf("\n=== %s (Stage I Specifications) ===\n", toupper(dataset_name)))

    ds <- load_benchmark_dataset(dataset_name)
    net <- ds$network

    # Admissible library from stored results; if missing fallback to on-the-fly
    if (!dataset_name %in% names(candidate_libraries)) {
      reference <- build_admissible_library(net, lambda_values = lambda_values)
    } else {
      reference <- candidate_libraries[[dataset_name]]
    }

    meta <- summarize_attributes(net)
    if (dataset_name %in% names(diagnostics_cached)) {
      diags <- diagnostics_cached[[dataset_name]]
    } else {
      diags <- diagnostics_summary(net)
    }

    attr_names <- names(meta)

    # Build example specification for M5 using actual attributes when available
    categorical_attrs <- attr_names[vapply(attr_names, function(x) meta[[x]]$classification == "categorical" && meta[[x]]$unique >= 2, logical(1))]
    numeric_attrs <- attr_names[vapply(attr_names, function(x) meta[[x]]$classification == "numeric" && meta[[x]]$unique >= 2, logical(1))]
    categorical_all <- attr_names[vapply(attr_names, function(x) meta[[x]]$classification == "categorical", logical(1))]

    # Helper to check if a term is in the admissible library
    has_term <- function(term) term %in% reference$terms

    closure_term <- if (has_term("gwesp(decay=0.25, fixed=TRUE)")) {
      "gwesp(decay=0.25, fixed=TRUE)"
    } else if (has_term("gwesp(decay=0.5, fixed=TRUE)")) {
      "gwesp(decay=0.5, fixed=TRUE)"
    } else if (has_term("gwdsp(decay=0.25, fixed=TRUE)")) {
      "gwdsp(decay=0.25, fixed=TRUE)"
    } else {
      NULL
    }

    degree_term <- if (is.directed(net)) {
      if (has_term("gwidegree(decay=0.25, fixed=TRUE)")) {
        "gwidegree(decay=0.25, fixed=TRUE)"
      } else if (has_term("gwodegree(decay=0.25, fixed=TRUE)")) {
        "gwodegree(decay=0.25, fixed=TRUE)"
      } else {
        NULL
      }
    } else {
      if (has_term("gwdegree(decay=0.25, fixed=TRUE)")) {
        "gwdegree(decay=0.25, fixed=TRUE)"
      } else if (has_term("gwdegree(decay=0.5, fixed=TRUE)")) {
        "gwdegree(decay=0.5, fixed=TRUE)"
      } else {
        NULL
      }
    }

    attribute_term <- NULL
    attribute_effect <- NULL
    attribute_rationale <- NULL
    if (length(categorical_attrs) > 0) {
      attr_candidate <- categorical_attrs[[1]]
      nm_term <- sprintf("nodematch(\"%s\")", attr_candidate)
      if (has_term(nm_term)) {
        attribute_term <- nm_term
        attribute_effect <- list(nm_term = "+")
        attribute_rationale <- list(nm_term = sprintf("assortativity by %s", attr_candidate))
      }
    }
    if (is.null(attribute_term) && length(numeric_attrs) > 0) {
      attr_candidate <- numeric_attrs[[1]]
      nc_term <- sprintf("nodecov(\"%s\")", attr_candidate)
      if (has_term(nc_term)) {
        attribute_term <- nc_term
        attribute_effect <- list(nc_term = "+")
        attribute_rationale <- list(nc_term = sprintf("value effect of %s", attr_candidate))
      }
    }

    example_formula <- c("edges")
    example_effects <- list()
    example_rationale <- list()

    if (!is.null(closure_term)) {
      example_formula <- c(example_formula, closure_term)
      example_effects[[closure_term]] <- "+"
      example_rationale[[closure_term]] <- "captures clustering"
    }
    if (!is.null(degree_term)) {
      example_formula <- c(example_formula, degree_term)
      example_effects[[degree_term]] <- "+"
      example_rationale[[degree_term]] <- "controls for skewed degree"
    }
    if (!is.null(attribute_term)) {
      example_formula <- c(example_formula, attribute_term)
      example_effects <- c(example_effects, attribute_effect)
      example_rationale <- c(example_rationale, attribute_rationale)
    }

    # Ensure example has at least 3 terms; if not, pad with available structural terms from library
    additional_terms <- setdiff(reference$terms, example_formula)
    structural_pool <- grep("^gw", additional_terms, value = TRUE)
    target_size <- 4
    for (term in head(structural_pool, max(0, target_size - length(example_formula)))) {
      example_formula <- c(example_formula, term)
      example_effects[[term]] <- "+"
      example_rationale[[term]] <- "structural support"
    }

    example_spec <- list(
      specifications = list(list(
        formula = example_formula,
        expected_effects = example_effects,
        rationale = example_rationale
      ))
    )

    results[[dataset_name]] <- list(
      network = list(
        name = ds$metadata$name,
        nodes = network.size(net),
        directed = is.directed(net)
      ),
      admissible_library = reference$terms,
      attribute_details = meta,
      nodemix_allowed = grep("^nodemix\\(", reference$terms, value = TRUE),
      nodemix_allowed_attrs = {
        allowed_terms <- grep("^nodemix\\(", reference$terms, value = TRUE)
        if (length(allowed_terms) == 0) character(0) else vapply(allowed_terms, extract_nodemix_attr, character(1))
      },
      nodemix_blocked_attrs = {
        allowed_terms <- grep("^nodemix\\(", reference$terms, value = TRUE)
        allowed_attrs <- if (length(allowed_terms) == 0) character(0) else vapply(allowed_terms, extract_nodemix_attr, character(1))
        setdiff(categorical_all, allowed_attrs)
      },
      diagnostics = diags,
      models = list()
    )

    system_brief <- get_system_brief(dataset_name)

    if (!is.null(system_brief)) {
      results[[dataset_name]]$system_brief <- system_brief
    }

    for (model_id in models) {
      cat(sprintf("Model: %s\n", model_id))
      results[[dataset_name]]$models[[model_id]] <- list()

      for (strategy in strategies) {
        cat(sprintf("  Strategy %s\n", strategy))
        spec_result <- list(success = FALSE, error = NULL)

        if (llm_available) {
          prompt <- build_strategy_prompt(
            net = net,
            admissible_terms = reference$terms,
            meta = meta,
            diags = diags,
            strategy = strategy,
            example_spec = example_spec,
            dyad_covariates = NULL,
            system_brief = system_brief
          )

          tryCatch({
            llm_spec <- call_llm_specifications(prompt, model = model_id, temperature = 0.0)
            # Normalize formula terms
            normalized_formula <- normalize_formula_terms(llm_spec$formula, attr_names)
            canonical_formula <- canonicalize_vec(normalized_formula)
            smell <- smell_test(
              spec_terms = canonical_formula,
              strategy = strategy,
              library_terms = reference$terms,
              attr_meta = meta,
              diags = diags,
              expected_effects = llm_spec$expected_effects
            )

            spec_result <- list(
              success = TRUE,
              formula_raw = llm_spec$formula,
              formula_normalized = normalized_formula,
              formula_canonical = canonical_formula,
              expected_effects = llm_spec$expected_effects,
              rationale = llm_spec$rationale,
              smell_test = smell,
              prompt = prompt
            )
          }, error = function(e) {
            spec_result <- list(
              success = FALSE,
              error = e$message,
              formula_raw = character(0),
              formula_normalized = character(0),
              formula_canonical = character(0),
              expected_effects = list(),
              rationale = character(0),
              smell_test = list(),
              prompt = prompt
            )
            cat(sprintf("    LLM call failed: %s\n", e$message))
          })
        } else {
          spec_result$error <- "LLM call skipped"
        }

        results[[dataset_name]]$models[[model_id]][[strategy]] <- spec_result
      }
    }
  }

  if (!dir.exists("results")) dir.create("results", recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(results,
                       path = file.path("results", "stage1_specifications_results.json"),
                       auto_unbox = TRUE,
                       pretty = TRUE)
  saveRDS(results, file.path("results", "stage1_specifications_results.rds"))

  invisible(results)
}

if (identical(environment(), globalenv()) && !interactive()) {
  datasets <- c(
    "faux_mesa", "faux_dixon", "faux_magnolia", "kapferer", "lazega",
    "krackhardt", "glasgow_s50", "manufacturing_emails", "enron_emails", "florentine",
    "noordin_top", "caltech_36"
  )
  models <- c(
    "openai/gpt-4o",
    "openai/gpt-4o-mini",
    "anthropic/claude-3.5-sonnet",
    "meta-llama/llama-3.1-70b-instruct",
    "google/gemini-2.5-pro"
  )
  generate_specifications(datasets, models = models)
}
