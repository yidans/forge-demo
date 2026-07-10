#!/usr/bin/env Rscript
# Stage III refinement pipeline: single-edit iterative loop with LLM guidance.
# Canonical refinement implementation for the supported small-network set.

suppressPackageStartupMessages({
  library(ergm)
  library(network)
  library(jsonlite)
  library(httr)
  library(R.utils)
  library(utils)
})

if (file.exists(".env")) {
  try(readRenviron(".env"), silent = TRUE)
}

source("benchmark_datasets.R")
source("consolidated_guardrails.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

STAGE1_RDS <- "results/stage1_specifications_results.rds"
STAGE2_BASELINE_RDS <- "results/stage2_baseline_specifications.rds"
STAGE2_MPLE_CSV <- "results/stage2_step1_mple_results.csv"
STAGE3_RESULTS_CSV <- "results/stage3_mcmle_results.csv"
STAGE3_MODELS_DIR <- "results/stage3_models"
REFINEMENT_HISTORY_RDS <- "results/stage3_refinement_history.rds"
REFINEMENT_HISTORY_JSON <- "results/stage3_refinement_history.json"
REFINEMENT_SUMMARY_CSV <- "results/stage3_refinement_summary.csv"
USE_PREBUILT_LIB <- TRUE
SPEC_LIB_RDS <- "results/stage2_spec_libraries.rds"

# Focus on smaller datasets only
SMALL_DATASETS <- c("florentine", "krackhardt", "glasgow_s50", "lazega", "noordin_top")

MAX_ROUNDS <- 8
MAX_CONSECUTIVE_REJECTIONS <- 6
GOF_PASS_THRESHOLD <- 2.5
GOF_STRICT_THRESHOLD <- 2.0
STAGE2_GOF_THRESHOLD <- 2.5
RELAX_AFTER_ATTEMPTS <- 3  # Relax guardrails after this many rejections

MCMLE_CONTROL <- control.ergm(
  seed = NULL,
  init.method = "MPLE",
  MCMLE.maxit = 12,
  MCMC.burnin = 1e5,
  MCMC.interval = 1e3,
  MCMC.samplesize = 5e3,
  MCMLE.steplength = 0.5
)
MCMLE_TIMEOUT <- 900

TRY_LADDER <- list(
  list(
    ctrl = list(
      init.method = "MPLE",
      MCMLE.maxit = 6,
      MCMC.burnin = 20000,
      MCMC.interval = 1000,
      MCMC.samplesize = 1500,
      MCMLE.steplength = 0.4
    ),
    timeout = 300
  ),
  list(
    ctrl = list(
      init.method = "MPLE",
      MCMLE.maxit = 10,
      MCMC.burnin = 50000,
      MCMC.interval = 1000,
      MCMC.samplesize = 3000,
      MCMLE.steplength = 0.4
    ),
    timeout = 600
  ),
  list(
    ctrl = list(
      init.method = "MPLE",
      MCMLE.maxit = 12,
      MCMC.burnin = 1e5,
      MCMC.interval = 1000,
      MCMC.samplesize = 5000,
      MCMLE.steplength = 0.3
    ),
    timeout = 900
  )
)

# Network cache for efficiency
network_cache <- new.env(parent = emptyenv())

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

sanitize_terms <- function(terms) {
  cleaned <- trimws(terms)
  cleaned <- gsub("`", "", cleaned, fixed = TRUE)
  cleaned <- cleaned[nzchar(cleaned)]
  unique(cleaned)
}

extract_terms_field <- function(entry) {
  if (!is.null(entry$terms) && length(entry$terms) > 0) {
    return(sanitize_terms(entry$terms))
  }
  if (!is.null(entry$formula) && length(entry$formula) > 0) {
    return(sanitize_terms(entry$formula))
  }
  character(0)
}

extract_error <- function(obj) {
  if (inherits(obj, "try-error")) {
    cond <- attr(obj, "condition")
    if (!is.null(cond)) {
      return(conditionMessage(cond))
    }
    return(as.character(obj)[1])
  }
  "unknown error"
}

ensure_edges_term <- function(terms) {
  t <- sanitize_terms(terms)
  if (!"edges" %in% t) {
    t <- c("edges", t)
  }
  t
}

terms_to_formula <- function(terms, lhs = "g") {
  rhs <- paste(unique(terms), collapse = " + ")
  as.formula(paste(lhs, "~", rhs))
}

get_network <- function(dataset) {
  if (exists(dataset, envir = network_cache, inherits = FALSE)) {
    return(get(dataset, envir = network_cache, inherits = FALSE))
  }
  net <- suppressMessages(load_benchmark_dataset(dataset)$network)
  assign(dataset, net, envir = network_cache)
  net
}

build_spec_libraries <- function(stage1_results, stage2_baselines) {
  libs <- list(
    M2_randomK = list(),
    M3_null = list(),
    M4_oneshot = list(),
    M5_fewshot = list(),
    M6_unconstrained = list()
  )
  
  for (dataset in names(stage1_results)) {
    s1_entry <- stage1_results[[dataset]]
    s2_entry <- stage2_baselines[[dataset]]
    
    # Baseline models
    libs$M2_randomK[[dataset]] <- list(
      list(
        source = "baseline_random_k",
        terms = extract_terms_field(s2_entry$baselines$M2_random_k),
        id = sprintf("%s__M2_randomK__01", dataset)
      )
    )
    
    libs$M3_null[[dataset]] <- list(
      list(
        source = "baseline_null",
        terms = extract_terms_field(s2_entry$baselines$M3_null),
        id = sprintf("%s__M3_null__01", dataset)
      )
    )
    
    # LLM models
    for (model_name in names(s1_entry$models)) {
      model <- s1_entry$models[[model_name]]
      
      libs$M4_oneshot[[dataset]] <- c(
        libs$M4_oneshot[[dataset]],
        list(list(
          source = model_name,
          terms = sanitize_terms(model$M4$formula_canonical)
        ))
      )
      
      libs$M5_fewshot[[dataset]] <- c(
        libs$M5_fewshot[[dataset]],
        list(list(
          source = model_name,
          terms = sanitize_terms(model$M5$formula_canonical)
        ))
      )
      
      libs$M6_unconstrained[[dataset]] <- c(
        libs$M6_unconstrained[[dataset]],
        list(list(
          source = model_name,
          terms = sanitize_terms(model$M6$formula_canonical)
        ))
      )
    }
    
    # Deduplicate and assign IDs
    for (method in names(libs)) {
      specs <- libs[[method]][[dataset]]
      if (is.null(specs) || !length(specs)) next
      
      # Deduplicate by term signature
      keys <- vapply(specs, function(entry) {
        paste(entry$terms, collapse = "|")
      }, character(1))
      keep <- !duplicated(keys)
      specs <- specs[keep]
      
      # Assign IDs
      for (idx in seq_along(specs)) {
        specs[[idx]]$id <- sprintf("%s__%s__%02d", dataset, method, idx)
      }
      
      libs[[method]][[dataset]] <- specs
    }
  }
  
  libs
}

get_spec_terms <- function(spec_id, spec_libs) {
  parts <- strsplit(spec_id, "__")[[1]]
  if (length(parts) < 3) return(character(0))
  
  dataset <- parts[1]
  method <- if (length(parts) == 3) {
    parts[2]
  } else {
    paste(parts[2:(length(parts)-1)], collapse = "__")
  }
  
  specs <- spec_libs[[method]][[dataset]]
  if (is.null(specs)) return(character(0))
  
  idx <- match(spec_id, vapply(specs, function(x) x$id, character(1)))
  if (is.na(idx)) return(character(0))
  
  specs[[idx]]$terms
}

get_admissible_library <- function(dataset, stage2_baselines) {
  entry <- stage2_baselines[[dataset]]
  terms <- sanitize_terms(entry$admissible_library)
  # Remove any NA or empty terms
  terms[!is.na(terms) & nzchar(terms)]
}

validate_candidate_terms <- function(terms, admissible, net, relax_mode = FALSE) {
  candidate <- ensure_edges_term(terms)
  
  # Check if all terms are in admissible library
  if (!all(candidate %in% admissible)) {
    return(list(
      valid = FALSE,
      error = "Proposed terms not entirely in admissible library"
    ))
  }
  
  # Check guardrails (skip when in relaxed mode after many failed attempts)
  if (!relax_mode) {
    # Normal mode: strict guardrails
    guard <- try(check_all_guardrails(candidate, net, admissible), silent = TRUE)
    if (!inherits(guard, "try-error") && !guard$all_passed) {
      return(list(
        valid = FALSE,
        error = "Guardrail check failed",
        details = guard
      ))
    }
  }
  # If relax_mode = TRUE, we skip guardrail checks entirely (calculated risk)
  
  list(valid = TRUE, terms = candidate)
}

compute_gof_summary <- function(fit, net) {
  # Use appropriate GOF statistics based on network type
  is_directed <- is.directed(net)
  
  if (is_directed) {
    # For directed networks, use idegree, odegree, and simpler statistics
    gof_stats <- try(gof(fit, GOF = ~ idegree + odegree + distance, verbose = FALSE), silent = TRUE)
  } else {
    # For undirected networks, use standard statistics
    gof_stats <- try(gof(fit, GOF = ~ degree + espartners + distance, verbose = FALSE), silent = TRUE)
  }
  
  detail_df <- data.frame(
    component = character(0),
    stat = character(0),
    z = numeric(0),
    stringsAsFactors = FALSE
  )
  
  if (!inherits(gof_stats, "try-error")) {
    # Extract z-scores from gof object using the proper field names
    # Try multiple component prefixes depending on network type
    prefixes <- if (is_directed) {
      c('ideg', 'odeg', 'dist')
    } else {
      c('deg', 'espart', 'dist')
    }
    
    for (comp_prefix in prefixes) {
      obs_name <- paste0('obs.', comp_prefix)
      psim_name <- paste0('psim.', comp_prefix)
      sim_name <- paste0('sim.', comp_prefix)
      
      if (!is.null(gof_stats[[obs_name]]) && 
          !is.null(gof_stats[[psim_name]]) && 
          !is.null(gof_stats[[sim_name]])) {
        
        obs <- gof_stats[[obs_name]]
        mean_sim <- gof_stats[[psim_name]]
        sim_matrix <- gof_stats[[sim_name]]
        
        # Compute SD from simulation matrix
        sd_sim <- apply(sim_matrix, 2, sd)
        
        # Compute z-scores
        z <- (obs - mean_sim) / sd_sim
        
        # Filter: only keep finite z-scores
        valid <- is.finite(z)
        
        if (any(valid)) {
          stat_names <- names(obs)
          if (is.null(stat_names)) {
            stat_names <- paste0(comp_prefix, seq_along(obs))
          }
          
          detail_df <- rbind(detail_df, data.frame(
            component = comp_prefix,
            stat = stat_names[valid],
            z = z[valid],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  max_abs_z <- if (nrow(detail_df)) max(abs(detail_df$z)) else NA_real_
  
  list(
    gof_object = gof_stats,
    max_abs_z = max_abs_z,
    pass = is.finite(max_abs_z) && max_abs_z <= GOF_PASS_THRESHOLD,
    strict_pass = is.finite(max_abs_z) && max_abs_z < GOF_STRICT_THRESHOLD,
    details = detail_df
  )
}

collect_diagnostics <- function(fit, net) {
  summary_fit <- summary(fit)
  coef_table <- summary_fit$coefficients
  gof_summary <- compute_gof_summary(fit, net)
  
  mcmc_diag <- try(capture.output(mcmc.diagnostics(fit)), silent = TRUE)
  
  list(
    coefficients = coef_table,
    gof = gof_summary,
    mcmc = if (inherits(mcmc_diag, "try-error")) character(0) else mcmc_diag,
    bic = tryCatch(BIC(fit), error = function(e) NA_real_),
    aic = tryCatch(AIC(fit), error = function(e) NA_real_)
  )
}

serialize_diagnostics <- function(diag) {
  coeff_df <- if (!is.null(diag$coefficients)) {
    data.frame(
      term = rownames(diag$coefficients),
      estimate = diag$coefficients[, "Estimate"],
      std_error = diag$coefficients[, "Std. Error"],
      row.names = NULL
    )
  } else {
    data.frame()
  }
  
  list(
    bic = diag$bic,
    aic = diag$aic,
    max_abs_z = diag$gof$max_abs_z,
    gof_pass = diag$gof$pass,
    strict_pass = diag$gof$strict_pass,
    coefficients = coeff_df,
    mcmc = diag$mcmc
  )
}

lexicographic_better <- function(candidate, reference) {
  # First criterion: GOF pass
  if (isTRUE(candidate$gof$pass) && !isTRUE(reference$gof$pass)) {
    return(TRUE)
  }
  if (!isTRUE(candidate$gof$pass) && isTRUE(reference$gof$pass)) {
    return(FALSE)
  }
  
  # Second criterion: max absolute z-score
  cand_max <- candidate$gof$max_abs_z
  ref_max <- reference$gof$max_abs_z
  
  if (is.finite(cand_max) && (!is.finite(ref_max) || cand_max < ref_max - 1e-6)) {
    return(TRUE)
  }
  if (is.finite(ref_max) && (!is.finite(cand_max) || cand_max > ref_max + 1e-6)) {
    return(FALSE)
  }
  
  # Third criterion: BIC
  cand_bic <- candidate$bic
  ref_bic <- reference$bic
  
  if (is.finite(cand_bic) && (!is.finite(ref_bic) || cand_bic < ref_bic - 1e-6)) {
    return(TRUE)
  }
  
  FALSE
}

pick_stage2_winner <- function(df) {
  if (!nrow(df)) return(NA_character_)
  
  df$max_abs_wald <- as.numeric(df$max_abs_wald)
  df$pseudo_bic <- as.numeric(df$pseudo_bic)
  df$gof_pass <- is.finite(df$max_abs_wald) & df$max_abs_wald <= STAGE2_GOF_THRESHOLD
  
  # Order by: GOF pass (TRUE first), then max_abs_wald (ascending), then pseudo_bic (ascending)
  ord <- order(!df$gof_pass, df$max_abs_wald, df$pseudo_bic, na.last = TRUE)
  df$spec_id[ord][1]
}

format_diagnostics_for_prompt <- function(diag) {
  coef_lines <- character(0)
  if (!is.null(diag$coefficients)) {
    coef_lines <- sprintf(
      "%s: est=%.3f (SE=%.3f)",
      rownames(diag$coefficients),
      diag$coefficients[, "Estimate"],
      diag$coefficients[, "Std. Error"]
    )
  }
  
  gof_lines <- character(0)
  gof_detail <- diag$gof$details
  if (!is.null(gof_detail) && nrow(gof_detail)) {
    ordered <- gof_detail[order(-abs(gof_detail$z)), , drop = FALSE]
    top <- head(ordered, 10)
    gof_lines <- sprintf(
      "%s/%s: z=%.2f (%sfit)",
      top$component, top$stat, top$z,
      ifelse(top$z > 0, "over", ifelse(top$z < 0, "under", "neutral"))
    )
  }
  
  coef_block <- if (length(coef_lines)) {
    paste0("  * ", coef_lines, collapse = "\n")
  } else {
    "  * None"
  }
  
  gof_block <- if (length(gof_lines)) {
    paste0("  * ", gof_lines, collapse = "\n")
  } else {
    "  * No GOF deviations reported"
  }
  
  paste(
    "Current specification diagnostics:",
    sprintf("- BIC: %.2f", diag$bic %||% NA_real_),
    sprintf("- Max |z|: %.3f", diag$gof$max_abs_z %||% Inf),
    sprintf("- GOF pass: %s", ifelse(diag$gof$pass, "YES", "NO")),
    "- GOF detail (positive z = overfit; negative z = underfit):",
    gof_block,
    "- Coefficients:",
    coef_block,
    sep = "\n"
  )
}

call_llm_refinement <- function(dataset, current_terms, diagnostics, admissible_terms) {
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  if (!nzchar(api_key)) {
    return(list(success = FALSE, error = "OPENROUTER_API_KEY not set"))
  }
  
  # Format admissible terms as NUMBERED list for absolute clarity
  admissible_display <- if (length(admissible_terms) > 20) {
    top_20 <- head(admissible_terms, 20)
    paste0(
      paste(sapply(seq_along(top_20), function(i) sprintf("%2d. %s", i, top_20[i])), collapse = "\n"),
      sprintf("\n... and %d more terms", length(admissible_terms) - 20)
    )
  } else {
    paste(sapply(seq_along(admissible_terms), function(i) sprintf("%2d. %s", i, admissible_terms[i])), collapse = "\n")
  }
  
  prompt <- paste(
    "You are an ERGM expert refining a model specification.",
    "",
    "═══ ADMISSIBLE TERMS (choose EXACTLY from this numbered list) ═══",
    admissible_display,
    "",
    "═══ CURRENT MODEL ═══",
    paste(current_terms, collapse = " + "),
    "",
    "═══ YOUR TASK ═══",
    "Suggest ONE edit to improve model fit:",
    "• 'add': Pick a term from the list above NOT in current model",
    "• 'remove': Drop an existing term (NEVER 'edges')",  
    "• 'substitute': Replace one current term with one from the list",
    "",
    "⚠️ COPY THE TERM EXACTLY as shown (including quotes, parentheses, all syntax)",
    "",
    "═══ GOF GUIDE ═══",
    "• Positive z = OVERFIT (model produces too many)",
    "• Negative z = UNDERFIT (model produces too few)",
    "• Target largest |z| values",
    "",
    diagnostics,
    "",
    "═══ RESPONSE FORMAT (raw JSON only, no markdown) ═══",
    '{"action": "add", "term": "exact_term_from_numbered_list", "rationale": "brief_why"}',
    sep = "\n"
  )
  
  body <- list(
    model = "openai/gpt-4o-mini",
    messages = list(
      list(role = "system", content = "You are an ERGM expert."),
      list(role = "user", content = prompt)
    )
  )
  
  resp <- try(POST(
    url = "https://openrouter.ai/api/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    encode = "json",
    body = body
  ), silent = TRUE)
  
  if (inherits(resp, "try-error")) {
    return(list(success = FALSE, error = conditionMessage(attr(resp, "condition"))))
  }
  
  if (status_code(resp) >= 300) {
    return(list(success = FALSE, error = paste("HTTP", status_code(resp))))
  }
  
  content <- content(resp, as = "parsed")
  message_text <- content$choices[[1]]$message$content
  
  # Strip markdown code blocks if present
  message_text <- gsub("^```json\\s*", "", message_text)
  message_text <- gsub("^```\\s*", "", message_text)
  message_text <- gsub("\\s*```$", "", message_text)
  message_text <- trimws(message_text)
  
  result <- try(fromJSON(message_text), silent = TRUE)
  if (inherits(result, "try-error")) {
    return(list(success = FALSE, error = "Failed to parse LLM response", raw = message_text))
  }
  
  list(
    success = TRUE,
    action = result$action,
    term = result$term,
    target = result$target %||% NA_character_,
    rationale = result$rationale %||% ""
  )
}

propose_edit_locally <- function(current_diag, current_terms, admissible) {
  details <- current_diag$gof$details
  
  add_term <- function(term, rationale) {
    if (!is.null(term) && nzchar(term) && term %in% admissible && !(term %in% current_terms)) {
      return(list(
        action = "add",
        term = term,
        target = NA_character_,
        rationale = rationale
      ))
    }
    NULL
  }
  
  # Priority list of common good terms to try
  common_terms <- c(
    "gwesp(decay=0.25, fixed=TRUE)",
    "gwesp(decay=0.5, fixed=TRUE)",
    "gwdsp(decay=0.25, fixed=TRUE)",
    "gwdsp(decay=0.5, fixed=TRUE)",
    "gwdegree(decay=0.25, fixed=TRUE)",
    "gwdegree(decay=0.5, fixed=TRUE)",
    "twopath"
  )
  
  if (is.null(details) || !nrow(details)) {
    # No GOF details - try common terms
    for (term in common_terms) {
      res <- add_term(term, paste("fallback: try", term))
      if (!is.null(res)) return(res)
    }
    
    # Try any admissible term not in model
    for (cand in admissible) {
      if (!(cand %in% current_terms) && cand != "edges") {
        return(list(
          action = "add",
          term = cand,
          target = NA_character_,
          rationale = "fallback: any available admissible term"
        ))
      }
    }
    
    return(NULL)
  }
  
  ordered <- details[order(-abs(details$z)), , drop = FALSE]
  top <- ordered[1, ]
  
  # Heuristic rules based on GOF patterns
  if (grepl("deg", top$component, ignore.case = TRUE) && top$z > 0) {
    # Overfitting on degree - try to dampen
    for (cand in c("gwdegree(0.25, fixed=TRUE)", "gwdegree(0.5, fixed=TRUE)")) {
      res <- add_term(cand, "fallback: damp degree overfit")
      if (!is.null(res)) return(res)
    }
  }
  
  if (grepl("deg", top$component, ignore.case = TRUE) && top$z < 0) {
    # Underfitting on degree - try to boost
    for (cand in c("gwdegree(0.5, fixed=TRUE)", "gwdegree(0.25, fixed=TRUE)")) {
      res <- add_term(cand, "fallback: boost degree")
      if (!is.null(res)) return(res)
    }
  }
  
  if (grepl("espart", top$component, ignore.case = TRUE) && top$z < 0) {
    # Underfitting on closure - try to boost
    for (cand in c("gwesp(0.5, fixed=TRUE)", "gwesp(0.25, fixed=TRUE)", "twopath")) {
      res <- add_term(cand, "fallback: boost closure")
      if (!is.null(res)) return(res)
    }
  }
  
  if (grepl("espart", top$component, ignore.case = TRUE) && top$z > 0) {
    # Overfitting on closure - try to reduce
    for (cand in c("gwdsp(0.5, fixed=TRUE)", "gwdsp(0.25, fixed=TRUE)")) {
      res <- add_term(cand, "fallback: reduce closure overfit")
      if (!is.null(res)) return(res)
    }
  }
  
  # Try all common terms in order
  for (term in common_terms) {
    res <- add_term(term, paste("fallback: systematic try", term))
    if (!is.null(res)) return(res)
  }
  
  # Try any admissible term not in model
  for (cand in admissible) {
    if (!(cand %in% current_terms) && cand != "edges") {
      return(list(
        action = "add",
        term = cand,
        target = NA_character_,
        rationale = "fallback: any available admissible term"
      ))
    }
  }
  
  NULL
}

apply_edit <- function(action, term, target, current_terms) {
  terms <- ensure_edges_term(current_terms)
  term <- trimws(term)
  target <- trimws(target %||% "")
  
  if (identical(action, "add")) {
    if (term %in% terms) {
      return(list(success = FALSE, error = "Term already present"))
    }
    return(list(
      success = TRUE,
      terms = c(terms, term),
      edit = sprintf("Add %s", term)
    ))
  }
  
  if (identical(action, "remove")) {
    if (term == "edges") {
      return(list(success = FALSE, error = "Cannot remove edges"))
    }
    if (!(term %in% terms)) {
      return(list(success = FALSE, error = "Term not in specification"))
    }
    return(list(
      success = TRUE,
      terms = setdiff(terms, term),
      edit = sprintf("Remove %s", term)
    ))
  }
  
  if (identical(action, "substitute")) {
    if (!(term %in% terms)) {
      return(list(success = FALSE, error = "Source term not in spec"))
    }
    if (!nzchar(target)) {
      return(list(success = FALSE, error = "Missing replacement term"))
    }
    terms[which(terms == term)[1]] <- target
    return(list(
      success = TRUE,
      terms = terms,
      edit = sprintf("Substitute %s -> %s", term, target)
    ))
  }
  
  list(success = FALSE, error = "Unknown action")
}

fit_specification <- function(net, terms, seed_offset = 0) {
  terms <- ensure_edges_term(terms)
  formula <- terms_to_formula(terms, lhs = "g")
  environment(formula) <- environment()
  g <- net
  
  last_error <- "unknown error"
  
  for (attempt in seq_along(TRY_LADDER)) {
    ladder <- TRY_LADDER[[attempt]]
    ctrl_args <- ladder$ctrl
    ctrl_args$seed <- 300000 + seed_offset + attempt
    ctrl <- do.call(control.ergm, ctrl_args)
    
    fit_attempt <- try(withTimeout(
      ergm(formula, control = ctrl),
      timeout = ladder$timeout,
      onTimeout = "error"
    ), silent = TRUE)
    
    if (!inherits(fit_attempt, "try-error") && !isTRUE(fit_attempt$failure)) {
      diag <- collect_diagnostics(fit_attempt, net)
      return(list(
        success = TRUE,
        fit = fit_attempt,
        diagnostics = diag
      ))
    }
    
    last_error <- extract_error(fit_attempt)
  }
  
  list(
    success = FALSE,
    error = last_error,
    fit = NULL,
    diagnostics = NULL
  )
}

refine_dataset <- function(dataset, spec_id, spec_libs, admissible_terms) {
  net <- get_network(dataset)
  current_terms <- get_spec_terms(spec_id, spec_libs)
  
  if (!length(current_terms)) {
    return(list(success = FALSE, error = "Unable to locate specification terms"))
  }
  
  current_terms <- ensure_edges_term(current_terms)
  
  # Initial fit
  current_fit <- fit_specification(net, current_terms, seed_offset = 1)
  if (!current_fit$success) {
    return(list(success = FALSE, error = paste("Initial fit failed:", current_fit$error)))
  }
  
  current_diag <- current_fit$diagnostics
  
  # Initialize history
  history <- list(list(
    round = 0,
    spec_id = spec_id,
    terms = current_terms,
    diagnostics = serialize_diagnostics(current_diag),
    action = "initial",
    rationale = "Stage II winner"
  ))
  
  rejections <- 0
  best_terms <- current_terms
  best_diag <- current_diag
  
  # Early exit if already good enough
  if (best_diag$gof$strict_pass) {
    return(list(
      success = TRUE,
      terms = best_terms,
      diagnostics = best_diag,
      history = history
    ))
  }
  
  # Refinement loop
  for (round_idx in 1:MAX_ROUNDS) {
    diag_text <- format_diagnostics_for_prompt(current_diag)
    
    # Try LLM first
    proposal <- call_llm_refinement(dataset, current_terms, diag_text, admissible_terms)
    
    if (!proposal$success) {
      history[[length(history) + 1]] <- list(
        round = round_idx,
        terms = current_terms,
        diagnostics = serialize_diagnostics(current_diag),
        action = "llm_failure",
        rationale = proposal$error
      )
      
      # Fallback to local heuristics
      fallback <- propose_edit_locally(current_diag, current_terms, admissible_terms)
      if (is.null(fallback) || is.null(fallback$term) || !nzchar(fallback$term)) {
        rejections <- rejections + 1
        if (rejections >= MAX_CONSECUTIVE_REJECTIONS) break
        next
      }
      
      proposal <- fallback
      proposal$success <- TRUE
    }
    
    # Apply the edit
    edit_result <- apply_edit(proposal$action, proposal$term, proposal$target, current_terms)
    
    if (!edit_result$success) {
      history[[length(history) + 1]] <- list(
        round = round_idx,
        terms = current_terms,
        diagnostics = serialize_diagnostics(current_diag),
        action = proposal$action,
        rationale = paste(proposal$rationale, "|", edit_result$error)
      )
      rejections <- rejections + 1
      if (rejections >= MAX_CONSECUTIVE_REJECTIONS) break
      next
    }
    
    # Validate candidate (with graduated relaxation after repeated failures)
    relax_mode <- (rejections >= RELAX_AFTER_ATTEMPTS)
    if (relax_mode && rejections == RELAX_AFTER_ATTEMPTS) {
      cat(" [RELAXING GUARDRAILS]")
    }
    validation <- validate_candidate_terms(edit_result$terms, admissible_terms, net, relax_mode = relax_mode)
    if (!validation$valid) {
      history[[length(history) + 1]] <- list(
        round = round_idx,
        terms = current_terms,
        diagnostics = serialize_diagnostics(current_diag),
        action = proposal$action,
        rationale = paste(proposal$rationale, "|", validation$error)
      )
      rejections <- rejections + 1
      if (rejections >= MAX_CONSECUTIVE_REJECTIONS) break
      next
    }
    
    candidate_terms <- validation$terms
    
    # Fit candidate
    candidate_fit <- fit_specification(net, candidate_terms, seed_offset = 100 + round_idx)
    if (!candidate_fit$success) {
      history[[length(history) + 1]] <- list(
        round = round_idx,
        terms = current_terms,
        diagnostics = serialize_diagnostics(current_diag),
        action = proposal$action,
        rationale = paste(proposal$rationale, "|", candidate_fit$error)
      )
      rejections <- rejections + 1
      if (rejections >= MAX_CONSECUTIVE_REJECTIONS) break
      next
    }
    
    candidate_diag <- candidate_fit$diagnostics
    
    # Evaluate improvement
    accept <- lexicographic_better(candidate_diag, current_diag)
    
    history[[length(history) + 1]] <- list(
      round = round_idx,
      terms = candidate_terms,
      diagnostics = serialize_diagnostics(candidate_diag),
      action = proposal$action,
      rationale = proposal$rationale,
      accepted = accept
    )
    
    if (accept) {
      current_terms <- candidate_terms
      current_diag <- candidate_diag
      rejections <- 0
      
      # Update best if better
      if (lexicographic_better(candidate_diag, best_diag)) {
        best_terms <- candidate_terms
        best_diag <- candidate_diag
      }
      
      # Early exit if good enough
      if (candidate_diag$gof$strict_pass) break
    } else {
      rejections <- rejections + 1
      if (rejections >= MAX_CONSECUTIVE_REJECTIONS) break
    }
  }
  
  list(
    success = TRUE,
    terms = best_terms,
    diagnostics = best_diag,
    history = history
  )
}

main <- function() {
  # Check required inputs
  if (!file.exists(STAGE1_RDS) || !file.exists(STAGE2_BASELINE_RDS) || !file.exists(STAGE2_MPLE_CSV)) {
    stop("Required inputs missing (Stage I/II results). Stage III preflight (Script 1) is optional and not required.")
  }
  
  # Load data
  stage1_results <- readRDS(STAGE1_RDS)
  stage2_baselines <- readRDS(STAGE2_BASELINE_RDS)
  stage3_results <- if (file.exists(STAGE3_RESULTS_CSV)) {
    read.csv(STAGE3_RESULTS_CSV, stringsAsFactors = FALSE)
  } else NULL
  
  # Load or build spec libraries
  spec_libs <- NULL
  if (USE_PREBUILT_LIB && file.exists(SPEC_LIB_RDS)) {
    spec_libs <- tryCatch(readRDS(SPEC_LIB_RDS), error = function(e) NULL)
    if (!is.null(spec_libs)) {
      message(sprintf("[Stage3] Loaded prebuilt spec libraries from %s", SPEC_LIB_RDS))
    }
  }
  
  if (is.null(spec_libs)) {
    message("[Stage3] Building specification libraries from Stage I/II results")
    spec_libs <- build_spec_libraries(stage1_results, stage2_baselines)
    if (USE_PREBUILT_LIB) {
      try(saveRDS(spec_libs, SPEC_LIB_RDS), silent = TRUE)
    }
  }
  
  # Load Stage II MPLE results
  stage2_mple <- read.csv(STAGE2_MPLE_CSV, stringsAsFactors = FALSE)
  
  # Validate required columns
  required_cols <- c("dataset", "spec_id", "max_abs_wald", "pseudo_bic")
  missing_cols <- setdiff(required_cols, names(stage2_mple))
  if (length(missing_cols)) {
    stop(sprintf("STAGE2_MPLE_CSV missing: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter to small datasets only
  stage2_mple_small <- stage2_mple[stage2_mple$dataset %in% SMALL_DATASETS, ]
  
  # Pick Stage II winners for small datasets
  initial_specs <- lapply(split(stage2_mple_small, stage2_mple_small$dataset), pick_stage2_winner)
  initial_specs <- initial_specs[!is.na(unlist(initial_specs))]
  
  if (!length(initial_specs)) {
    stop("No Stage II winners found for small datasets to initialize Stage III.")
  }
  
  message(sprintf("[Stage3] Processing %d small datasets: %s", 
                  length(initial_specs), paste(names(initial_specs), collapse = ", ")))
  
  # Refinement loop
  refinement_history <- list()
  summary_rows <- list()
  
  for (ds in names(initial_specs)) {
    spec_id <- initial_specs[[ds]]
    message(sprintf("[Stage3] %s | Stage II winner (s*): %s", ds, spec_id))
    
    admissible <- get_admissible_library(ds, stage2_baselines)
    spec_terms <- get_spec_terms(spec_id, spec_libs)
    
    if (!length(spec_terms)) {
      stop(sprintf("[Stage3] %s | s* %s not found in spec libraries. Check Stage I/II ID consistency.", ds, spec_id))
    }
    
    res <- refine_dataset(ds, spec_id, spec_libs, admissible)
    
    if (!res$success) {
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        dataset = ds,
        spec_id = spec_id,
        success = FALSE,
        final_terms = NA_character_,
        final_bic = NA_real_,
        final_max_abs_z = NA_real_,
        gof_pass = NA,
        error = res$error,
        stringsAsFactors = FALSE
      )
      next
    }
    
    refinement_history[[ds]] <- res$history
    final_diag <- res$diagnostics
    final_serial <- serialize_diagnostics(final_diag)
    
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      dataset = ds,
      spec_id = spec_id,
      final_terms = paste(res$terms, collapse = " + "),
      success = TRUE,
      final_bic = final_serial$bic,
      final_max_abs_z = final_serial$max_abs_z,
      gof_pass = final_serial$gof_pass,
      error = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  
  # Save results
  ensure_dir(dirname(REFINEMENT_HISTORY_RDS))
  saveRDS(refinement_history, REFINEMENT_HISTORY_RDS)
  write_json(refinement_history, REFINEMENT_HISTORY_JSON, pretty = TRUE, auto_unbox = TRUE)
  
  if (length(summary_rows)) {
    summary_df <- do.call(rbind, summary_rows)
    write.csv(summary_df, REFINEMENT_SUMMARY_CSV, row.names = FALSE)
  }
  
  cat("Stage III refinement complete for small datasets.\n")
}

main()
