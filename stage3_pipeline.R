#!/usr/bin/env Rscript
# Stage III finalists: select baseline/LLM specs and fit short MCMLE models.

suppressPackageStartupMessages({
  library(ergm)
  library(network)
  library(jsonlite)
  library(R.utils)
})

source("benchmark_datasets.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

STAGE1_RDS <- "results/stage1_specifications_results.rds"
STAGE2_BASELINE_RDS <- "results/stage2_baseline_specifications.rds"
STAGE2_MPLE_CSV <- "results/stage2_step1_mple_results.csv"
STAGE3_FINALISTS_CSV <- "results/stage3_finalists.csv"
STAGE3_RESULTS_CSV <- "results/stage3_mcmle_results.csv"
STAGE3_RESULTS_RDS <- "results/stage3_mcmle_results.rds"
STAGE3_MODELS_DIR <- "results/stage3_models"

MCMLE_TIMEOUT <- 900
MCMLE_CONTROL <- list(
  MCMC.burnin = 1e5,
  MCMC.interval = 1000,
  MCMC.samplesize = 5000,
  MCMLE.maxit = 12,
  MCMLE.steplength = 0.5,
  seed = 202700
)

BASE_SEED <- 202600

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Shared helpers (spec library rebuild, network cache)
# -----------------------------------------------------------------------------

network_cache <- new.env(parent = emptyenv())

get_network <- function(dataset) {
  if (exists(dataset, envir = network_cache, inherits = FALSE)) {
    return(get(dataset, envir = network_cache, inherits = FALSE))
  }
  net <- suppressMessages(load_benchmark_dataset(dataset)$network)
  assign(dataset, net, envir = network_cache)
  net
}

normalize_terms <- function(terms) unique(trimws(terms))

build_spec_libraries <- function(stage1_results, stage2_baselines) {
  libs <- list(M2_randomK = list(), M3_null = list(), M4_oneshot = list(),
               M5_fewshot = list(), M6_unconstrained = list())
  for (dataset in names(stage1_results)) {
    s1_entry <- stage1_results[[dataset]]
    s2_entry <- stage2_baselines[[dataset]]

    libs$M2_randomK[[dataset]] <- list(list(
      source = "baseline_random_k",
      terms = normalize_terms(s2_entry$baselines$M2_random_k$terms),
      id = sprintf("%s__%s__01", dataset, "M2_randomK")
    ))
    libs$M3_null[[dataset]] <- list(list(
      source = "baseline_null",
      terms = normalize_terms(s2_entry$baselines$M3_null$terms),
      id = sprintf("%s__%s__01", dataset, "M3_null")
    ))

    for (model_name in names(s1_entry$models)) {
      model <- s1_entry$models[[model_name]]
      libs$M4_oneshot[[dataset]] <- c(libs$M4_oneshot[[dataset]], list(list(
        source = model_name,
        terms = normalize_terms(model$M4$formula_canonical)
      )))
      libs$M5_fewshot[[dataset]] <- c(libs$M5_fewshot[[dataset]], list(list(
        source = model_name,
        terms = normalize_terms(model$M5$formula_canonical)
      )))
      libs$M6_unconstrained[[dataset]] <- c(libs$M6_unconstrained[[dataset]], list(list(
        source = model_name,
        terms = normalize_terms(model$M6$formula_canonical)
      )))
    }
    for (method in names(libs)) {
      specs <- libs[[method]][[dataset]]
      if (is.null(specs) || !length(specs)) next
      keys <- vapply(specs, function(entry) paste(entry$terms, collapse = "|"), character(1))
      keep <- !duplicated(keys)
      specs <- specs[keep]
      for (idx in seq_along(specs)) {
        specs[[idx]]$id <- sprintf("%s__%s__%02d", dataset, method, idx)
      }
      libs[[method]][[dataset]] <- specs
    }
  }
  libs
}

terms_to_formula <- function(terms, lhs = "g") {
  rhs <- paste(unique(terms), collapse = " + ")
  as.formula(paste(lhs, "~", rhs))
}

append_csv_row <- function(path, row_df) {
  ensure_dir(dirname(path))
  header_needed <- !file.exists(path)
  write.table(row_df, file = path, sep = ",", row.names = FALSE,
              col.names = header_needed, append = !header_needed, qmethod = "double")
}

read_existing_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

extract_error <- function(obj) {
  if (inherits(obj, "try-error")) {
    cond <- attr(obj, "condition")
    if (!is.null(cond)) return(conditionMessage(cond))
    return(as.character(obj)[1])
  }
  NA_character_
}

# -----------------------------------------------------------------------------
# Finalist selection (1 baseline + 2 LLM per network)
# -----------------------------------------------------------------------------

select_finalists <- function(summary_df) {
  baseline_methods <- c("M2_randomK", "M3_null")
  llm_methods <- c("M4_oneshot", "M5_fewshot", "M6_unconstrained")
  datasets <- unique(summary_df$dataset)
  rows <- list()

  for (dataset_name in datasets) {
    subset_df <- subset(summary_df, dataset == dataset_name & success)
    if (!nrow(subset_df)) next

    base_df <- subset(subset_df, method %in% baseline_methods)
    if (nrow(base_df)) {
      base_df$au_order <- ifelse(is.na(base_df$auprc), -Inf, base_df$auprc)
      base_df$bic_order <- ifelse(is.na(base_df$pseudo_bic), Inf, base_df$pseudo_bic)
      base_df <- base_df[order(-base_df$au_order, base_df$bic_order), ]
      base_pick <- base_df[1, ]
      base_pick$role <- "baseline"
      rows[[length(rows) + 1]] <- base_pick
    }

    llm_df <- subset(subset_df, method %in% llm_methods)
    if (nrow(llm_df)) {
      llm_df$au_order <- ifelse(is.na(llm_df$auprc), -Inf, llm_df$auprc)
      llm_df$bic_order <- ifelse(is.na(llm_df$pseudo_bic), Inf, llm_df$pseudo_bic)
      llm_df <- llm_df[order(-llm_df$au_order, llm_df$bic_order), ]
      llm_df <- llm_df[!duplicated(llm_df$spec_id), ]
      picks <- head(llm_df, 2)
      if (nrow(picks) >= 1) {
        picks$role <- paste0("llm", seq_len(nrow(picks)))
        for (idx in seq_len(nrow(picks))) {
          rows[[length(rows) + 1]] <- picks[idx, ]
        }
      }
    }
  }

  if (!length(rows)) return(data.frame())
  finalists <- do.call(rbind, rows)
  finalists <- finalists[, c("dataset", "method", "spec_id", "source", "auprc", "pseudo_bic", "max_abs_wald", "runtime_sec", "role")]
  finalists
}

# -----------------------------------------------------------------------------
# Stage III MCMLE fitting
# -----------------------------------------------------------------------------

run_mple_init <- function(formula, seed) {
  ctrl <- control.ergm(seed = seed)
  fit <- try(withTimeout(ergm(formula, estimate = "MPLE", control = ctrl),
                         timeout = MPLE_TIMEOUT, onTimeout = "error"), silent = TRUE)
  if (inherits(fit, "try-error")) return(NULL)
  coef(fit)
}

fit_mcmle_for_spec <- function(formula, init_coef, seed) {
  ctrl_args <- MCMLE_CONTROL
  ctrl_args$seed <- seed
  if (!is.null(init_coef)) ctrl_args$init <- init_coef
  ctrl <- do.call(control.ergm, ctrl_args)
  t0 <- proc.time()
  fit <- try(withTimeout(ergm(formula, estimate = "MLE", control = ctrl),
                         timeout = MCMLE_TIMEOUT, onTimeout = "error"), silent = TRUE)
  runtime <- unname((proc.time() - t0)["elapsed"])
  if (inherits(fit, "try-error")) {
    return(list(success = FALSE, runtime = runtime, error = extract_error(fit), fit = NULL))
  }
  if (isTRUE(fit$failure)) {
    return(list(success = FALSE, runtime = runtime, error = "ergm reported failure flag", fit = fit))
  }
  list(success = TRUE, runtime = runtime, error = NA_character_, fit = fit)
}

run_stage3_fits <- function(finalists, spec_libs, base_seed) {
  ensure_dir(STAGE3_MODELS_DIR)
  existing <- read_existing_csv(STAGE3_RESULTS_CSV)
  processed <- if (!is.null(existing)) unique(existing$spec_id) else character(0)
  results_rows <- if (!is.null(existing)) existing else data.frame()

  for (i in seq_len(nrow(finalists))) {
    row <- finalists[i, ]
    spec_id <- row$spec_id
    if (spec_id %in% processed) next

    dataset <- row$dataset
    method <- row$method
    specs_list <- spec_libs[[method]][[dataset]]
    idx <- match(spec_id, vapply(specs_list, function(x) x$id, character(1)))
    if (is.na(idx)) {
      cat(sprintf("Skipping %s: spec not found in library.\n", spec_id))
      next
    }
   spec_entry <- specs_list[[idx]]

    net <- get_network(dataset)
    g <- net
    formula <- terms_to_formula(spec_entry$terms, lhs = "g")
    environment(formula) <- environment()

    init_coef <- run_mple_init(formula, seed = base_seed + 200 + i)
    fit_res <- fit_mcmle_for_spec(formula, init_coef, seed = base_seed + 1000 + i)

    aic <- if (fit_res$success) tryCatch(AIC(fit_res$fit), error = function(e) NA_real_) else NA_real_
    bic <- if (fit_res$success) tryCatch(BIC(fit_res$fit), error = function(e) NA_real_) else NA_real_
    loglik <- if (fit_res$success) tryCatch(logLik(fit_res$fit), error = function(e) NA_real_) else NA_real_
    iterations <- if (fit_res$success) length(fit_res$fit$iterations) else NA_integer_

    row_df <- data.frame(
      dataset = dataset,
      method = method,
      spec_id = spec_id,
      source = row$source,
      role = row$role,
      auprc_step1 = row$auprc,
      pseudo_bic_step1 = row$pseudo_bic,
      max_abs_wald_step1 = row$max_abs_wald,
      success = fit_res$success,
      aic = aic,
      bic = bic,
      loglik = as.numeric(loglik),
      iterations = iterations,
      runtime_sec = fit_res$runtime,
      error = if (fit_res$success) NA_character_ else fit_res$error,
      stringsAsFactors = FALSE
    )

    append_csv_row(STAGE3_RESULTS_CSV, row_df)
    processed <- c(processed, spec_id)

    if (fit_res$success && inherits(fit_res$fit, "ergm")) {
      model_filename <- sprintf("%s.rds", tolower(spec_id))
      saveRDS(fit_res$fit, file = file.path(STAGE3_MODELS_DIR, model_filename))
    }

    cat(sprintf("Stage3 MCMLE: %s | success=%s | runtime=%.1fs\n",
                spec_id, fit_res$success, fit_res$runtime))
  }

  final_df <- read_existing_csv(STAGE3_RESULTS_CSV)
  if (!is.null(final_df)) saveRDS(final_df, STAGE3_RESULTS_RDS)
  final_df %||% data.frame()
}

# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

main <- function() {
  if (!file.exists(STAGE1_RDS)) stop("Stage I results missing")
  if (!file.exists(STAGE2_MPLE_CSV)) stop("Stage II MPLE results missing; run stage2_pipeline.R first")

  stage1_results <- readRDS(STAGE1_RDS)
  baselines <- if (file.exists(STAGE2_BASELINE_RDS)) readRDS(STAGE2_BASELINE_RDS) else stop("Stage II baselines missing")
  baselines <- lapply(baselines, function(entry) {
    if (!is.null(entry$baselines$M2_random_k) && is.null(entry$baselines$M2_random_k$terms)) {
      entry$baselines$M2_random_k$terms <- normalize_terms(entry$baselines$M2_random_k$formula)
    }
    if (!is.null(entry$baselines$M3_null) && is.null(entry$baselines$M3_null$terms)) {
      entry$baselines$M3_null$terms <- normalize_terms(entry$baselines$M3_null$formula)
    }
    entry
  })

  spec_libs <- build_spec_libraries(stage1_results, baselines)

  summary_df <- read.csv(STAGE2_MPLE_CSV, stringsAsFactors = FALSE)
  finalists <- select_finalists(summary_df)
  if (!nrow(finalists)) stop("No finalists available from Stage II results")
  write.csv(finalists, STAGE3_FINALISTS_CSV, row.names = FALSE)

  cat(sprintf("Selected %d finalists across %d networks (Stage3_finalists.csv).\n", nrow(finalists), length(unique(finalists$dataset))))

  results_df <- run_stage3_fits(finalists, spec_libs, BASE_SEED)
  if (is.null(results_df) || !nrow(results_df)) {
    cat("\nNo Stage III fits executed (perhaps all already done).\n")
  } else {
    cat("\nStage III results written to stage3_mcmle_results.csv / .rds\n")
  }
}

main()
