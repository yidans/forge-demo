#!/usr/bin/env Rscript
# Stage II (Steps 0–2): holdout creation, MPLE sweep, and optional CD refinement.

suppressPackageStartupMessages({
  library(ergm)
  library(network)
  library(jsonlite)
  library(PRROC)
  library(R.utils)
})

source("benchmark_datasets.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

# -----------------------------------------------------------------------------
# Paths & constants
# -----------------------------------------------------------------------------

STAGE1_RDS <- "results/stage1_specifications_results.rds"
STAGE2_BASELINE_RDS <- "results/stage2_baseline_specifications.rds"
STAGE2_BASELINE_JSON <- "results/stage2_baseline_specifications.json"
HOLDOUT_RDS <- "results/stage2_holdouts.rds"
HOLDOUT_CSV <- "results/stage2_holdouts_summary.csv"
MPLE_RDS <- "results/stage2_step1_mple_results.rds"
MPLE_CSV <- "results/stage2_step1_mple_results.csv"
CD_RDS <- "results/stage2_step2_counts.rds"
CD_CSV <- "results/stage2_step2_counts.csv"

BASE_SEED <- 202600
K_RANDOM <- 4
HOLDOUT_FRAC <- 0.03
HOLDOUT_SPLITS <- 3

MPLE_TIMEOUT <- 180
ERGMPLE_TIMEOUT <- 120
CD_TIMEOUT <- 300
CD_TOP_K <- 1
CD_RUN_IF_WALD_GT <- 2

network_cache <- new.env(parent = emptyenv())

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Baseline construction (M2 random-K, M3 null)
# -----------------------------------------------------------------------------

draw_random_k <- function(terms, k = K_RANDOM, ensure_edges = TRUE, seed = NULL) {
  unique_terms <- unique(trimws(terms))
  if (!length(unique_terms) || k <= 0) {
    return(list(terms = character(0), available = length(unique_terms)))
  }
  if (!is.null(seed)) set.seed(seed)
  if (ensure_edges && "edges" %in% unique_terms) {
    others <- setdiff(unique_terms, "edges")
    remaining <- max(k - 1L, 0L)
    if (remaining > 0 && length(others) > 0) {
      draw_count <- min(remaining, length(others))
      sampled <- if (length(others) <= draw_count) others else sample(others, draw_count)
      return(list(terms = c("edges", sampled), available = length(unique_terms)))
    }
    return(list(terms = "edges", available = length(unique_terms)))
  }
  draw_count <- min(k, length(unique_terms))
  sampled <- if (length(unique_terms) <= draw_count) unique_terms else sample(unique_terms, draw_count)
  list(terms = sampled, available = length(unique_terms))
}

build_stage2_baselines <- function(stage1_results) {
  baselines <- list()
  dataset_names <- names(stage1_results)
  for (i in seq_along(dataset_names)) {
    key <- dataset_names[i]
    entry <- stage1_results[[i]]
    admissible <- entry$admissible_library
    if (is.null(admissible) || !length(admissible)) {
      warning(sprintf("Dataset %s has no admissible terms; skipping baselines", key))
      next
    }
    seed <- BASE_SEED + i - 1L
    random_spec <- draw_random_k(admissible, seed = seed)
    baselines[[key]] <- list(
      network = entry$network,
      admissible_library = admissible,
      baselines = list(
        M2_random_k = list(
          terms = random_spec$terms,
          description = sprintf("Random sample of up to %d admissible terms with edges forced in when available.", K_RANDOM),
          seed = seed
        ),
        M3_null = list(
          terms = "edges",
          description = "Edges-only null ERGM specification."
        )
      ),
      diagnostics = entry$diagnostics %||% NULL,
      system_brief = entry$system_brief %||% NULL
    )
  }
  if (!length(baselines)) stop("Stage 2 baselines could not be generated")
  ensure_dir(dirname(STAGE2_BASELINE_RDS))
  saveRDS(baselines, STAGE2_BASELINE_RDS)
  write_json(baselines, STAGE2_BASELINE_JSON, pretty = TRUE, auto_unbox = TRUE)
  baselines
}

coerce_baseline_terms <- function(baselines) {
  lapply(baselines, function(entry) {
    if (!is.null(entry$baselines$M2_random_k) && is.null(entry$baselines$M2_random_k$terms)) {
      entry$baselines$M2_random_k$terms <- unique(trimws(entry$baselines$M2_random_k$formula))
    }
    if (!is.null(entry$baselines$M3_null) && is.null(entry$baselines$M3_null$terms)) {
      entry$baselines$M3_null$terms <- unique(trimws(entry$baselines$M3_null$formula))
    }
    entry
  })
}

# -----------------------------------------------------------------------------
# Holdout generation (Step 0)
# -----------------------------------------------------------------------------

get_network <- function(dataset) {
  if (exists(dataset, envir = network_cache, inherits = FALSE)) {
    return(get(dataset, envir = network_cache, inherits = FALSE))
  }
  net <- suppressMessages(load_benchmark_dataset(dataset)$network)
  assign(dataset, net, envir = network_cache)
  net
}

make_holdouts <- function(g, frac, seeds) {
  directed <- is.directed(g)
  el <- as.matrix(as.edgelist(g))
  n_edges <- nrow(el)
  n_vertices <- network.size(g)
  lapply(seeds, function(seed) {
    set.seed(seed)
    pos_count <- if (n_edges > 0) max(1L, floor(frac * n_edges)) else 0L
    pos_idx <- if (pos_count > 0L) sample(seq_len(n_edges), pos_count) else integer(0)
    pos <- if (length(pos_idx) > 0) el[pos_idx, , drop = FALSE] else matrix(0, 0, 2)
    neg <- matrix(0, pos_count, 2)
    if (pos_count > 0) {
      for (k in seq_len(pos_count)) {
        repeat {
          i <- sample.int(n_vertices, 1)
          j <- sample.int(n_vertices, 1)
          if (!directed && i == j) next
          if (!directed && i > j) {
            tmp <- i; i <- j; j <- tmp
          }
          if (!is.adjacent(g, i, j)) {
            neg[k, ] <- c(i, j)
            break
          }
        }
      }
    }
    list(pos = pos, neg = neg, seed = seed)
  })
}

prepare_holdouts <- function(stage1_results) {
  if (file.exists(HOLDOUT_RDS)) {
    return(readRDS(HOLDOUT_RDS))
  }
  holdouts <- list()
  summary_rows <- list()
  dataset_names <- names(stage1_results)
  for (i in seq_along(dataset_names)) {
    dataset <- dataset_names[i]
    net <- get_network(dataset)
    seeds <- BASE_SEED + (i - 1L) * 10L + seq_len(HOLDOUT_SPLITS)
    splits <- make_holdouts(net, HOLDOUT_FRAC, seeds)
    holdouts[[dataset]] <- splits
    for (j in seq_along(splits)) {
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        dataset = dataset,
        split = j,
        seed = splits[[j]]$seed,
        pos_edges = nrow(splits[[j]]$pos),
        stringsAsFactors = FALSE
      )
    }
  }
  ensure_dir(dirname(HOLDOUT_RDS))
  saveRDS(holdouts, HOLDOUT_RDS)
  summary_df <- do.call(rbind, summary_rows)
  write.csv(summary_df, HOLDOUT_CSV, row.names = FALSE)
  holdouts
}

# -----------------------------------------------------------------------------
# Specification libraries
# -----------------------------------------------------------------------------

normalize_terms <- function(terms) unique(trimws(terms))

build_spec_libraries <- function(stage1_results, stage2_baselines) {
  libs <- list(M2_randomK = list(), M3_null = list(), M4_oneshot = list(),
               M5_fewshot = list(), M6_unconstrained = list())
  for (dataset in names(stage1_results)) {
    s1_entry <- stage1_results[[dataset]]
    s2_entry <- stage2_baselines[[dataset]]
    libs$M2_randomK[[dataset]] <- list(list(
      source = "baseline_random_k",
      terms = normalize_terms(s2_entry$baselines$M2_random_k$terms)
    ))
    libs$M3_null[[dataset]] <- list(list(
      source = "baseline_null",
      terms = normalize_terms(s2_entry$baselines$M3_null$terms)
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
      libs[[method]][[dataset]] <- specs[keep]
      libs[[method]][[dataset]] <- lapply(seq_along(libs[[method]][[dataset]]), function(idx) {
        entry <- libs[[method]][[dataset]][[idx]]
        entry$id <- sprintf("%s__%s__%02d", dataset, method, idx)
        entry
      })
    }
  }
  libs
}

flatten_spec_catalog <- function(spec_libs) {
  rows <- list()
  for (method in names(spec_libs)) {
    for (dataset in names(spec_libs[[method]])) {
      specs <- spec_libs[[method]][[dataset]]
      for (entry in specs) {
        rows[[length(rows) + 1L]] <- data.frame(
          dataset = dataset,
          method = method,
          spec_id = entry$id,
          source = entry$source,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

# -----------------------------------------------------------------------------
# Helper utilities
# -----------------------------------------------------------------------------

terms_to_formula <- function(terms, lhs = "g") {
  rhs <- paste(unique(terms), collapse = " + ")
  as.formula(paste(lhs, "~", rhs))
}

extract_error <- function(obj) {
  if (inherits(obj, "try-error")) {
    cond <- attr(obj, "condition")
    if (!is.null(cond)) return(conditionMessage(cond))
    return(as.character(obj)[1])
  }
  NA_character_
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

# -----------------------------------------------------------------------------
# Step 1: MPLE sweep
# -----------------------------------------------------------------------------

fit_mple_for_spec <- function(spec_entry, net, seed_base) {
  g <- net
  formula <- terms_to_formula(spec_entry$terms, lhs = "g")
  environment(formula) <- environment()
  ctrl <- control.ergm(seed = seed_base)
  t0 <- proc.time()
  fit <- try(withTimeout(ergm(formula, estimate = "MPLE", control = ctrl),
                         timeout = MPLE_TIMEOUT, onTimeout = "error"), silent = TRUE)
  runtime <- unname((proc.time() - t0)["elapsed"])
  if (inherits(fit, "try-error")) {
    return(list(success = FALSE, error = extract_error(fit), runtime = runtime))
  }

  pseudo_bic <- tryCatch(BIC(fit), error = function(e) NA_real_)

  summary_fit <- try(summary(fit)$coefficients, silent = TRUE)
  wald_max <- if (inherits(summary_fit, "try-error")) {
    NA_real_
  } else {
    est <- summary_fit[, "Estimate"]
    se <- summary_fit[, "Std. Error"]
    vals <- est / se
    if (length(vals)) max(abs(vals), na.rm = TRUE) else NA_real_
  }

  mp <- try(withTimeout(ergmMPLE(formula), timeout = ERGMPLE_TIMEOUT, onTimeout = "error"), silent = TRUE)
  auprc <- NA_real_
  if (!inherits(mp, "try-error")) {
    beta <- coef(fit)
    X <- mp$predictor
    y <- mp$response
    w <- mp$weights
    eta <- as.vector(X %*% beta)
    p <- plogis(eta)
    pos_idx <- y == 1
    neg_idx <- y == 0
    if (any(pos_idx) && any(neg_idx)) {
      w_pos <- as.integer(round(w[pos_idx]))
      w_neg <- as.integer(round(w[neg_idx]))
      scores_pos <- rep(p[pos_idx], w_pos)
      scores_neg <- rep(p[neg_idx], w_neg)
      auprc <- tryCatch(PRROC::pr.curve(scores.class0 = scores_pos,
                                        scores.class1 = scores_neg)$auc.integral,
                        error = function(e) NA_real_)
      if (is.nan(auprc)) auprc <- NA_real_
    }
  }

  list(success = TRUE,
       pseudo_bic = pseudo_bic,
       auprc = auprc,
       wald_max = wald_max,
       runtime = runtime)
}

run_step1_mple <- function(spec_libs) {
  catalog <- flatten_spec_catalog(spec_libs)
  if (!nrow(catalog)) stop("No specifications found.")

  existing <- read_existing_csv(MPLE_CSV)
  processed <- if (!is.null(existing)) unique(existing$spec_id) else character(0)

  for (i in seq_len(nrow(catalog))) {
    row <- catalog[i, ]
    spec_id <- row$spec_id
    if (spec_id %in% processed) next

    dataset <- row$dataset
    method <- row$method
    source <- row$source
    specs_list <- spec_libs[[method]][[dataset]]
    idx <- match(spec_id, vapply(specs_list, function(x) x$id, character(1)))
    if (is.na(idx)) next
    spec_entry <- specs_list[[idx]]

    net <- get_network(dataset)
    seed_base <- BASE_SEED + i
    res <- fit_mple_for_spec(spec_entry, net, seed_base)

    row_df <- data.frame(
      dataset = dataset,
      method = method,
      spec_id = spec_id,
      source = source,
      success = res$success,
      pseudo_bic = if (res$success) res$pseudo_bic else NA_real_,
      auprc = if (res$success) res$auprc else NA_real_,
      max_abs_wald = if (res$success) res$wald_max else NA_real_,
      runtime_sec = res$runtime,
      error = if (res$success) NA_character_ else res$error,
      stringsAsFactors = FALSE
    )

    append_csv_row(MPLE_CSV, row_df)
    processed <- c(processed, spec_id)

    au_text <- ifelse(is.na(row_df$auprc), "NA", sprintf("%.4f", row_df$auprc))
    wald_text <- ifelse(is.na(row_df$max_abs_wald), "NA", sprintf("%.2f", row_df$max_abs_wald))
    cat(sprintf("Step1 MPLE: %s | %s | success=%s | auprc=%s | max|z|=%s\n",
                dataset, spec_id, res$success, au_text, wald_text))
  }

  summary_df <- read_existing_csv(MPLE_CSV)
  if (!is.null(summary_df)) saveRDS(summary_df, MPLE_RDS)
  summary_df %||% data.frame()
}

# -----------------------------------------------------------------------------
# Step 2: CD refinement on top-ranked specs (optional)
# -----------------------------------------------------------------------------

rank_specs <- function(summary_df) {
  subset_df <- subset(summary_df, method %in% c("M4_oneshot", "M5_fewshot", "M6_unconstrained") & success)
  if (!nrow(subset_df)) return(list())
  subset_df$au_order <- ifelse(is.na(subset_df$auprc), -Inf, subset_df$auprc)
  subset_df$bic_order <- ifelse(is.na(subset_df$pseudo_bic), Inf, subset_df$pseudo_bic)
  split(subset_df, list(subset_df$dataset, subset_df$method), drop = TRUE)
}

perform_cd_fit <- function(spec_entry, net, seed) {
  g <- net
  formula <- terms_to_formula(spec_entry$terms, lhs = "g")
  environment(formula) <- environment()
  ctrl <- control.ergm(seed = seed, CD.nsteps = 20)
  t0 <- proc.time()
  fit <- try(withTimeout(ergm(formula, estimate = "CD", control = ctrl),
                         timeout = CD_TIMEOUT, onTimeout = "error"), silent = TRUE)
  runtime <- unname((proc.time() - t0)["elapsed"])
  if (inherits(fit, "try-error")) {
    return(list(success = FALSE, runtime = runtime, error = extract_error(fit)))
  }
  list(success = TRUE, runtime = runtime, error = NA_character_)
}

run_step2_cd <- function(spec_libs, summary_df) {
  if (is.null(summary_df) || !nrow(summary_df)) {
    return(read_existing_csv(CD_CSV) %||% data.frame())
  }

  ranks <- rank_specs(summary_df)
  existing_counts <- read_existing_csv(CD_CSV)
  processed_specs <- if (!is.null(existing_counts)) unique(existing_counts$spec_id) else character(0)
  counter <- length(processed_specs)

  for (key in names(ranks)) {
    df <- ranks[[key]]
    if (!nrow(df)) next
    df$au_order <- ifelse(is.na(df$auprc), -Inf, df$auprc)
    df$bic_order <- ifelse(is.na(df$pseudo_bic), Inf, df$pseudo_bic)
    df <- df[order(-df$au_order, df$bic_order), ]
    finalists <- head(df, CD_TOP_K)
    if (!nrow(finalists)) next

    for (row_idx in seq_len(nrow(finalists))) {
      row <- finalists[row_idx, ]
      spec_id <- row$spec_id
      if (spec_id %in% processed_specs) next

      dataset <- row$dataset
      method <- row$method
      specs_list <- spec_libs[[method]][[dataset]]
      idx <- match(spec_id, vapply(specs_list, function(x) x$id, character(1)))
      if (is.na(idx)) next
      spec_entry <- specs_list[[idx]]

      wald <- row$max_abs_wald
      need_cd <- is.na(wald) || wald > CD_RUN_IF_WALD_GT
      cd_success <- NA
      cd_runtime <- NA_real_
      cd_error <- NA_character_

      if (need_cd) {
        counter <- counter + 1L
        seed <- BASE_SEED + 10000 + counter
        net <- get_network(dataset)
        cd_fit <- perform_cd_fit(spec_entry, net, seed)
        cd_success <- cd_fit$success
        cd_runtime <- cd_fit$runtime
        cd_error <- cd_fit$error
      }

      row_df <- data.frame(
        dataset = dataset,
        method = method,
        spec_id = spec_id,
        source = row$source,
        auprc = row$auprc,
        pseudo_bic = row$pseudo_bic,
        max_abs_wald = row$max_abs_wald,
        cd_run = need_cd,
        cd_success = cd_success,
        cd_runtime_sec = cd_runtime,
        error = cd_error,
        stringsAsFactors = FALSE
      )

      append_csv_row(CD_CSV, row_df)
      processed_specs <- c(processed_specs, spec_id)

      cat(sprintf("Step2 CD: %s | %s | cd_run=%s | cd_success=%s\n",
                  dataset, spec_id, need_cd, ifelse(is.na(cd_success), "NA", cd_success)))
    }
  }

  counts_df <- read_existing_csv(CD_CSV)
  if (!is.null(counts_df)) saveRDS(counts_df, CD_RDS)
  counts_df %||% data.frame()
}

# -----------------------------------------------------------------------------
# Main driver
# -----------------------------------------------------------------------------

main <- function() {
  if (!file.exists(STAGE1_RDS)) stop("Stage I results missing; run earlier stages first.")
  stage1_results <- readRDS(STAGE1_RDS)
  baselines <- if (file.exists(STAGE2_BASELINE_RDS)) {
    readRDS(STAGE2_BASELINE_RDS)
  } else {
    build_stage2_baselines(stage1_results)
  }
  baselines <- coerce_baseline_terms(baselines)
  prepare_holdouts(stage1_results)  # Step 0 snapshot (once)
  spec_libs <- build_spec_libraries(stage1_results, baselines)

  summary_df <- run_step1_mple(spec_libs)
  cat(sprintf("\nStep 1 complete: %d specifications recorded.\n", nrow(summary_df)))

  cd_summary <- run_step2_cd(spec_libs, summary_df)
  if (is.data.frame(cd_summary) && nrow(cd_summary)) {
    cat("\nStep 2 CD summary (per specification):\n")
    print(cd_summary[, c("dataset", "method", "spec_id", "cd_run", "cd_success")])
    cat("\nCD run counts by dataset × method:\n")
    counts <- aggregate(cd_run ~ dataset + method, data = cd_summary, FUN = function(x) sum(x, na.rm = TRUE))
    print(counts)
  } else {
    cat("\nNo CD fits executed (either all finalists skipped or already processed).\n")
  }

  cat("\nPipeline complete. Stage 3 (MCMLE finalists) can proceed after review.\n")
}

main()
