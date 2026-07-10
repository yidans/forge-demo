#!/usr/bin/env Rscript
# Stage IV interpretation pipeline.
# Consumes fitted/refined ERGM specifications and asks an LLM to explain the
# mechanisms, evidence, and limits of interpretation.

suppressPackageStartupMessages({
  library(jsonlite)
  library(httr)
  library(utils)
})

if (file.exists(".env")) {
  try(readRenviron(".env"), silent = TRUE)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

STAGE1_RDS <- "results/stage1_specifications_results.rds"
STAGE2_BASELINE_RDS <- "results/stage2_baseline_specifications.rds"
STAGE2_SPEC_LIB_RDS <- "results/stage2_spec_libraries.rds"
STAGE3_REFINEMENT_SUMMARY_CSV <- "results/stage3_refinement_summary.csv"
STAGE3_REFINEMENT_HISTORY_RDS <- "results/stage3_refinement_history.rds"
STAGE3_MCMLE_CSV <- "results/stage3_mcmle_results.csv"
STAGE3_MODELS_DIR <- "results/stage3_models"

STAGE4_OUTPUT_RDS <- "results/stage4_interpretations.rds"
STAGE4_OUTPUT_JSON <- "results/stage4_interpretations.json"
STAGE4_OUTPUT_CSV <- "results/stage4_interpretations_summary.csv"
STAGE4_OUTPUT_MD <- "results/stage4_interpretation_report.md"
STAGE4_PROMPT_DIR <- "prompts/stage4"

DEFAULT_MODEL <- "openai/gpt-4o-mini"
INTERPRETATION_MODEL <- Sys.getenv("STAGE4_MODEL", unset = DEFAULT_MODEL)
SKIP_LLM <- tolower(Sys.getenv("STAGE4_SKIP_LLM", unset = "false")) %in% c("1", "true", "yes")
TIMEOUT_SECONDS <- as.numeric(Sys.getenv("STAGE4_TIMEOUT_SECONDS", unset = "120"))
OUTPUT_LANGUAGE <- Sys.getenv("STAGE4_OUTPUT_LANGUAGE", unset = "English")

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

sanitize_terms <- function(terms) {
  terms <- trimws(terms)
  terms <- gsub("`", "", terms, fixed = TRUE)
  terms <- terms[!is.na(terms) & nzchar(terms)]
  unique(terms)
}

split_terms <- function(term_string) {
  if (is.null(term_string) || is.na(term_string) || !nzchar(term_string)) {
    return(character(0))
  }
  sanitize_terms(strsplit(term_string, "\\s+\\+\\s+")[[1]])
}

term_signature <- function(terms) {
  paste(sort(gsub("\\s+", "", sanitize_terms(terms))), collapse = "|")
}

format_bool <- function(x) {
  if (isTRUE(x)) "true" else if (identical(x, FALSE)) "false" else "unknown"
}

term_family <- function(term) {
  sub("\\(.*$", "", gsub("\\s+", "", term))
}

term_mechanism <- function(term) {
  family <- term_family(term)
  attr_match <- regmatches(term, regexec('^([A-Za-z0-9_]+)\\("([^"]+)"', term))[[1]]
  attr <- if (length(attr_match) >= 3) attr_match[3] else NA_character_

  switch(
    family,
    edges = "baseline tie propensity after accounting for all other terms",
    mutual = "reciprocity: actors tend to return directed ties",
    gwesp = "triadic closure and local clustering among connected pairs",
    gwdsp = "shared-partner structure among dyads, including two-path pressure not limited to closed triangles",
    gwdegree = "degree heterogeneity and hub tendency in undirected networks",
    gwidegree = "in-degree heterogeneity and popularity in directed networks",
    gwodegree = "out-degree heterogeneity and activity in directed networks",
    ttriple = "transitive triads in directed networks",
    ctriple = "cyclic triads in directed networks",
    twopath = "open two-path prevalence and brokerage opportunities",
    nodematch = sprintf("homophily on %s: ties are more likely within the same category", attr),
    nodemix = sprintf("mixing pattern across levels of %s", attr),
    nodefactor = sprintf("composition or activity differences by %s category", attr),
    nodeifactor = sprintf("receiver-side category effect for %s", attr),
    nodeofactor = sprintf("sender-side category effect for %s", attr),
    nodecov = sprintf("node-level covariate effect of %s on tie propensity", attr),
    nodeicov = sprintf("receiver-side covariate effect of %s", attr),
    nodeocov = sprintf("sender-side covariate effect of %s", attr),
    absdiff = sprintf("similarity by numeric distance on %s", attr),
    sprintf("mechanism represented by ERGM term family %s", family)
  )
}

read_required_rds <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Missing required input: %s", path))
  }
  suppressWarnings(readRDS(path))
}

coerce_baseline_terms <- function(baselines) {
  lapply(baselines, function(entry) {
    if (!is.null(entry$baselines$M2_random_k) && is.null(entry$baselines$M2_random_k$terms)) {
      entry$baselines$M2_random_k$terms <- sanitize_terms(entry$baselines$M2_random_k$formula)
    }
    if (!is.null(entry$baselines$M3_null) && is.null(entry$baselines$M3_null$terms)) {
      entry$baselines$M3_null$terms <- sanitize_terms(entry$baselines$M3_null$formula)
    }
    entry
  })
}

get_history_entry_for_terms <- function(history, terms) {
  if (is.null(history) || !length(history)) return(NULL)
  sig <- term_signature(terms)
  matches <- Filter(function(entry) term_signature(entry$terms %||% character(0)) == sig, history)
  if (length(matches)) {
    return(matches[[length(matches)]])
  }
  history[[length(history)]]
}

summarize_history <- function(history) {
  if (is.null(history) || !length(history)) {
    return(data.frame())
  }
  rows <- lapply(history, function(entry) {
    diag <- entry$diagnostics %||% list()
    data.frame(
      round = entry$round %||% NA_integer_,
      action = entry$action %||% NA_character_,
      accepted = entry$accepted %||% NA,
      bic = diag$bic %||% NA_real_,
      max_abs_z = diag$max_abs_z %||% NA_real_,
      rationale = entry$rationale %||% NA_character_,
      terms = paste(entry$terms %||% character(0), collapse = " + "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

extract_coefficients <- function(history_entry) {
  if (is.null(history_entry)) return(data.frame())
  diag <- history_entry$diagnostics %||% list()
  coeffs <- diag$coefficients %||% data.frame()
  if (!is.data.frame(coeffs) || !nrow(coeffs)) {
    return(data.frame())
  }
  coeffs
}

build_refinement_cases <- function(stage1_results, stage2_baselines) {
  if (!file.exists(STAGE3_REFINEMENT_SUMMARY_CSV)) {
    return(NULL)
  }
  summary_df <- read.csv(STAGE3_REFINEMENT_SUMMARY_CSV, stringsAsFactors = FALSE)
  history <- if (file.exists(STAGE3_REFINEMENT_HISTORY_RDS)) {
    suppressWarnings(readRDS(STAGE3_REFINEMENT_HISTORY_RDS))
  } else {
    list()
  }

  rows <- lapply(seq_len(nrow(summary_df)), function(i) {
    row <- summary_df[i, ]
    dataset <- row$dataset
    terms <- split_terms(row$final_terms)
    hist <- history[[dataset]] %||% list()
    final_hist <- get_history_entry_for_terms(hist, terms)
    coeffs <- extract_coefficients(final_hist)

    s1 <- stage1_results[[dataset]] %||% list()
    s2 <- stage2_baselines[[dataset]] %||% list()
    network <- s1$network %||% s2$network %||% list()

    list(
      dataset = dataset,
      input_source = "stage3_refinement",
      spec_id = row$spec_id,
      source = NA_character_,
      method = NA_character_,
      terms = terms,
      final_bic = as.numeric(row$final_bic),
      final_aic = NA_real_,
      final_max_abs_z = as.numeric(row$final_max_abs_z),
      gof_pass = row$gof_pass,
      coefficients = coeffs,
      history = summarize_history(hist),
      network = network,
      diagnostics = s1$diagnostics %||% s2$diagnostics %||% list(),
      system_brief = s1$system_brief %||% s2$system_brief %||% list(),
      admissible_library = s1$admissible_library %||% s2$admissible_library %||% character(0),
      raw_row = row
    )
  })
  rows
}

get_spec_terms <- function(spec_id, spec_libs) {
  parts <- strsplit(spec_id, "__")[[1]]
  if (length(parts) < 3) return(character(0))
  dataset <- parts[1]
  method <- if (length(parts) == 3) parts[2] else paste(parts[2:(length(parts) - 1)], collapse = "__")
  specs <- spec_libs[[method]][[dataset]]
  if (is.null(specs)) return(character(0))
  idx <- match(spec_id, vapply(specs, function(x) x$id, character(1)))
  if (is.na(idx)) return(character(0))
  sanitize_terms(specs[[idx]]$terms)
}

extract_model_coefficients <- function(spec_id) {
  path <- file.path(STAGE3_MODELS_DIR, sprintf("%s.rds", tolower(spec_id)))
  if (!file.exists(path)) return(data.frame())
  fit <- try(suppressWarnings(readRDS(path)), silent = TRUE)
  if (inherits(fit, "try-error")) return(data.frame())
  coef_table <- try(summary(fit)$coefficients, silent = TRUE)
  if (inherits(coef_table, "try-error") || is.null(coef_table)) return(data.frame())
  data.frame(
    term = rownames(coef_table),
    estimate = coef_table[, "Estimate"],
    std_error = coef_table[, "Std. Error"],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

build_mcmle_cases <- function(stage1_results, stage2_baselines) {
  if (!file.exists(STAGE3_MCMLE_CSV) || !file.exists(STAGE2_SPEC_LIB_RDS)) {
    return(list())
  }
  results <- read.csv(STAGE3_MCMLE_CSV, stringsAsFactors = FALSE)
  results <- subset(results, success)
  if (!nrow(results)) return(list())
  spec_libs <- suppressWarnings(readRDS(STAGE2_SPEC_LIB_RDS))

  best_rows <- do.call(rbind, lapply(split(results, results$dataset), function(df) {
    df[which.min(df$bic), , drop = FALSE]
  }))

  lapply(seq_len(nrow(best_rows)), function(i) {
    row <- best_rows[i, ]
    dataset <- row$dataset
    s1 <- stage1_results[[dataset]] %||% list()
    s2 <- stage2_baselines[[dataset]] %||% list()
    network <- s1$network %||% s2$network %||% list()

    list(
      dataset = dataset,
      input_source = "stage3_mcmle_best_bic",
      spec_id = row$spec_id,
      source = row$source,
      method = row$method,
      terms = get_spec_terms(row$spec_id, spec_libs),
      final_bic = as.numeric(row$bic),
      final_aic = as.numeric(row$aic),
      final_max_abs_z = NA_real_,
      gof_pass = NA,
      coefficients = extract_model_coefficients(row$spec_id),
      history = data.frame(),
      network = network,
      diagnostics = s1$diagnostics %||% s2$diagnostics %||% list(),
      system_brief = s1$system_brief %||% s2$system_brief %||% list(),
      admissible_library = s1$admissible_library %||% s2$admissible_library %||% character(0),
      raw_row = row
    )
  })
}

format_coefficients_for_prompt <- function(coefficients) {
  if (!is.data.frame(coefficients) || !nrow(coefficients)) {
    return("No coefficient table available.")
  }
  lines <- apply(coefficients, 1, function(row) {
    est <- suppressWarnings(as.numeric(row[["estimate"]]))
    se <- suppressWarnings(as.numeric(row[["std_error"]]))
    sprintf("- %s: estimate=%.4f, SE=%.4f", row[["term"]], est, se)
  })
  paste(lines, collapse = "\n")
}

format_history_for_prompt <- function(history) {
  if (!is.data.frame(history) || !nrow(history)) {
    return("No refinement history available.")
  }
  accepted <- subset(history, isTRUE(accepted) | identical(accepted, TRUE) | accepted == TRUE)
  use_rows <- if (nrow(accepted)) accepted else tail(history, min(5, nrow(history)))
  lines <- apply(use_rows, 1, function(row) {
    sprintf(
      "- round=%s action=%s accepted=%s BIC=%s max_abs_z=%s rationale=%s",
      row[["round"]], row[["action"]], row[["accepted"]],
      row[["bic"]], row[["max_abs_z"]], row[["rationale"]]
    )
  })
  paste(lines, collapse = "\n")
}

format_mechanism_glossary <- function(terms) {
  lines <- vapply(terms, function(term) {
    sprintf("- %s: %s", term, term_mechanism(term))
  }, character(1))
  paste(lines, collapse = "\n")
}

build_interpretation_prompt <- function(case) {
  brief <- case$system_brief %||% list()
  diagnostics <- case$diagnostics %||% list()
  network <- case$network %||% list()

  system_prompt <- paste(
    "You are Stage 4 of FORGE: an interpretation LLM for fitted ERGMs.",
    "Your job is to explain social/network mechanisms, not to select terms or improve the model.",
    "Use cautious language: ERGM coefficients support conditional association mechanisms, not causal proof.",
    "If GOF does not pass or diagnostics are weak, say so clearly.",
    "Return valid JSON only.",
    sep = "\n"
  )

  user_prompt <- sprintf(
"Dataset: %s
Input source: %s
Specification ID: %s
Network: name=%s, nodes=%s, directed=%s
Actors: %s
Tie meaning: %s
Context constraint: %s

Final ERGM terms:
%s

Term mechanism glossary:
%s

Coefficient table:
%s

Fit and diagnostic evidence:
- BIC: %s
- AIC: %s
- GOF max_abs_z: %s
- GOF pass: %s
- Initial diagnostics: density=%s, isolates=%s, reciprocity=%s, clustering=%s, triangles=%s

Refinement evidence:
%s

Task:
Explain the mechanisms represented by this final ERGM. Tie each mechanism to specific terms and, where available, coefficient signs/magnitudes and refinement evidence. Then synthesize those mechanisms into one human-understandable theory of how ties form in this network. Separate supported interpretation from limitations. Do not overclaim causality.
Output language: %s.

Output JSON schema:
{
  \"headline\": \"one-sentence mechanism summary\",
  \"human_understandable_theory\": \"plain theory of the network in 1-2 short paragraphs, using everyday language and no ERGM jargon unless briefly defined\",
  \"mechanism_explanation\": \"clear paragraph explaining the main network mechanisms\",
  \"term_interpretations\": [
    {\"term\": \"term name\", \"mechanism\": \"what it means\", \"evidence\": \"coefficient/diagnostic evidence\", \"caution\": \"interpretive limit\"}
  ],
  \"evidence_assessment\": \"how strong the fitted evidence is, including GOF/BIC caveats\",
  \"limitations\": [\"specific limitation 1\", \"specific limitation 2\"],
  \"plain_language\": \"nontechnical explanation for a domain audience\",
  \"recommended_followups\": [\"diagnostic or modeling follow-up 1\", \"follow-up 2\"]
}
",
    case$dataset,
    case$input_source,
    case$spec_id,
    network$name %||% NA_character_,
    network$nodes %||% NA,
    format_bool(network$directed %||% NA),
    brief$actors %||% "Not provided",
    brief$tie_meaning %||% "Not provided",
    brief$constraint %||% "Not provided",
    paste(sprintf("- %s", case$terms), collapse = "\n"),
    format_mechanism_glossary(case$terms),
    format_coefficients_for_prompt(case$coefficients),
    ifelse(is.na(case$final_bic), "NA", sprintf("%.4f", case$final_bic)),
    ifelse(is.na(case$final_aic), "NA", sprintf("%.4f", case$final_aic)),
    ifelse(is.na(case$final_max_abs_z), "NA", sprintf("%.4f", case$final_max_abs_z)),
    format_bool(case$gof_pass),
    diagnostics$density %||% "NA",
    diagnostics$isolates %||% "NA",
    diagnostics$reciprocity %||% "NA",
    diagnostics$clustering %||% "NA",
    diagnostics$triangles %||% "NA",
    format_history_for_prompt(case$history),
    OUTPUT_LANGUAGE
  )

  list(system = system_prompt, user = user_prompt)
}

strip_json_fences <- function(text) {
  text <- gsub("^```json\\s*", "", text, ignore.case = TRUE)
  text <- gsub("^```\\s*", "", text)
  text <- gsub("\\s*```$", "", text)
  trimws(text)
}

call_interpretation_llm <- function(prompt, model = INTERPRETATION_MODEL) {
  api_key <- Sys.getenv("OPENROUTER_API_KEY")
  if (!nzchar(api_key)) {
    return(list(success = FALSE, error = "OPENROUTER_API_KEY not set"))
  }
  if (SKIP_LLM) {
    return(list(success = FALSE, error = "STAGE4_SKIP_LLM enabled"))
  }

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = prompt$system),
      list(role = "user", content = prompt$user)
    ),
    temperature = 0.2
  )

  resp <- try(httr::POST(
    url = "https://openrouter.ai/api/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    httr::timeout(TIMEOUT_SECONDS)
  ), silent = TRUE)

  if (inherits(resp, "try-error")) {
    return(list(success = FALSE, error = conditionMessage(attr(resp, "condition"))))
  }
  if (httr::status_code(resp) >= 300) {
    return(list(success = FALSE, error = sprintf("HTTP %s", httr::status_code(resp))))
  }

  payload <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  message_text <- parsed$choices[[1]]$message$content %||% ""
  message_text <- strip_json_fences(message_text)
  result <- try(jsonlite::fromJSON(message_text, simplifyVector = FALSE), silent = TRUE)
  if (inherits(result, "try-error")) {
    return(list(success = FALSE, error = "Failed to parse Stage 4 JSON", raw = message_text))
  }
  list(success = TRUE, model = model, interpretation = result, raw = message_text)
}

compose_local_theory <- function(case) {
  families <- unique(vapply(case$terms, term_family, character(1)))
  brief <- case$system_brief %||% list()
  clean_sentence <- function(x) {
    x <- trimws(x)
    gsub("[.]+$", "", x)
  }
  actors <- clean_sentence(brief$actors %||% "Actors in this network")
  tie_meaning <- clean_sentence(brief$tie_meaning %||% "A tie indicates a relationship")

  mechanism_phrases <- character(0)
  if (any(families %in% c("nodematch", "absdiff"))) {
    mechanism_phrases <- c(mechanism_phrases, "similar actors are more likely to be connected")
  }
  if (any(families %in% c("gwesp", "gwdsp", "twopath", "ttriple", "ctriple"))) {
    mechanism_phrases <- c(mechanism_phrases, "shared partners and local clustering help organize ties")
  }
  if (any(families %in% c("gwdegree", "gwidegree", "gwodegree"))) {
    mechanism_phrases <- c(mechanism_phrases, "some actors occupy more central or active positions than others")
  }
  if ("mutual" %in% families) {
    mechanism_phrases <- c(mechanism_phrases, "relationships tend to be reciprocated")
  }
  if (any(families %in% c("nodefactor", "nodeifactor", "nodeofactor", "nodecov", "nodeicov", "nodeocov"))) {
    mechanism_phrases <- c(mechanism_phrases, "actor attributes shift who tends to send or receive ties")
  }
  if (!length(mechanism_phrases)) {
    mechanism_phrases <- "the baseline rate of ties is the dominant recorded mechanism"
  }

  gof_sentence <- if (isTRUE(case$gof_pass)) {
    "The recorded GOF check passed, so this theory is better supported by the fitted diagnostics."
  } else if (identical(case$gof_pass, FALSE)) {
    "The recorded GOF check did not pass, so this theory should be treated as a provisional interpretation rather than a settled account."
  } else {
    "The GOF status is unavailable, so the strength of this theory still needs diagnostic review."
  }

  paste(
    sprintf("%s. %s.", actors, tie_meaning),
    sprintf(
      "In plain terms, the model's theory is that ties are patterned rather than random: %s.",
      paste(mechanism_phrases, collapse = "; ")
    ),
    gof_sentence
  )
}

local_interpretation_draft <- function(case, error) {
  term_items <- lapply(case$terms, function(term) {
    list(
      term = term,
      mechanism = term_mechanism(term),
      evidence = "Generated without LLM; inspect coefficient table and GOF before using as final interpretation.",
      caution = "This is a deterministic fallback, not the Stage 4 LLM interpretation."
    )
  })

  gof_note <- if (isTRUE(case$gof_pass)) {
    "GOF passed under the recorded threshold."
  } else if (identical(case$gof_pass, FALSE)) {
    "GOF did not pass, so mechanism claims should be treated as provisional."
  } else {
    "GOF pass status is unavailable."
  }

  list(
    headline = sprintf("%s final specification combines %s mechanisms.", case$dataset, paste(unique(vapply(case$terms, term_family, character(1))), collapse = ", ")),
    human_understandable_theory = compose_local_theory(case),
    mechanism_explanation = paste(
      "The final ERGM terms point to the mechanisms listed in the term_interpretations block.",
      gof_note,
      "Run with OPENROUTER_API_KEY and STAGE4_SKIP_LLM=false for the full interpretation LLM output."
    ),
    term_interpretations = term_items,
    evidence_assessment = sprintf("BIC=%s; max_abs_z=%s. %s", case$final_bic, case$final_max_abs_z, gof_note),
    limitations = c(
      "Fallback interpretation was produced without an LLM call.",
      "ERGM coefficients are conditional associations and should not be presented as causal effects.",
      "Model fit limitations must be reported alongside mechanism claims."
    ),
    plain_language = "The model describes which network tendencies are consistent with the observed ties, but the current fallback does not provide a polished domain interpretation.",
    recommended_followups = c(
      "Run Stage 4 with an API key enabled.",
      "Review GOF diagnostics before using the interpretation in a paper or presentation."
    ),
    stage4_error = error
  )
}

write_prompt_file <- function(dataset, prompt) {
  ensure_dir(STAGE4_PROMPT_DIR)
  path <- file.path(STAGE4_PROMPT_DIR, sprintf("%s_stage4_prompt.txt", dataset))
  writeLines(c("SYSTEM PROMPT", prompt$system, "", "USER PROMPT", prompt$user), path, useBytes = TRUE)
  path
}

interpret_case <- function(case) {
  prompt <- build_interpretation_prompt(case)
  prompt_path <- write_prompt_file(case$dataset, prompt)
  llm_result <- call_interpretation_llm(prompt)

  if (isTRUE(llm_result$success)) {
    interpretation <- llm_result$interpretation
    llm_success <- TRUE
    error <- NA_character_
  } else {
    interpretation <- local_interpretation_draft(case, llm_result$error)
    llm_success <- FALSE
    error <- llm_result$error
  }

  list(
    dataset = case$dataset,
    input_source = case$input_source,
    spec_id = case$spec_id,
    model = INTERPRETATION_MODEL,
    llm_success = llm_success,
    error = error,
    prompt_path = prompt_path,
    terms = case$terms,
    final_bic = case$final_bic,
    final_aic = case$final_aic,
    final_max_abs_z = case$final_max_abs_z,
    gof_pass = case$gof_pass,
    coefficients = case$coefficients,
    interpretation = interpretation
  )
}

write_markdown_report <- function(results, path) {
  lines <- c(
    "# Stage 4 ERGM Mechanism Interpretations",
    "",
    sprintf("Generated: %s", Sys.time()),
    sprintf("Interpretation model: %s", INTERPRETATION_MODEL),
    ""
  )

  for (res in results) {
    interp <- res$interpretation
    lines <- c(
      lines,
      sprintf("## %s", res$dataset),
      "",
      sprintf("- Input source: `%s`", res$input_source),
      sprintf("- Specification: `%s`", res$spec_id),
      sprintf("- LLM success: `%s`", res$llm_success),
      sprintf("- BIC: `%s`", res$final_bic),
      sprintf("- GOF max |z|: `%s`", res$final_max_abs_z),
      sprintf("- GOF pass: `%s`", res$gof_pass),
      "",
      sprintf("**Headline:** %s", interp$headline %||% ""),
      "",
      "**Human-Understandable Theory**",
      "",
      interp$human_understandable_theory %||% "",
      "",
      interp$mechanism_explanation %||% "",
      "",
      "**Terms**",
      ""
    )

    term_items <- interp$term_interpretations %||% list()
    if (length(term_items)) {
      for (item in term_items) {
        lines <- c(
          lines,
          sprintf("- `%s`: %s", item$term %||% "", item$mechanism %||% "")
        )
      }
    }

    limitations <- interp$limitations %||% character(0)
    if (length(limitations)) {
      lines <- c(lines, "", "**Limitations**", "")
      for (lim in limitations) {
        lines <- c(lines, sprintf("- %s", lim))
      }
    }

    lines <- c(
      lines,
      "",
      "**Plain Language**",
      "",
      interp$plain_language %||% "",
      ""
    )
  }

  writeLines(lines, path, useBytes = TRUE)
}

write_summary_csv <- function(results, path) {
  rows <- lapply(results, function(res) {
    data.frame(
      dataset = res$dataset,
      input_source = res$input_source,
      spec_id = res$spec_id,
      model = res$model,
      llm_success = res$llm_success,
      final_bic = res$final_bic,
      final_max_abs_z = res$final_max_abs_z,
      gof_pass = res$gof_pass,
      headline = res$interpretation$headline %||% NA_character_,
      human_understandable_theory = res$interpretation$human_understandable_theory %||% NA_character_,
      error = res$error %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  write.csv(do.call(rbind, rows), path, row.names = FALSE)
}

main <- function() {
  ensure_dir("results")
  stage1_results <- read_required_rds(STAGE1_RDS)
  stage2_baselines <- coerce_baseline_terms(read_required_rds(STAGE2_BASELINE_RDS))

  cases <- build_refinement_cases(stage1_results, stage2_baselines)
  if (is.null(cases) || !length(cases)) {
    message("[Stage4] Refinement summary not found; falling back to best successful Stage 3 MCMLE fits.")
    cases <- build_mcmle_cases(stage1_results, stage2_baselines)
  }
  if (!length(cases)) {
    stop("No Stage 3 outputs found for Stage 4 interpretation.")
  }

  message(sprintf("[Stage4] Interpreting %d fitted specifications with model %s", length(cases), INTERPRETATION_MODEL))
  if (SKIP_LLM) {
    message("[Stage4] STAGE4_SKIP_LLM is enabled; writing local drafts only.")
  }

  results <- lapply(cases, function(case) {
    message(sprintf("[Stage4] %s | %s", case$dataset, case$spec_id))
    interpret_case(case)
  })

  saveRDS(results, STAGE4_OUTPUT_RDS)
  jsonlite::write_json(results, STAGE4_OUTPUT_JSON, pretty = TRUE, auto_unbox = TRUE)
  write_summary_csv(results, STAGE4_OUTPUT_CSV)
  write_markdown_report(results, STAGE4_OUTPUT_MD)

  cat("Stage IV interpretation complete. Outputs:\n")
  cat(sprintf("- %s\n", STAGE4_OUTPUT_RDS))
  cat(sprintf("- %s\n", STAGE4_OUTPUT_JSON))
  cat(sprintf("- %s\n", STAGE4_OUTPUT_CSV))
  cat(sprintf("- %s\n", STAGE4_OUTPUT_MD))
  cat(sprintf("- %s/*.txt\n", STAGE4_PROMPT_DIR))
}

if (!interactive() && sys.nframe() == 0) {
  main()
}
