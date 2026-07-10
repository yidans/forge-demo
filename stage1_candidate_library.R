#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(network) })

# ---------- utils ----------
safe_quote <- function(name) {
  escaped <- gsub("\"", "\\\\\"", name)
  sprintf('"%s"', escaped)
}

classify_attribute <- function(values) {
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    return(list(type = "empty", unique = 0, classification = "none"))
  }
  unique_count <- length(unique(values))
  is_logical   <- is.logical(values)
  is_factor    <- is.factor(values)
  is_character <- is.character(values)
  is_numeric   <- is.numeric(values) && !is.logical(values)

  if (is_logical) {
    classification <- "categorical"
  } else if (is_factor || is_character) {
    classification <- "categorical"
  } else if (is_numeric) {
    integer_like <- all(abs(values - round(values)) < .Machine$double.eps ^ 0.5)
    classification <- if (unique_count <= 8 && integer_like) "categorical" else "numeric"
  } else {
    classification <- "unknown"
  }
  list(type = class(values)[1], unique = unique_count, classification = classification)
}

# ---------- guardrails ----------
guardrail_ok_for_categorical <- function(unique_count, n_nodes,
                                         max_free_params = 20,
                                         min_expected_cell = 5) {
  free_params <- unique_count - 1
  if (free_params > max_free_params) return(FALSE)
  (n_nodes / unique_count) >= min_expected_cell
}

# dyad-based guard for nodemix (better than node-count proxies)
guardrail_ok_for_nodemix_dyads <- function(values, directed, min_expected_dyads = 30) {
  v <- values[!is.na(values)]
  if (length(v) == 0) return(FALSE)
  tbl <- table(v)
  if (length(tbl) < 2) return(FALSE)
  gs <- as.numeric(tbl)

  if (!directed) {
    same  <- ifelse(gs >= 2, gs * (gs - 1) / 2, 0)
    cross <- outer(gs, gs)
    cross[lower.tri(cross, diag = TRUE)] <- NA
    mix_cells <- c(same, as.vector(cross[!is.na(cross)]))
  } else {
    mix_cells <- as.vector(outer(gs, gs))  # ordered pairs
  }
  length(mix_cells) > 0 && all(mix_cells >= min_expected_dyads)
}

# ---------- structural terms ----------
build_structural_terms <- function(is_directed,
                                   is_bipartite,
                                   lambda_values = c(0.25, 0.5),
                                   include_twopath = TRUE,
                                   include_triples = TRUE,
                                   include_kstar = FALSE,
                                   max_kstar_k = 3) {
  terms <- c("edges")

  # Geometrically-weighted families (preferred)
  for (lambda in lambda_values) {
    terms <- c(terms,
      sprintf("gwesp(decay=%s, fixed=TRUE)", lambda),
      sprintf("gwdsp(decay=%s, fixed=TRUE)", lambda)
    )
  }

  if (is_directed) {
    terms <- c(terms, "mutual")
    for (lambda in lambda_values) {
      terms <- c(terms,
        sprintf("gwidegree(decay=%s, fixed=TRUE)", lambda),
        sprintf("gwodegree(decay=%s, fixed=TRUE)", lambda)
      )
    }
    if (include_triples) terms <- c(terms, "ttriple", "ctriple")
    if (include_kstar) {
      ks <- seq_len(max_kstar_k)
      terms <- c(terms, sprintf("istar(%s)", paste(ks, collapse=",")),
                        sprintf("ostar(%s)", paste(ks, collapse=",")))
    }
  } else {
    for (lambda in lambda_values) {
      terms <- c(terms, sprintf("gwdegree(decay=%s, fixed=TRUE)", lambda))
    }
    # Do NOT include triangle (we prefer gwesp and avoid collinearity).
    if (include_twopath && !is_bipartite) terms <- c(terms, "twopath")
    if (include_kstar) {
      ks <- seq_len(max_kstar_k)
      terms <- c(terms, sprintf("kstar(%s)", paste(ks, collapse=",")))
    }
  }

  unique(terms)
}

# ---------- dyadic covariates (edgecov) ----------
build_dyad_covariate_terms <- function(network_obj, dyad_covariates = NULL) {
  if (is.null(dyad_covariates) || length(dyad_covariates) == 0) {
    return(list(terms = character(0), accepted = character(0), rejected = character(0)))
  }
  n <- network.size(network_obj)
  undirected <- !network::is.directed(network_obj)

  terms <- c(); accepted <- c(); rejected <- c()
  for (nm in names(dyad_covariates)) {
    M <- dyad_covariates[[nm]]
    ok_dim <- is.matrix(M) && all(dim(M) == c(n, n))
    if (!ok_dim) { rejected <- c(rejected, nm); next }
    if (undirected && !isTRUE(all.equal(M, t(M)))) { rejected <- c(rejected, nm); next }
    terms <- c(terms, sprintf("edgecov(%s)", safe_quote(nm)))
    accepted <- c(accepted, nm)
  }
  list(terms = unique(terms), accepted = accepted, rejected = rejected)
}

# ---------- attribute terms ----------
build_attribute_terms <- function(network_obj,
                                  include_main_effects = TRUE,
                                  include_numeric_homophily = TRUE,
                                  include_categorical_homophily = TRUE,
                                  include_mixing = TRUE,
                                  max_free_params = 20,
                                  min_expected_cell = 5,
                                  min_expected_dyads_mix = 30) {

  attrs <- setdiff(list.vertex.attributes(network_obj), c("na", "vertex.names"))
  n_nodes <- network.size(network_obj)
  is_dir  <- network::is.directed(network_obj)

  terms <- c()
  attr_details <- list()

  for (attr in attrs) {
    values <- network::get.vertex.attribute(network_obj, attr)
    meta <- classify_attribute(values)
    attr_details[[attr]] <- meta

    # Numeric main effects on tie propensity
    if (include_main_effects && meta$classification == "numeric" && meta$unique >= 2) {
      terms <- c(terms, sprintf("nodecov(%s)", safe_quote(attr)))
      if (is_dir) {
        terms <- c(terms,
                   sprintf("nodeicov(%s)", safe_quote(attr)),
                   sprintf("nodeocov(%s)", safe_quote(attr)))
      }
    }

    # Categorical: match / composition / mixing (under guardrails)
    if (meta$classification == "categorical" && meta$unique >= 2) {
      cat_ok <- guardrail_ok_for_categorical(meta$unique, n_nodes,
                                             max_free_params, min_expected_cell)
      if (cat_ok) {
        if (!is_dir) {
          terms <- c(terms,
                     sprintf("nodematch(%s)", safe_quote(attr)),
                     sprintf("nodefactor(%s)", safe_quote(attr)))
        } else {
          terms <- c(terms,
                     sprintf("nodematch(%s)", safe_quote(attr)),
                     sprintf("nodeifactor(%s)", safe_quote(attr)),
                     sprintf("nodeofactor(%s)", safe_quote(attr)))
        }
      }
      if (include_mixing) {
        dyad_ok <- guardrail_ok_for_nodemix_dyads(values, directed = is_dir,
                                                  min_expected_dyads = min_expected_dyads_mix)
        if (dyad_ok) {
          terms <- c(terms, sprintf("nodemix(%s)", safe_quote(attr)))
        }
      }
    }

    # Numeric homophily
    if (include_numeric_homophily && meta$classification == "numeric" && meta$unique >= 2) {
      terms <- c(terms, sprintf("absdiff(%s)", safe_quote(attr)))
    }
  }

  list(terms = unique(terms), details = attr_details)
}

# ---------- dominance rules ----------
# Prefer GW families over their low-order cousins, and avoid near duplicates.
apply_dominance_rules <- function(terms, is_directed) {
  keep <- terms

  # Prefer gwdegree over kstar; gwidegree/gwodegree over istar/ostar
  if (!is_directed) {
    if (any(grepl("^gwdegree\\(", keep))) {
      keep <- keep[!grepl("^kstar\\(", keep)]
    }
  } else {
    if (any(grepl("^gwidegree\\(", keep))) keep <- keep[!grepl("^istar\\(", keep)]
    if (any(grepl("^gwodegree\\(", keep))) keep <- keep[!grepl("^ostar\\(", keep)]
  }

  # Prefer gwesp over triangle (we already omit triangle entirely) and over redundant twopaths
  # but twopath can still be informative; drop only if user wants a minimal set.
  # Here we keep twopath; if you want to drop it when gwesp exists, uncomment the next line:
  # if (any(grepl("^gwesp\\(", keep))) keep <- keep[keep != "twopath"]

  unique(keep)
}

# ---------- assembly ----------
build_admissible_library <- function(network_obj,
                                     lambda_values = c(0.25, 0.5),
                                     include_twopath  = TRUE,
                                     include_triples  = TRUE,
                                     include_kstar    = FALSE,
                                     max_kstar_k      = 3,
                                     include_main_effects = TRUE,
                                     include_numeric_homophily = TRUE,
                                     include_categorical_homophily = TRUE,
                                     include_mixing = TRUE,
                                     max_free_params = 20,
                                     min_expected_cell = 5,
                                     min_expected_dyads_mix = 30,
                                     dyad_covariates = NULL) {

  stopifnot(inherits(network_obj, "network"))
  is_dir <- network::is.directed(network_obj)
  is_bip <- isTRUE(is.bipartite(network_obj))

  base_terms <- build_structural_terms(
    is_directed      = is_dir,
    is_bipartite     = is_bip,
    lambda_values    = lambda_values,
    include_twopath  = include_twopath,
    include_triples  = include_triples,
    include_kstar    = include_kstar,
    max_kstar_k      = max_kstar_k
  )

  attr_terms <- build_attribute_terms(
    network_obj                 = network_obj,
    include_main_effects        = include_main_effects,
    include_numeric_homophily   = include_numeric_homophily,
    include_categorical_homophily = include_categorical_homophily,
    include_mixing              = include_mixing,
    max_free_params             = max_free_params,
    min_expected_cell           = min_expected_cell,
    min_expected_dyads_mix      = min_expected_dyads_mix
  )

  dyad_terms <- build_dyad_covariate_terms(network_obj, dyad_covariates)

  library_terms <- unique(c(base_terms, attr_terms$terms, dyad_terms$terms))
  library_terms <- apply_dominance_rules(library_terms, is_directed = is_dir)

  list(
    terms = library_terms,
    base_terms = base_terms,
    attribute_terms = attr_terms$terms,
    dyadcov_terms = dyad_terms$terms,
    dyadcov_accepted = dyad_terms$accepted,
    dyadcov_rejected = dyad_terms$rejected,
    attribute_details = attr_terms$details,
    directed = is_dir,
    bipartite = is_bip,
    lambda = lambda_values,
    guardrails = list(
      max_free_params = max_free_params,
      min_expected_cell = min_expected_cell,
      min_expected_dyads_mix = min_expected_dyads_mix
    ),
    toggles = list(
      include_twopath = include_twopath,
      include_triples = include_triples,
      include_kstar = include_kstar,
      include_main_effects = include_main_effects,
      include_numeric_homophily = include_numeric_homophily,
      include_categorical_homophily = include_categorical_homophily,
      include_mixing = include_mixing
    )
  )
}
