#!/usr/bin/env Rscript
# Consolidated Guardrail System for ERGM Specifications
# Includes all guardrails: basic, triangles, and library enforcement

# =============================================================================
# GUARDRAIL FUNCTIONS
# =============================================================================

check_guardrail_1 <- function(terms) {
  has_edges <- "edges" %in% terms
  term_count <- length(terms)
  return(has_edges && term_count >= 3 && term_count <= 8)
}

check_guardrail_2 <- function(terms, network_obj) {
  closure_terms <- terms[grepl("^(gwesp|gwdsp)", terms)]
  return(length(closure_terms) <= 1)
}

check_guardrail_3 <- function(terms, network_obj) {
  # Check categorical parameter limits
  nodematch_terms <- terms[grepl("^nodematch", terms)]
  
  for (term in nodematch_terms) {
    attr_name <- gsub("nodematch\\(['\"]?([^'\"]+)['\"]?\\)", "\\1", term)
    attr_name <- gsub("nodematch\\.", "", attr_name)
    
    if (attr_name %in% list.vertex.attributes(network_obj)) {
      attr_values <- get.vertex.attribute(network_obj, attr_name)
      unique_vals <- unique(attr_values[!is.na(attr_values)])
      n_categories <- length(unique_vals)
      if (n_categories == 0) next
      
      if (is.character(attr_values) || is.factor(attr_values)) {
        free_params <- n_categories - 1
        if (free_params > 20) return(FALSE)
        
        n_nodes <- network.size(network_obj)
        expected_cell_count <- n_nodes / n_categories
        if (expected_cell_count < 5) return(FALSE)
      }
    }
  }
  return(TRUE)
}

check_guardrail_4 <- function(terms) {
  nodematch_terms <- terms[grepl("^nodematch", terms)]
  nodefactor_terms <- terms[grepl("^nodefactor", terms)]
  
  for (nodematch_term in nodematch_terms) {
    attr_name <- gsub("nodematch\\(['\"]?([^'\"]+)['\"]?\\)", "\\1", nodematch_term)
    attr_name <- gsub("nodematch\\.", "", attr_name)
    
    for (nodefactor_term in nodefactor_terms) {
      factor_attr <- gsub("nodefactor\\(['\"]?([^'\"]+)['\"]?\\)", "\\1", nodefactor_term)
      factor_attr <- gsub("nodefactor\\.", "", factor_attr)
      
      if (attr_name == factor_attr) return(FALSE)
    }
  }
  return(TRUE)
}

check_guardrail_5_no_triangles <- function(terms) {
  has_triangles <- "triangles" %in% terms
  if (has_triangles) {
    return(list(
      pass = FALSE,
      reason = "Guardrail 5: 'triangles' term is forbidden"
    ))
  }
  return(list(
    pass = TRUE,
    reason = "Guardrail 5: No triangles term found"
  ))
}

# =============================================================================
# LIBRARY VALIDATION AND FILTERING
# =============================================================================

INVALID_ERGM_TERMS <- c(
  "triangles", "triangle", "transitive", "reciprocity"
)

filter_library_terms <- function(library_terms) {
  cat("🔍 Filtering library terms for validity...\n")
  
  # Remove invalid terms
  valid_terms <- library_terms[!library_terms %in% INVALID_ERGM_TERMS]
  
  # More permissive validation - just check it's not obviously invalid
  # Allow terms with and without parameters
  valid_patterns <- c(
    "^edges$", "^degree", "^gwdegree", "^gwidegree", "^gwodegree", "^gwesp", "^gwdsp", 
    "^nodematch", "^nodefactor", "^nodeifactor", "^nodeofactor",
    "^nodecov", "^nodeicov", "^nodeocov", "^absdiff",
    "^mutual$", "^istar", "^ostar", "^idegree", "^odegree"
  )
  
  pattern_matches <- sapply(valid_terms, function(term) {
    any(sapply(valid_patterns, function(pattern) {
      grepl(pattern, term)
    }))
  })
  
  filtered_terms <- valid_terms[pattern_matches]
  
  removed_terms <- setdiff(library_terms, filtered_terms)
  if (length(removed_terms) > 0) {
    cat(sprintf("   Removed invalid terms: %s\n", paste(removed_terms, collapse = ", ")))
  }
  
  cat(sprintf("   Original terms: %d\n", length(library_terms)))
  cat(sprintf("   Valid terms: %d\n", length(filtered_terms)))
  
  return(filtered_terms)
}

check_guardrail_6_library_enforcement <- function(terms, library_terms) {
  # Check if base terms (without parameters) are in library
  # Handle both dot notation and parentheses notation in library
  base_terms <- sapply(terms, function(term) {
    # Extract base term name (remove parameters)
    if (grepl("^edges$", term)) return("edges")
    if (grepl("^degree", term)) return("degree")
    if (grepl("^gwdegree", term)) return("gwdegree")
    if (grepl("^gwesp", term)) return("gwesp")
    if (grepl("^gwdsp", term)) return("gwdsp")
    if (grepl("^nodematch", term)) {
      # Handle both dot notation and parentheses notation
      if (grepl("\\.", term)) {
        # Dot notation: nodematch.alcohol -> nodematch(alcohol)
        attr_name <- sub("nodematch\\.", "", term)
        return(paste0("nodematch(", attr_name, ")"))
      } else {
        # Parentheses notation: nodematch(alcohol) -> nodematch(alcohol)
        attr_match <- regmatches(term, regexec("nodematch\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodematch(", attr_match[[1]][2], ")"))
        }
        return("nodematch")
      }
    }
    if (grepl("^nodefactor", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("nodefactor\\.", "", term)
        return(paste0("nodefactor(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("nodefactor\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodefactor(", attr_match[[1]][2], ")"))
        }
        return("nodefactor")
      }
    }
    if (grepl("^nodeifactor", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("nodeifactor\\.", "", term)
        return(paste0("nodeifactor(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("nodeifactor\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodeifactor(", attr_match[[1]][2], ")"))
        }
        return("nodeifactor")
      }
    }
    if (grepl("^nodeofactor", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("nodeofactor\\.", "", term)
        return(paste0("nodeofactor(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("nodeofactor\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodeofactor(", attr_match[[1]][2], ")"))
        }
        return("nodeofactor")
      }
    }
    if (grepl("^nodecov", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("nodecov\\.", "", term)
        return(paste0("nodecov(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("nodecov\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodecov(", attr_match[[1]][2], ")"))
        }
        return("nodecov")
      }
    }
    if (grepl("^nodeicov", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("nodeicov\\.", "", term)
        return(paste0("nodeicov(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("nodeicov\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodeicov(", attr_match[[1]][2], ")"))
        }
        return("nodeicov")
      }
    }
    if (grepl("^nodeocov", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("nodeocov\\.", "", term)
        return(paste0("nodeocov(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("nodeocov\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("nodeocov(", attr_match[[1]][2], ")"))
        }
        return("nodeocov")
      }
    }
    if (grepl("^absdiff", term)) {
      if (grepl("\\.", term)) {
        attr_name <- sub("absdiff\\.", "", term)
        return(paste0("absdiff(", attr_name, ")"))
      } else {
        attr_match <- regmatches(term, regexec("absdiff\\(['\"]?([^'\"]+)['\"]?\\)", term))
        if (length(attr_match[[1]]) > 1) {
          return(paste0("absdiff(", attr_match[[1]][2], ")"))
        }
        return("absdiff")
      }
    }
    if (grepl("^mutual$", term)) return("mutual")
    return(term) # Return as-is if no pattern matches
  })
  
  # Convert library terms to all formats for comparison
  library_terms_all_formats <- c(library_terms)
  for (lib_term in library_terms) {
    if (grepl("^nodematch\\.", lib_term)) {
      # Dot notation: nodematch.alcohol -> nodematch(alcohol), nodematch("alcohol"), nodematch('alcohol')
      attr_name <- sub("nodematch\\.", "", lib_term)
      library_terms_all_formats <- c(library_terms_all_formats, 
                                    paste0("nodematch(", attr_name, ")"),
                                    paste0("nodematch(\"", attr_name, "\")"),
                                    paste0("nodematch('", attr_name, "')"))
    } else if (grepl("^nodefactor\\.", lib_term)) {
      attr_name <- sub("nodefactor\\.", "", lib_term)
      library_terms_all_formats <- c(library_terms_all_formats, 
                                    paste0("nodefactor(", attr_name, ")"),
                                    paste0("nodefactor(\"", attr_name, "\")"),
                                    paste0("nodefactor('", attr_name, "')"))
    } else if (grepl("^nodecov\\.", lib_term)) {
      attr_name <- sub("nodecov\\.", "", lib_term)
      library_terms_all_formats <- c(library_terms_all_formats, 
                                    paste0("nodecov(", attr_name, ")"),
                                    paste0("nodecov(\"", attr_name, "\")"),
                                    paste0("nodecov('", attr_name, "')"))
    } else if (grepl("^absdiff\\.", lib_term)) {
      attr_name <- sub("absdiff\\.", "", lib_term)
      library_terms_all_formats <- c(library_terms_all_formats, 
                                    paste0("absdiff(", attr_name, ")"),
                                    paste0("absdiff(\"", attr_name, "\")"),
                                    paste0("absdiff('", attr_name, "')"))
    } else if (grepl("^nodematch\\(", lib_term)) {
      # Parentheses notation: nodematch("Grade") -> nodematch(Grade), nodematch('Grade')
      attr_match <- regmatches(lib_term, regexec("nodematch\\(['\"]?([^'\"]+)['\"]?\\)", lib_term))
      if (length(attr_match[[1]]) > 1) {
        attr_name <- attr_match[[1]][2]
        library_terms_all_formats <- c(library_terms_all_formats, 
                                      paste0("nodematch(", attr_name, ")"),
                                      paste0("nodematch('", attr_name, "')"))
      }
    } else if (grepl("^nodefactor\\(", lib_term)) {
      attr_match <- regmatches(lib_term, regexec("nodefactor\\(['\"]?([^'\"]+)['\"]?\\)", lib_term))
      if (length(attr_match[[1]]) > 1) {
        attr_name <- attr_match[[1]][2]
        library_terms_all_formats <- c(library_terms_all_formats, 
                                      paste0("nodefactor(", attr_name, ")"),
                                      paste0("nodefactor('", attr_name, "')"))
      }
    } else if (grepl("^nodecov\\(", lib_term)) {
      attr_match <- regmatches(lib_term, regexec("nodecov\\(['\"]?([^'\"]+)['\"]?\\)", lib_term))
      if (length(attr_match[[1]]) > 1) {
        attr_name <- attr_match[[1]][2]
        library_terms_all_formats <- c(library_terms_all_formats, 
                                      paste0("nodecov(", attr_name, ")"),
                                      paste0("nodecov('", attr_name, "')"))
      }
    } else if (grepl("^absdiff\\(", lib_term)) {
      attr_match <- regmatches(lib_term, regexec("absdiff\\(['\"]?([^'\"]+)['\"]?\\)", lib_term))
      if (length(attr_match[[1]]) > 1) {
        attr_name <- attr_match[[1]][2]
        library_terms_all_formats <- c(library_terms_all_formats, 
                                      paste0("absdiff(", attr_name, ")"),
                                      paste0("absdiff('", attr_name, "')"))
      }
    } else if (grepl("^gwesp\\(", lib_term)) {
      # Handle gwesp with parameters: gwesp(0.5) -> gwesp
      library_terms_all_formats <- c(library_terms_all_formats, "gwesp")
    } else if (grepl("^gwdsp\\(", lib_term)) {
      # Handle gwdsp with parameters: gwdsp(0.5) -> gwdsp
      library_terms_all_formats <- c(library_terms_all_formats, "gwdsp")
    } else if (grepl("^gwdegree\\(", lib_term)) {
      # Handle gwdegree with parameters: gwdegree(0.5) -> gwdegree
      library_terms_all_formats <- c(library_terms_all_formats, "gwdegree")
    } else if (grepl("^degree\\(", lib_term)) {
      # Handle degree with parameters: degree(1) -> degree
      library_terms_all_formats <- c(library_terms_all_formats, "degree")
    }
  }
  
  invalid_terms <- base_terms[!base_terms %in% library_terms_all_formats]
  
  if (length(invalid_terms) > 0) {
    return(list(
      pass = FALSE,
      reason = sprintf("Guardrail 6: Base terms not in admissible library L*: %s", 
                      paste(invalid_terms, collapse = ", "))
    ))
  }
  
  return(list(
    pass = TRUE,
    reason = "Guardrail 6: All base terms are from admissible library L*"
  ))
}

# =============================================================================
# MAIN GUARDRAIL CHECKING FUNCTION
# =============================================================================

check_all_guardrails <- function(terms, network_obj, library_terms = NULL) {
  guardrail_1 <- check_guardrail_1(terms)
  guardrail_2 <- check_guardrail_2(terms, network_obj)
  guardrail_3 <- check_guardrail_3(terms, network_obj)
  guardrail_4 <- check_guardrail_4(terms)
  guardrail_5 <- check_guardrail_5_no_triangles(terms)
  
  # Library enforcement (optional)
  if (!is.null(library_terms)) {
    guardrail_6 <- check_guardrail_6_library_enforcement(terms, library_terms)
    all_passed <- all(c(guardrail_1, guardrail_2, guardrail_3, guardrail_4, 
                       guardrail_5$pass, guardrail_6$pass))
    
    return(list(
      guardrail_1 = guardrail_1,
      guardrail_2 = guardrail_2,
      guardrail_3 = guardrail_3,
      guardrail_4 = guardrail_4,
      guardrail_5 = guardrail_5,
      guardrail_6 = guardrail_6,
      all_passed = all_passed
    ))
  } else {
    all_passed <- all(c(guardrail_1, guardrail_2, guardrail_3, guardrail_4, guardrail_5$pass))
    
    return(list(
      guardrail_1 = guardrail_1,
      guardrail_2 = guardrail_2,
      guardrail_3 = guardrail_3,
      guardrail_4 = guardrail_4,
      guardrail_5 = guardrail_5,
      all_passed = all_passed
    ))
  }
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

add_curved_parameters <- function(terms) {
  corrected_terms <- c()
  
  for (term in terms) {
    if (term == "gwesp") {
      corrected_terms <- c(corrected_terms, "gwesp(0.5, fixed=TRUE)")
    } else if (term == "gwdsp") {
      corrected_terms <- c(corrected_terms, "gwdsp(0.5, fixed=TRUE)")
    } else if (term == "gwdegree") {
      corrected_terms <- c(corrected_terms, "gwdegree(0.5, fixed=TRUE)")
    } else if (term == "degree") {
      corrected_terms <- c(corrected_terms, "degree(1)")
    } else {
      corrected_terms <- c(corrected_terms, term)
    }
  }
  
  return(corrected_terms)
}

print_guardrail_results <- function(guardrail_results) {
  cat("📊 Guardrail Results:\n")
  cat(sprintf("   Guardrail 1 (Baseline + Count): %s\n", 
              ifelse(guardrail_results$guardrail_1, "✅ PASS", "❌ FAIL")))
  cat(sprintf("   Guardrail 2 (Closure Stats): %s\n", 
              ifelse(guardrail_results$guardrail_2, "✅ PASS", "❌ FAIL")))
  cat(sprintf("   Guardrail 3 (Categorical Params): %s\n", 
              ifelse(guardrail_results$guardrail_3, "✅ PASS", "❌ FAIL")))
  cat(sprintf("   Guardrail 4 (Mixing vs Factors): %s\n", 
              ifelse(guardrail_results$guardrail_4, "✅ PASS", "❌ FAIL")))
  cat(sprintf("   Guardrail 5 (No Triangles): %s\n", 
              ifelse(guardrail_results$guardrail_5$pass, "✅ PASS", "❌ FAIL")))
  
  if (!is.null(guardrail_results$guardrail_6)) {
    cat(sprintf("   Guardrail 6 (Library Enforcement): %s\n", 
                ifelse(guardrail_results$guardrail_6$pass, "✅ PASS", "❌ FAIL")))
  }
  
  cat(sprintf("   Overall: %s\n", 
              ifelse(guardrail_results$all_passed, "✅ ALL PASS", "❌ FAILED")))
}

cat("✅ Consolidated guardrail system created\n")
