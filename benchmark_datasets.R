# Benchmark Datasets for LLM-Guided ERGM Specification
# Comprehensive collection of canonical ERGM benchmark networks
# All datasets are available in R via ergm/statnet packages

library(ergm)
library(network)
library(statnet)
library(networkDynamic)
library(readxl)
library(R.matlab)

load_noordin_top_dataset <- function() {
  file_candidates <- c(
    file.path("data", "raw", "external", "noordin_top_terrorist_network.xlsx")
  )
  file_path <- file_candidates[file.exists(file_candidates)][1]
  if (is.na(file_path)) {
    stop("noordin_top_terrorist_network.xlsx not found under ./data/raw/external/.")
  }

  raw_data <- readxl::read_excel(file_path, sheet = "Data")

  if (!"NAME" %in% names(raw_data)) {
    stop("Expected 'NAME' column not found in Noordin Top dataset.")
  }

  node_names <- raw_data$NAME

  comm_cols <- grep("^COMMUN[0-9]+$", names(raw_data), value = TRUE)
  if (length(comm_cols) == 0) {
    stop("Communication adjacency columns not found (expecting columns named COMMUN1...COMMUNn).")
  }

  communication_matrix <- as.matrix(raw_data[, comm_cols])
  communication_matrix[is.na(communication_matrix)] <- 0
  communication_matrix <- ifelse(communication_matrix > 0, 1, 0)

  if (nrow(communication_matrix) != ncol(communication_matrix)) {
    stop(sprintf("Communication matrix is not square: %d x %d", nrow(communication_matrix), ncol(communication_matrix)))
  }

  if (nrow(communication_matrix) != length(node_names)) {
    stop("Row count of communication matrix does not match number of actors in the dataset.")
  }

  # Symmetrise and remove self-loops to create an undirected communication network
  communication_matrix <- (communication_matrix + t(communication_matrix)) > 0
  diag(communication_matrix) <- 0
  communication_matrix <- ifelse(communication_matrix, 1, 0)

  rownames(communication_matrix) <- node_names
  colnames(communication_matrix) <- node_names

  network_obj <- network::network(communication_matrix, directed = FALSE, matrix.type = "adjacency")
  network.vertex.names(network_obj) <- node_names

  # Attach rich actor attributes when available
  attribute_columns <- c("STATUS", "ROLE", "GROUP", "NATION", "CONTACT", "MILITARY", "EDUC", "NOORDIN")
  for (attr_name in attribute_columns) {
    if (attr_name %in% names(raw_data)) {
      network::set.vertex.attribute(network_obj, tolower(attr_name), raw_data[[attr_name]])
    }
  }

  # Derived counts for multi-membership attributes
  derived_specs <- list(
    org_affiliations = "^ORGAN[0-9]+$",
    school_ties = "^SCHOOL[0-9]+$",
    kinship_ties = "^KIN[0-9]+$",
    communication_degree = "^COMMUN[0-9]+$",
    spiritual_ties = "^SOUL[0-9]+$",
    meeting_count = "^MEET[0-9]+$",
    locations_count = "^PLACE[0-9]+$"
  )

  for (attr in names(derived_specs)) {
    cols <- grep(derived_specs[[attr]], names(raw_data), value = TRUE)
    if (length(cols) > 0) {
      counts <- rowSums(as.data.frame(raw_data[, cols]), na.rm = TRUE)
      network::set.vertex.attribute(network_obj, attr, counts)
    }
  }

  final_attrs <- list.vertex.attributes(network_obj)
  final_attrs <- final_attrs[!final_attrs %in% c("na", "vertex.names")]

  metadata <- list(
    name = "Noordin Top Terrorist Network",
    description = "Undirected communication network among actors linked to the Noordin Top terrorist organisation",
    nodes = network.size(network_obj),
    edges = network.edgecount(network_obj),
    directed = FALSE,
    attributes = final_attrs,
    context = "Terrorist communication and coordination network with organisational, kinship, and operational attributes",
    literature = c(
      "Roberts & Everton (2011). Computational & Mathematical Organization Theory",
      "Everton (2012). Disrupting Dark Networks"
    )
  )

  list(network = network_obj, metadata = metadata)
}

# =============================================================================
# BENCHMARK DATASET LOADER
# =============================================================================

load_benchmark_dataset <- function(dataset_name) {
  # Load a benchmark dataset by name
  # Args: dataset_name - Name of the dataset to load
  # Returns: List containing network object, metadata, and literature references
  
  cat(sprintf("Loading benchmark dataset: %s\n", dataset_name))
  
  switch(dataset_name,
    
    # Faux Mesa High School
    "faux_mesa" = {
      data(faux.mesa.high)
      network_obj <- faux.mesa.high
      metadata <- list(
        name = "Faux Mesa High",
        description = "Synthetic high school friendship network (Statnet team)",
        nodes = network.size(network_obj),
        edges = network.edgecount(network_obj),
        directed = FALSE,
        attributes = c("Grade", "Race", "Sex"),
        context = "High school friendship network with demographic attributes",
        literature = c(
          "Hunter, Handcock & Goodreau (2008). Journal of Statistical Software",
          "Goodreau, Kitts & Morris (2009). Social Networks"
        )
      )
    },
    
    # Faux Dixon High School  
    "faux_dixon" = {
      data(faux.dixon.high)
      network_obj <- faux.dixon.high
      # Ensure the network is undirected (convert if needed)
      if (is.directed(network_obj)) {
        network_obj <- as.network(as.matrix(network_obj), directed = FALSE)
        # Copy vertex attributes
        for (attr_name in list.vertex.attributes(faux.dixon.high)) {
          set.vertex.attribute(network_obj, attr_name, get.vertex.attribute(faux.dixon.high, attr_name))
        }
      }
      metadata <- list(
        name = "Faux Dixon High",
        description = "Synthetic high school friendship network (Statnet team)",
        nodes = network.size(network_obj),
        edges = network.edgecount(network_obj),
        directed = FALSE,
        attributes = c("Grade", "Race", "Sex"),
        context = "High school friendship network with demographic attributes",
        literature = c(
          "Hunter, Handcock & Goodreau (2008). Journal of Statistical Software",
          "Goodreau, Kitts & Morris (2009). Social Networks"
        )
      )
    },
    
    # Faux Magnolia High School
    "faux_magnolia" = {
      data(faux.magnolia.high)
      network_obj <- faux.magnolia.high
      metadata <- list(
        name = "Faux Magnolia High", 
        description = "Synthetic high school friendship network (Statnet team)",
        nodes = network.size(network_obj),
        edges = network.edgecount(network_obj),
        directed = FALSE,
        attributes = c("Grade", "Race", "Sex"),
        context = "High school friendship network with demographic attributes",
        literature = c(
          "Hunter, Handcock & Goodreau (2008). Journal of Statistical Software",
          "Goodreau, Kitts & Morris (2009). Social Networks"
        )
      )
    },
    
    # Kapferer Tailor Shop
    "kapferer" = {
      data(kapferer, package = "ergm")
      network_obj <- kapferer
      metadata <- list(
        name = "Kapferer Tailor Shop",
        description = "Work interaction network in a Zambian tailor shop",
        nodes = network.size(network_obj),
        edges = network.edgecount(network_obj),
        directed = network::is.directed(network_obj),
        attributes = setdiff(list.vertex.attributes(network_obj), c("na", "vertex.names")),
        context = "Work interaction network with limited node attributes",
        literature = c(
          "Kapferer (1972). Strategy and Transaction in an African Factory",
          "Hunter & Handcock (2006). Journal of the American Statistical Association"
        )
      )
    },
    
    # Lazega Lawyers
    "lazega" = {
      # Try to load from ergm.multi package
      if (require("ergm.multi", quietly = TRUE)) {
        data(Lazega, package = "ergm.multi")
        network_obj <- Lazega
        metadata <- list(
          name = "Lazega Lawyers",
          description = "Law firm advice and friendship networks",
          nodes = network.size(network_obj),
          edges = network.edgecount(network_obj),
          directed = TRUE,
          attributes = c("practice", "status", "gender", "office", "years", "age", "seniority"),
          context = "Law firm multiplex network (advice, friendship, coworking)",
          literature = c(
            "Lazega (2001). The Collegial Phenomenon",
            "Lazega & Pattison (2001). Social Networks"
          )
        )
      } else {
        # Create placeholder if ergm.multi not available
        stop("ergm.multi package required for Lazega dataset. Install with: install.packages('ergm.multi')")
      }
    },
    
    # Krackhardt High-Tech Managers
    "krackhardt" = {
      if (require("concorR", quietly = TRUE) && require("intergraph", quietly = TRUE)) {
        # Load advice network as primary
        data(krack_advice, package = "concorR")
        # Convert igraph to network format
        network_obj <- asNetwork(krack_advice)
        metadata <- list(
          name = "Krackhardt High-Tech Managers",
          description = "Advice network among high-tech managers",
          nodes = network.size(network_obj),
          edges = network.edgecount(network_obj),
          directed = TRUE,
          attributes = c("Age", "Tenure", "Level", "Department"),
          context = "High-tech company advice network with organizational attributes",
          literature = c(
            "Krackhardt (1987). Social Networks",
            "Krackhardt & Porter (1985). Social Psychology Quarterly"
          )
        )
      } else {
        stop("concorR package required for Krackhardt dataset. Install with: install.packages('concorR')")
      }
    },
    
    # Glasgow s50 Teenage Friends & Lifestyle
    "glasgow_s50" = {
      if (require("RSiena", quietly = TRUE)) {
        # Load network data (s501 is the first wave friendship network)
        data(s501, package = "RSiena")
        network_obj <- as.network(s501, directed = TRUE)
        
        # Load attribute data (s50a contains alcohol and smoking)
        data(s50a, package = "RSiena")
        # s50a is 50x3 matrix: [node_id, alcohol, smoking]
        # Add attributes to network
        set.vertex.attribute(network_obj, "alcohol", s50a[,2])
        set.vertex.attribute(network_obj, "smoking", s50a[,3])
        
        # Load additional attributes if available
        tryCatch({
          data(s50s, package = "RSiena")
          # s50s might contain additional lifestyle data
          if (ncol(s50s) >= 3) {
            set.vertex.attribute(network_obj, "drugs", s50s[,2])
            set.vertex.attribute(network_obj, "sport", s50s[,3])
          }
        }, error = function(e) {
          # If s50s fails, continue without additional attributes
        })
        metadata <- list(
          name = "Glasgow s50 Teenage Friends",
          description = "Teenage friendship and lifestyle network",
          nodes = network.size(network_obj),
          edges = network.edgecount(network_obj),
          directed = TRUE,
          attributes = c("alcohol", "smoke", "drugs", "sport", "music", "dance"),
          context = "Teenage friendship network with lifestyle behaviors",
          literature = c(
            "Steglich, Snijders & Pearson (2010). Annual Review of Sociology",
            "Ripley et al. (2022). RSiena: Statistical Analysis of Network Dynamics"
          )
        )
      } else {
        stop("RSiena package required for Glasgow s50 dataset. Install with: install.packages('RSiena')")
      }
    },
    
    # Manufacturing Emails (collapsed to a static directed network)
    "manufacturing_emails" = {
      if (require("networkDynamicData", quietly = TRUE)) {
        data(manufacturingEmails, package = "networkDynamicData")
        network_obj <- networkDynamic::network.collapse(manufacturingEmails, rule = "latest")
        metadata <- list(
          name = "Manufacturing Emails",
          description = "Internal email communication in a manufacturing company",
          nodes = network.size(network_obj),
          edges = network.edgecount(network_obj),
          directed = network::is.directed(network_obj),
          attributes = setdiff(list.vertex.attributes(network_obj), c("na", "vertex.names")),
          context = "Directed email communication network in a manufacturing company",
          literature = c(
            "Perer & Shneiderman (2006). IEEE Transactions on Visualization",
            "networkDynamicData package documentation"
          )
        )
      } else {
        stop("networkDynamicData package required for Manufacturing Emails dataset. Install with: install.packages('networkDynamicData')")
      }
    },
    
    # Enron Emails (collapsed to static)
    "enron_emails" = {
      if (require("networkDynamicData", quietly = TRUE)) {
        data(enronEmails, package = "networkDynamicData")
        # Collapse dynamic network to a static snapshot using the latest observed attributes
        network_obj <- networkDynamic::network.collapse(enronEmails, rule = "latest")
        # Get vertex attributes
        vertex_attrs <- list.vertex.attributes(network_obj)
        fix_categorical_na <- function(attr_name, replacement = "Missing") {
          if (!(attr_name %in% vertex_attrs)) return(NULL)
          vals <- network::get.vertex.attribute(network_obj, attr_name)
          if (is.null(vals) || !length(vals)) return(NULL)
          if (is.list(vals)) vals <- unlist(vals, use.names = FALSE)
          if (is.factor(vals)) vals <- as.character(vals)
          if (is.character(vals) && any(is.na(vals))) {
            vals[is.na(vals)] <- replacement
            network_obj <<- network::set.vertex.attribute(network_obj, attr_name, vals)
          }
          NULL
        }
        lapply(c("role", "dept", "person_name"), fix_categorical_na)
        vertex_attrs <- list.vertex.attributes(network_obj)
        metadata <- list(
          name = "Enron Emails",
          description = "Email communication network (collapsed to static)",
          nodes = network.size(network_obj),
          edges = network.edgecount(network_obj),
          directed = TRUE,
          attributes = vertex_attrs,  # Will include dept, role, etc.
          context = "Email communication network in Enron corporation",
          literature = c(
            "Klimt & Yang (2004). Enron Email Dataset",
            "Leskovec et al. (2007). ACM Transactions on Information Systems"
          )
        )
      } else {
        stop("networkDynamicData package required for Enron dataset. Install with: install.packages('networkDynamicData')")
      }
    },
    
    # Florentine Families
    "florentine" = {
      data(florentine, package = 'ergm')
      # Use marriage network as primary
      network_obj <- flomarriage
      metadata <- list(
        name = "Florentine Families",
        description = "Renaissance Florentine families marriage and business ties",
        nodes = network.size(network_obj),
        edges = network.edgecount(network_obj),
        directed = FALSE,
        attributes = c("wealth", "priorates"),
        context = "Classic historical social network of elite families",
        literature = c(
          "Padgett & Ansell (1993). American Journal of Sociology",
          "Breiger & Pattison (1986). Social Networks"
        )
      )
    },
    
    # Hospital SocioPatterns - COMMENTED OUT
    # "hospital" = {
    #   # Load hospital contact data from file
    #   if (file.exists("detailed_list_of_contacts_Hospital.dat_")) {
    #     hospital_contacts <- read.table("detailed_list_of_contacts_Hospital.dat_",
    #                                    header=FALSE, sep="\t",
    #                                    col.names=c("t","i","j","Si","Sj"))
    #     
    #     # Create network from contact data
    #     # Get unique nodes
    #     all_nodes <- unique(c(hospital_contacts$i, hospital_contacts$j))
    #     n_nodes <- length(all_nodes)
    #     
    #     # Create adjacency matrix
    #     adj_matrix <- matrix(0, nrow=n_nodes, ncol=n_nodes)
    #     rownames(adj_matrix) <- all_nodes
    #     colnames(adj_matrix) <- all_nodes
    #     
    #     # Fill adjacency matrix (undirected)
    #     for (k in 1:nrow(hospital_contacts)) {
    #       i_idx <- which(all_nodes == hospital_contacts$i[k])
    #       j_idx <- which(all_nodes == hospital_contacts$j[k])
    #       adj_matrix[i_idx, j_idx] <- 1
    #       adj_matrix[j_idx, i_idx] <- 1  # Undirected
    #     }
    #     
    #     # Create network object
    #     network_obj <- as.network(adj_matrix, directed = FALSE)
    #     
    #     # Add node attributes (status information)
    #     if ("Si" %in% names(hospital_contacts)) {
    #       # Get unique status for each node
    #       node_status <- aggregate(Si ~ i, data=hospital_contacts, FUN=function(x) names(sort(table(x), decreasing=TRUE))[1])
    #       status_map <- setNames(node_status$Si, node_status$i)
    #       
    #       # Add status attribute to network
    #       status_values <- sapply(all_nodes, function(x) ifelse(x %in% names(status_map), status_map[x], "unknown"))
    #       set.vertex.attribute(network_obj, "status", status_values)
    #     }
    #     
    #     # Get network attributes
    #     vertex_attrs <- list.vertex.attributes(network_obj)
    #     vertex_attrs <- vertex_attrs[!vertex_attrs %in% c("na", "vertex.names")]
    #     
    #     metadata <- list(
    #       name = "Hospital SocioPatterns",
    #       description = "Face-to-face contact network in hospital setting",
    #       nodes = network.size(network_obj),
    #       edges = network.edgecount(network_obj),
    #       directed = FALSE,
    #       attributes = vertex_attrs,  # Typically includes: status (patient, nurse, doctor, admin)
    #       context = "Healthcare contact network for disease transmission studies",
    #       literature = c(
    #         "Vanhems et al. (2013). PLoS ONE",
    #         "SocioPatterns collaboration dataset"
    #       )
    #     )
    #   } else {
    #     stop("Hospital contact data file 'detailed_list_of_contacts_Hospital.dat_' not found. Please ensure the file is in the current directory.")
    #   }
    # },
    
    # Noordin Top Terrorist Network
    "noordin_top" = {
      dataset <- load_noordin_top_dataset()
      network_obj <- dataset$network
      metadata <- dataset$metadata
    },
    
    # Backwards-compatible alias
    "terrorist" = {
      dataset <- load_noordin_top_dataset()
      network_obj <- dataset$network
      metadata <- dataset$metadata
    },
    
    # Caltech 36 Facebook Network
    "caltech_36" = {
      # Load Caltech data if not already loaded
      caltech_candidates <- c(
        file.path("data", "raw", "external", "caltech_36.mat")
      )
      caltech_file <- caltech_candidates[file.exists(caltech_candidates)][1]

      if (!exists("g") || !exists("m")) {
        if (!is.na(caltech_file)) {
          mat_data <- R.matlab::readMat(caltech_file)
          g <<- network::network(as.matrix(mat_data$A), directed = FALSE, matrix.type = "adjacency")
          m <<- mat_data
        } else if (file.exists("load_caltech_data.R")) {
          source("load_caltech_data.R")
        } else {
          stop("caltech_36.mat not found under ./data/raw/external/.")
        }
      }
      
      if (exists("g") && exists("m")) {
        network_obj <- g
        
        # Process attributes using your helper functions
        n <- network.size(network_obj)
        
        # Helper to coerce a MAT field into a length-n vector if present
        to_vec <- function(x) {
          if (is.null(x)) return(NULL)
          v <- as.vector(x)
          if (length(v) == n) return(v)
          if (length(v) == n*1) return(v)
          # Sometimes MATLAB stores as n x 1 or 1 x n; try transpose
          if (is.matrix(x) && prod(dim(x)) == n) return(as.vector(t(x)))
          return(NULL)
        }
        
        # Try separate vectors first
        gender     <- to_vec(m$gender)
        classyear  <- to_vec(m$year)
        major      <- to_vec(m$major)
        residence  <- to_vec(m$dorm)
        highschool <- to_vec(m$highschool)
        
        # If missing, try local_info matrix (common packing)
        if (any(sapply(list(gender,classyear,major,residence,highschool), is.null))) {
          li <- m$local_info
          if (!is.null(li) && is.matrix(li)) {
            # Typical column order documented in FB100:
            # 1 = status (ignore), 2 = gender, 3 = major, 4 = dorm/residence, 5 = year, 6 = high school
            if (is.null(gender)     && ncol(li) >= 2) gender     <- to_vec(li[,2, drop=FALSE])
            if (is.null(major)      && ncol(li) >= 3) major      <- to_vec(li[,3, drop=FALSE])
            if (is.null(residence)  && ncol(li) >= 4) residence  <- to_vec(li[,4, drop=FALSE])
            if (is.null(classyear)  && ncol(li) >= 5) classyear  <- to_vec(li[,5, drop=FALSE])
            if (is.null(highschool) && ncol(li) >= 6) highschool <- to_vec(li[,6, drop=FALSE])
          }
        }
        
        # Attach whatever we found
        attach_attr <- function(name, v) if(!is.null(v)) set.vertex.attribute(network_obj, name, v)
        attach_attr("gender",     gender)
        attach_attr("classyear",  classyear)
        attach_attr("major",      major)
        attach_attr("residence",  residence)
        attach_attr("highschool", highschool)
        
        # Recode 0 to NA (FB100 uses 0 for missing)
        for (nm in intersect(c("gender","classyear","major","residence","highschool"), list.vertex.attributes(network_obj))) {
          v <- get.vertex.attribute(network_obj, nm)
          v[!is.na(v) & v == 0] <- NA
          set.vertex.attribute(network_obj, nm, v)
        }
        
        # Make categorical for ERGM
        for (nm in c("gender","major","residence","highschool")) {
          if (nm %in% list.vertex.attributes(network_obj)) {
            set.vertex.attribute(network_obj, nm, as.factor(get.vertex.attribute(network_obj, nm)))
          }
        }
        
        # Get final attribute list
        final_attrs <- list.vertex.attributes(network_obj)
        final_attrs <- final_attrs[!final_attrs %in% c("na", "vertex.names")]
        
        metadata <- list(
          name = "Caltech 36 Facebook",
          description = "Facebook friendship network at Caltech (FB100 dataset)",
          nodes = network.size(network_obj),
          edges = network.edgecount(network_obj),
          directed = FALSE,
          attributes = final_attrs,
          context = "Facebook friendship network with demographic and academic attributes",
          literature = c(
            "Traud et al. (2011). Social Networks",
            "Traud et al. (2012). Physical Review E"
          )
        )
      } else {
        stop("Caltech 36 data not found. Please ensure 'g' (network) and 'm' (MAT file) objects are loaded.")
      }
    },
    
    # Default case
    {
      stop(sprintf("Unknown dataset: %s. Available datasets: faux_mesa, faux_dixon, faux_magnolia, kapferer, lazega, krackhardt, glasgow_s50, manufacturing_emails, enron_emails, florentine, noordin_top, caltech_36", dataset_name))
    }
  )
  
  # Print dataset summary
  cat(sprintf("✅ Loaded %s: %d nodes, %d edges, %s\n", 
              metadata$name, metadata$nodes, metadata$edges,
              ifelse(metadata$directed, "directed", "undirected")))
  cat(sprintf("   Attributes: %s\n", paste(metadata$attributes, collapse = ", ")))
  
  return(list(
    network = network_obj,
    metadata = metadata
  ))
}

# =============================================================================
# BENCHMARK DATASET INFORMATION
# =============================================================================

get_benchmark_info <- function() {
  # Return information about all available benchmark datasets
  
  datasets <- list(
    list(
      id = "faux_mesa",
      name = "Faux Mesa High",
      nodes = "205",
      edges = "203",
      directed = "No",
      attributes = "Grade, Race, Sex",
      context = "High school friendship",
      package = "ergm"
    ),
    list(
      id = "faux_dixon", 
      name = "Faux Dixon High",
      nodes = "248",
      edges = "519", 
      directed = "No",
      attributes = "Grade, Race, Sex",
      context = "High school friendship",
      package = "ergm"
    ),
    list(
      id = "faux_magnolia",
      name = "Faux Magnolia High", 
      nodes = "1461",
      edges = "974",
      directed = "No", 
      attributes = "Grade, Race, Sex",
      context = "High school friendship",
      package = "ergm"
    ),
    list(
      id = "kapferer",
      name = "Kapferer Tailor Shop",
      nodes = "39", 
      edges = "158",
      directed = "No",
      attributes = "Names",
      context = "Work interaction",
      package = "ergm"
    ),
    list(
      id = "lazega",
      name = "Lazega Lawyers",
      nodes = "71",
      edges = "575",
      directed = "Yes", 
      attributes = "Practice, Status, Gender, Office, Years, Age, Seniority",
      context = "Law firm multiplex",
      package = "ergm.multi"
    ),
    list(
      id = "noordin_top",
      name = "Noordin Top Terrorist Network",
      nodes = "79",
      edges = "200",
      directed = "No",
      attributes = "Status, Role, Group, Nation, Contact, Military, Education, Noordin, Org affiliations",
      context = "Terrorist communication",
      package = "Excel file"
    )
  )
  
  return(datasets)
}

# =============================================================================
# DATASET SUMMARY TABLE
# =============================================================================

print_benchmark_summary <- function() {
  # Print a formatted summary of all benchmark datasets
  
  datasets <- get_benchmark_info()
  
  cat("📊 BENCHMARK DATASETS SUMMARY\n")
  cat("=============================\n\n")
  
  cat(sprintf("%-15s %-20s %6s %6s %9s %-15s %-20s\n", 
              "ID", "Name", "Nodes", "Edges", "Directed", "Attributes", "Context"))
  cat(paste(rep("-", 90), collapse = ""), "\n")
  
  for (dataset in datasets) {
    cat(sprintf("%-15s %-20s %6s %6s %9s %-15s %-20s\n",
                dataset$id,
                dataset$name, 
                dataset$nodes,
                dataset$edges,
                dataset$directed,
                dataset$attributes,
                dataset$context))
  }
  
  cat("\n📚 LITERATURE REFERENCES:\n")
  cat("- Faux networks: Hunter, Handcock & Goodreau (2008). JSS\n")
  cat("- Kapferer: Kapferer (1972). Strategy and Transaction\n")
  cat("- Lazega: Lazega (2001). The Collegial Phenomenon\n")
  cat("- ERGM methods: Hunter & Handcock (2006). JASA\n\n")
}

# =============================================================================
# MULTI-DATASET EXPERIMENTAL FRAMEWORK
# =============================================================================

run_multi_dataset_experiment <- function(dataset_names = NULL, 
                                        methods = c("M4_OneShot", "M5_FewShot", "M6_Unconstrained", "M7_Iterative"),
                                        seeds = 1:5) {
  # Run LLM-ERGM experiments across multiple benchmark datasets
  # Args: dataset_names - Vector of dataset names to test (default: all)
  #       methods - Vector of methods to test (default: M4-M7)
  #       seeds - Vector of random seeds for reproducibility
  # Returns: List of results for each dataset-method combination
  
  if (is.null(dataset_names)) {
    dataset_names <- c("faux_mesa", "faux_dixon", "faux_magnolia", "kapferer", "lazega", 
                      "krackhardt", "glasgow_s50", "manufacturing_emails", "enron_emails", 
                      "florentine", "noordin_top", "caltech_36")
  }
  
  cat("🚀 MULTI-DATASET LLM-ERGM EXPERIMENT\n")
  cat("====================================\n")
  cat(sprintf("Datasets: %s\n", paste(dataset_names, collapse = ", ")))
  cat(sprintf("Methods: %s\n", paste(methods, collapse = ", ")))
  cat(sprintf("Seeds: %s\n", paste(seeds, collapse = ", ")))
  cat("\n")
  
  results <- list()
  
  for (dataset_name in dataset_names) {
    cat(sprintf("📊 Testing dataset: %s\n", dataset_name))
    
    # Load dataset
    dataset_result <- load_benchmark_dataset(dataset_name)
    network_obj <- dataset_result$network
    metadata <- dataset_result$metadata
    
    # Initialize results for this dataset
    results[[dataset_name]] <- list(
      metadata = metadata,
      methods = list()
    )
    
    # Test each method
    for (method in methods) {
      cat(sprintf("  🔄 Testing method: %s\n", method))
      
      method_results <- list()
      
      for (seed in seeds) {
        cat(sprintf("    Seed %d...", seed))
        
        tryCatch({
          # Call appropriate method based on method name
          if (method == "M4_OneShot") {
            result <- method_m4_oneshot(network_obj, seed = seed)
          } else if (method == "M5_FewShot") {
            result <- method_m5_fewshot(network_obj, seed = seed)
          } else if (method == "M6_Unconstrained") {
            result <- method_m6_unconstrained(network_obj, seed = seed)
          } else if (method == "M7_Iterative") {
            result <- method_m7_iterative_loop_enhanced(network_obj, seed = seed)
          }
          
          if (!is.null(result$fit)) {
            method_results[[as.character(seed)]] <- list(
              success = TRUE,
              aic = AIC(result$fit),
              bic = BIC(result$fit),
              converged = ergm_converged(result$fit),
              terms = result$terms,
              elapsed = if (!is.null(result$elapsed)) result$elapsed else NA
            )
            cat(" ✅\n")
          } else {
            method_results[[as.character(seed)]] <- list(
              success = FALSE,
              error = "Model fit failed"
            )
            cat(" ❌\n")
          }
          
        }, error = function(e) {
          method_results[[as.character(seed)]] <- list(
            success = FALSE,
            error = e$message
          )
          cat(" ❌\n")
        })
      }
      
      results[[dataset_name]]$methods[[method]] <- method_results
    }
    
    cat("\n")
  }
  
  return(results)
}

# =============================================================================
# RESULTS ANALYSIS
# =============================================================================

analyze_multi_dataset_results <- function(results) {
  # Analyze results from multi-dataset experiment
  
  cat("📈 MULTI-DATASET RESULTS ANALYSIS\n")
  cat("==================================\n\n")
  
  # Create summary table
  summary_data <- data.frame()
  
  for (dataset_name in names(results)) {
    dataset_results <- results[[dataset_name]]
    
    for (method_name in names(dataset_results$methods)) {
      method_results <- dataset_results$methods[[method_name]]
      
      # Extract successful runs
      successful_runs <- method_results[sapply(method_results, function(x) x$success)]
      
      if (length(successful_runs) > 0) {
        aic_values <- sapply(successful_runs, function(x) x$aic)
        bic_values <- sapply(successful_runs, function(x) x$bic)
        
        summary_data <- rbind(summary_data, data.frame(
          Dataset = dataset_name,
          Method = method_name,
          N_Success = length(successful_runs),
          N_Total = length(method_results),
          AIC_Mean = mean(aic_values),
          AIC_SD = sd(aic_values),
          BIC_Mean = mean(bic_values),
          BIC_SD = sd(bic_values),
          Success_Rate = length(successful_runs) / length(method_results)
        ))
      }
    }
  }
  
  # Print summary table
  if (nrow(summary_data) > 0) {
    cat("📊 PERFORMANCE SUMMARY\n")
    cat("======================\n")
    print(summary_data, digits = 2)
    
    # Best method per dataset
    cat("\n🏆 BEST METHOD PER DATASET\n")
    cat("===========================\n")
    
    for (dataset_name in unique(summary_data$Dataset)) {
      dataset_summary <- summary_data[summary_data$Dataset == dataset_name, ]
      best_method <- dataset_summary[which.min(dataset_summary$AIC_Mean), ]
      cat(sprintf("%s: %s (AIC: %.2f ± %.2f)\n", 
                  dataset_name, best_method$Method, 
                  best_method$AIC_Mean, best_method$AIC_SD))
    }
  } else {
    cat("❌ No successful runs found!\n")
  }
  
  return(summary_data)
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# Example 1: Load a single dataset
# dataset_result <- load_benchmark_dataset("faux_mesa")
# network_obj <- dataset_result$network

# Example 2: Print all available datasets
# print_benchmark_summary()

# Example 3: Run multi-dataset experiment
# results <- run_multi_dataset_experiment(
#   dataset_names = c("faux_mesa", "faux_dixon"),
#   methods = c("M4_OneShot", "M7_Iterative"),
#   seeds = 1:3
# )
# summary <- analyze_multi_dataset_results(results)

cat("📚 Benchmark datasets framework loaded successfully!\n")
cat("Available functions:\n")
cat("- load_benchmark_dataset(name) - Load specific dataset\n")
cat("- get_benchmark_info() - Get dataset information\n") 
cat("- print_benchmark_summary() - Print formatted summary\n")
cat("- run_multi_dataset_experiment() - Run experiments across datasets\n")
cat("- analyze_multi_dataset_results() - Analyze experiment results\n")
