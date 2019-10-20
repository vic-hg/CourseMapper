# Wei-Chi Victor Huang 27808319 19/10/2019
# Undegraduate Engineering Course Mapper
# pdfScrape.R

# Clear variables
rm(list = ls())

# Required libraries
library(pdftools)
library(tidyverse)

# Load unitNodes data
load("unitNodes.RData")

# Initialise matricies
synopsis <- matrix(nrow = nrow(nodes), ncol = 1)
competencies <- matrix(nrow = nrow(nodes), ncol = 1)
outcomes <- matrix(nrow = nrow(nodes), ncol = 1)
semester <- matrix(nrow = nrow(nodes), ncol = 1)
coords <- matrix(nrow = nrow(nodes), ncol = 1)

# Initialise semester tracking
isSemesterOne <- NULL

for (xrow in 1:nrow(nodes)) {
  continue <- T
  
  # Iterate through each of the units
  unit <- nodes[xrow,1]
  unit_pdf <- paste0(unit,"_Semester1(S1-01)_2019.pdf") # Try semester 1 first (this order can be changed)
  isSemesterOne <- T # Set tracking to sem 1
  
  # Error catching
  unit_text <- tryCatch(
    pdf_text(unit_pdf) %>%
      readr::read_lines(),
    error = function(e){NA}
  )
  
  # If pdf doesn't exist
  if (is.na(unit_text[1])) {
    continue <- F
    
    unit_pdf <- paste0(unit,"_Semester2(S2-01)_2019.pdf") # Try semester 2 second (this order can be changed)
    isSemesterOne <- F # Set tracking to sem 2
    
    # Error catching
    unit_text <- tryCatch(
      pdf_text(unit_pdf) %>%
        readr::read_lines(),
      error = function(e){NA}
    )
    
    if (is.na(unit_text[1])) {
      synopsis[xrow,] <- "Unit not found"
      competencies[xrow,] <- "Unit not found"
      outcomes[xrow,] <- "Unit not found"
    } else {
      continue <- T
    }
  }
  
  if (continue) {
    #________________________________________________________________________________________#
    # Record which semester unit data is from
    if (isSemesterOne) {
      semester[xrow,] <- 1
    } else {
      semester[xrow,] <- 2
    }
    
    #________________________________________________________________________________________#
    # Scrape synopsis data
    rowPrint <- F
    synopsis_data <- ""
    flag <- T
    
    for (row in 1:length(unit_text)) {
      # Define row without white space
      text_row <- sub("^\\s+", "", unit_text[row])
      
      if (text_row == "Mode of delivery" || text_row == "Location(s) and mode(s) of delivery"){
        rowPrint <- F
        flag <- F
      }
      
      if (rowPrint) {
        synopsis_data <- paste(synopsis_data, text_row)
      }
      
      if (text_row == "Synopsis" && flag) {
        rowPrint <- T
      }
    }
    
    # Remove leading whitespace
    synopsis_data <- sub("^\\s+", "", synopsis_data)
    synopsis[xrow,] <- synopsis_data
    
    #________________________________________________________________________________________#
    
    
    # Scrape EA competencies data
    rowPrint <- F
    complist <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "2.1", "2.2", "2.3", "2.4", "3.1", "3.2", "3.3", "3.4", "3.5", "3.6")
    competencies_data <- NULL
    
    for (row in 1:length(unit_text)) {
      # Define row without white space
      text_row <- sub("^\\s+", "", unit_text[row])
      
      if (text_row == "Teaching and learning method"){
        rowPrint <- F
      }
      
      if (rowPrint) {
        #element <- substr(text_row, 1, 3)
        check <- substr(text_row, 1, 2)
        
        if (check == 'PE') {
          element <- substr(text_row, 3,5)
        } else {
          element <- substr(text_row, 1,3)
        }
        
        
        # If element matches with one of the competencies in the list
        if (element %in% complist) {
          competencies_data <- c(competencies_data, element)
        }
      }
      
      if (text_row == "Academic Overview") {
        rowPrint <- T
      }
    }
    competencies_data <- paste(competencies_data,collapse=" ")
    
    # If no EA competencies found
    if (identical(competencies_data, "")  ) {
      competencies_data <- "No EA competencies found."
    }
    
    competencies[xrow,] <- competencies_data
    
    #_______________________________________________________________________________________#
    
    # Scrape Learning Outcomes
    
    rowPrint <- F
    clayton <- T # prevent malaysian unit guide from being read
    outcomes_data <- NULL
    
    for (row in 1:length(unit_text)) {
      # Define row without white space
      text_row <- sub("^\\s+", "", unit_text[row])
      
      if (text_row == "Your feedback to us" || text_row == "Teaching approach"){
        rowPrint <- F
        clayton <- F
      }
      
      if (rowPrint) {
        outcomes_data <- paste(outcomes_data, text_row) # This method will scrape page footers too
      }
      
      if (text_row == "Learning outcomes" && clayton) {
        rowPrint <- T
      }
    }
    
    outcomes[xrow,] <- outcomes_data
    
    #_______________________________________________________________________________________#
    # Scrape Unit Coordinators - note this is not standardised so may be hard to make modular in future
    
    rowPrint <- F
    coords_data <- NULL
    repeatCheck <- "Unit Coordinator(s)"
    
    for (row in 1:length(unit_text)) {
      # Define row without white space
      text_row <- sub("^\\s+", "", unit_text[row])
      
      if (substr(text_row, 1, 7) == "Clayton" || substr(text_row, 1, 8) == "Lecturer" || substr(text_row, 1, 6) == "Campus" || substr(text_row, 1, 8) == "Academic" || substr(text_row, 1, 5) == "Other"){
        rowPrint <- F
      }
      
      if (rowPrint) {
        # Ignore footers
        if ((substr(text_row, 1,4) == "Name" || substr(text_row, 1,5) == "Email") && text_row != repeatCheck) {
          coords_data <- paste(coords_data, text_row, "<br>")
          repeatCheck <- text_row
        } else if ((suppressWarnings(is.na(as.numeric(text_row))) && substr(text_row, 1,7) != unit && !grepl("^\\s*$", text_row)) && text_row != repeatCheck) {
          coords_data <- paste(coords_data, text_row, "<br>")
          repeatCheck <- text_row
        }
      }
      
      if (text_row == "Unit Coordinator(s)") {
        rowPrint <- T
      }
    }
    
    if (is.null(coords_data)) {
      coords_data <- "No unit coordinators found."
    }
    coords[xrow,] <- coords_data
    
  }
  print(paste0("scraping ", xrow," out of ", nrow(nodes)))
}

#_______________________________________________________________________________________#
# Append all the new columns

colnames(synopsis) <- "synopsis"
nodes <- cbind(nodes, synopsis)

colnames(competencies) <- "competencies"
nodes <- cbind(nodes, competencies)

colnames(outcomes) <- "outcomes"
nodes <- cbind(nodes, outcomes)

colnames(semester) <- "semester"
nodes <- cbind(nodes, semester)

colnames(coords) <- "coords"
nodes <- cbind(nodes, coords)

#_______________________________________________________________________________________#
# Remove Nodes and Edges of units that don't exist
nullUnits <- vector(mode = "character")
i <- 1

# Create a vector of all null units
for (row in 1:nrow(nodes)) {
  if (nodes[row, 11] == "Unit not found") {
    nullUnits[i] <- toString(nodes[row, 1])
    i <- i+1
  }
}

# Delete all the rows in nodes that have null units
for (row in 1:nrow(nodes)) {
  for (item in 1:length(nullUnits)) {
    if (nullUnits[item] == toString(nodes[row, 1])) {
      nodes <- nodes[-c(row),]
    }
  }
}

# Reorder rows
row.names(nodes) <- 1:nrow(nodes)

# Delete all the rows in edges that have null units
load("unitEdges.RData")

for (row in 1:nrow(edgesNew)) {
  for (item in 1:length(nullUnits)) {
    if (nullUnits[item] == toString(edgesNew[row, 1]) || nullUnits[item] == toString(edgesNew[row, 2])) {
      edgesNew <- edgesNew[-c(row),]
    }
  }
}

# Reorder rows
row.names(edgesNew) <- 1:nrow(edgesNew)
edges <- edgesNew

# Export files
save(nodes, file = "cleanedNodes.RData")
save(edges, file = "cleanedEdges.RData")
