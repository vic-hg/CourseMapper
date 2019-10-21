# Wei-Chi Victor Huang 27808319 19/10/2019
# Undegraduate Engineering Course Mapper
# csvToDataFrame.R

# Clear variables
rm(list = ls())

# Required Libraries
library(tidyverse)
library(stringr)
library(jsonlite)

# Read Units.csv file into unitData
unitData <- read.csv("Units.csv")

# Create dataframe of nodes assigned as the unit codes
nodes <- as.data.frame(unitData[1])
nodes[2] <- as.data.frame(unitData[1])
nodes[3] <- as.data.frame(unitData[2])
colnames(nodes) <- c("id","label","title")

# Remove repeated units
nodes <- distinct(nodes)


# Create empty dataframe to store prerequisite
prereqs <- data.frame()

# Extract prerequisite information
for (row in 1:nrow(unitData)) {
  if (unitData[row,9]=="Unit Pre-requisite (institution wide)" || unitData[row,9]=="Unit Pre-requisite") {
    prereq <- as.data.frame(unitData[row,10])
    prereq[2] <- as.data.frame(unitData[row,1])
    prereqs <- rbind(prereqs,prereq)
  }
}

# Extract only the prerequisite unit codes and saving to edges dataframe
edges <- data.frame("Type" = character(0), "Type" = character(),stringsAsFactors = FALSE)
for (row in 1:nrow(prereqs)) {
  edge = data.frame("Type" = character(30), stringsAsFactors = FALSE)
  edge <- rbind(edge,str_match_all(prereqs[row,1], "(?s)\\{(.*?)\\}")) # Obtains all the strings within curly brackets {}

  destNode = data.frame()
  
  for (i in 1:nrow(edge)) {
    destNode <- rbind(destNode, as.data.frame(prereqs[row,2]))
  }
  edge[2] <- destNode
  names(edge) <- names(edges) 
  
  edges <- rbind(edges,edge)
}

# Naming columns of 'edges'
colnames(edges) <- c("from","to")

# Delete blank rows
edges <- subset(edges, from!="")

# If name still contains { } get rid of the brackets
for (row in 1:nrow(edges)) {
  if (grepl("\\{",edges[row,1])) { 
    edges[row,1] <- gsub('^.|.$','',edges[row,1])
  }
}

# Second pass to extract only the unit codes
edgesNew <- data.frame("Type" = character(0), "Type" = character(),stringsAsFactors = FALSE)
for (row in 1:nrow(edges)) {
  edgeList <- unlist(strsplit(edges[row,1], ",")) # Elements seperated by commas are seperated into a list
  edgeList <- gsub(" ", "", edgeList, fixed = TRUE) # Remove whitespace
  for (i in 1:length(edgeList)) {
    if (nchar(edgeList[i]) == 7 && edgeList != "CLAYTON") { # If the element has 7 characters it is a unit
      newEdge <- cbind(edgeList[i], toString(edges[row,2]))
      edgesNew <- rbind(edgesNew, newEdge)
    }
  }
}

# Prevent double ups and reorder rows
edgesNew <- distinct(edgesNew)
row.names(edgesNew) <- 1:nrow(edgesNew)
names(edgesNew) <- names(edges) 

# Add edge nodes to nodes list (edge nodes are prerequisite units)
extraNodes <- data.frame("Type" = character(0),stringsAsFactors = FALSE)
extraNodes <- rbind(extraNodes,setdiff(edgesNew$from,nodes$id))
extraNodes <- as.data.frame(t(extraNodes))
extraNodes <- cbind(extraNodes,extraNodes)
extraNodes <- cbind(extraNodes,extraNodes[1])
colnames(extraNodes) <- c("id","label","title")
nodes <- rbind(nodes,extraNodes)
row.names(nodes) <- 1:nrow(nodes)

# Add groups to nodes according to year level
groups <- data.frame("Type" = character(1),stringsAsFactors = FALSE)
for (row in 1:nrow(nodes)) {
  if(substring(nodes[row,1],4,4)=="0") {
    groups <- rbind(groups, "0th Year")
  } else if (substring(nodes[row,1],4,4)=="1") {
    groups <- rbind(groups, "1st Year")
  } else if (substring(nodes[row,1],4,4)=="2") {
    groups <- rbind(groups, "2nd Year")
  } else if (substring(nodes[row,1],4,4)=="3") {
    groups <- rbind(groups, "3rd Year")
  } else if (substring(nodes[row,1],4,4)=="4") {
    groups <- rbind(groups, "4th Year")
  } else if (substring(nodes[row,1],4,4)=="5") {
    groups <- rbind(groups, "5th Year")
  } else if (substring(nodes[row,1],4,4)=="6") {
    groups <- rbind(groups, "6th Year")
  }
}

# Delete blank rows and reorder rows
colnames(groups) <- "group"
groups <- subset(groups, group!="")
row.names(groups) <- 1:nrow(groups)

# Add groups to nodes
nodes <- cbind(nodes,groups)

# Add level to nodes
level <- data.frame("Type" = character(1),stringsAsFactors = FALSE)
for (row in 1:nrow(nodes)) {
  if(substring(nodes[row,1],4,4)=="0") {
    level <- rbind(level, "0")
  } else if (substring(nodes[row,1],4,4)=="1") {
    level <- rbind(level, "1")
  } else if (substring(nodes[row,1],4,4)=="2") {
    level <- rbind(level, "2")
  } else if (substring(nodes[row,1],4,4)=="3") {
    level <- rbind(level, "3")
  } else if (substring(nodes[row,1],4,4)=="4") {
    level <- rbind(level, "4")
  } else if (substring(nodes[row,1],4,4)=="5") {
    level <- rbind(level, "5")
  } else if (substring(nodes[row,1],4,4)=="6") {
    level <- rbind(level, "6")
  }
}

# Delete blank rows and reorder rows
colnames(level) <- "level"
level <- subset(level, level!="")
row.names(level) <- 1:nrow(level)

# Add groups to nodes
nodes <- cbind(nodes,level)

# Add enrolment information for last 4 semesters
enrolmentCurYearS1 <- data.frame("Type" = character(1),stringsAsFactors = FALSE)
enrolmentCurYearS2 <- data.frame("Type" = character(1),stringsAsFactors = FALSE)
enrolmentLastYearS1 <- data.frame("Type" = character(1),stringsAsFactors = FALSE)
enrolmentLastYearS2 <- data.frame("Type" = character(1),stringsAsFactors = FALSE)

enrolmentCountCurYearS1 <- 0
enrolmentCountCurYearS2 <- 0
enrolmentCountLastYearS1 <- 0
enrolmentCountLastYearS2 <- 0

flag_one <- T
flag_two <- T
flag_three <- T
flag_four <- T

print("Scraping Enrolment Information...")

for (row in 1:nrow(nodes)) {
  for (i in 1:nrow(unitData)) {
    if (nodes[row,1] == unitData[i,1]) {
      # If current year and sem 1
      if (unitData[i,3] == "S1-01" && unitData[i,6] == format(Sys.Date(), "%Y") && flag_one) {
        enrolmentCountCurYearS1 <- enrolmentCountCurYearS1 + unitData[i,7]
        flag_one <- F
      # If current year and sem 2
      } else if (unitData[i,3] == "S2-01" && unitData[i,6] == format(Sys.Date(), "%Y") && flag_two) {
        enrolmentCountCurYearS2 <- enrolmentCountCurYearS2 + unitData[i,7]
        flag_two <- F
      # If last year and sem 1
      } else if (unitData[i,3] == "S1-01" && unitData[i,6] == as.numeric(format(Sys.Date(), "%Y"))-1 && flag_three) {
        enrolmentCountLastYearS1 <- enrolmentCountLastYearS1 + unitData[i,7]
        flag_three <- F
      # If last year and sem 2
      } else if (unitData[i,3] == "S2-01" && unitData[i,6] == as.numeric(format(Sys.Date(), "%Y"))-1 && flag_four) {
        enrolmentCountLastYearS2 <- enrolmentCountLastYearS2 + unitData[i,7]
        flag_four <- F
      }
    }
  }
  
  if(enrolmentCountCurYearS1 != 0){
    enrolmentCurYearS1 <- rbind(enrolmentCurYearS1, as.character(enrolmentCountCurYearS1))
  } else {
    enrolmentCurYearS1 <- rbind(enrolmentCurYearS1, "No Data")
  }
  
  if(enrolmentCountCurYearS2 != 0){
    enrolmentCurYearS2 <- rbind(enrolmentCurYearS2, as.character(enrolmentCountCurYearS2))
  } else {
    enrolmentCurYearS2 <- rbind(enrolmentCurYearS2, "No Data")
  }
  
  if(enrolmentCountLastYearS1 != 0){
    enrolmentLastYearS1 <- rbind(enrolmentLastYearS1, as.character(enrolmentCountLastYearS1))
  } else {
    enrolmentLastYearS1 <- rbind(enrolmentLastYearS1, "No Data")
  }
  
  if(enrolmentCountLastYearS2 != 0){
    enrolmentLastYearS2 <- rbind(enrolmentLastYearS2, as.character(enrolmentCountLastYearS2))
  } else {
    enrolmentLastYearS2 <- rbind(enrolmentLastYearS2, "No Data")
  }
  
  enrolmentCountCurYearS1 <- 0
  enrolmentCountCurYearS2 <- 0
  enrolmentCountLastYearS1 <- 0
  enrolmentCountLastYearS2 <- 0

  flag_one <- T
  flag_two <- T
  flag_three <- T
  flag_four <- T
}

colnames(enrolmentCurYearS1) <- "enrolmentCurYearS1"
enrolmentCurYearS1 <- subset(enrolmentCurYearS1, enrolmentCurYearS1!="")
nodes <- cbind(nodes,enrolmentCurYearS1)

colnames(enrolmentCurYearS2) <- "enrolmentCurYearS2"
enrolmentCurYearS2 <- subset(enrolmentCurYearS2, enrolmentCurYearS2!="")
nodes <- cbind(nodes,enrolmentCurYearS2)

colnames(enrolmentLastYearS1) <- "enrolmentLastYearS1"
enrolmentLastYearS1 <- subset(enrolmentLastYearS1, enrolmentLastYearS1!="")
nodes <- cbind(nodes,enrolmentLastYearS1)

colnames(enrolmentLastYearS2) <- "enrolmentLastYearS2"
enrolmentLastYearS2 <- subset(enrolmentLastYearS2, enrolmentLastYearS2!="")
nodes <- cbind(nodes,enrolmentLastYearS2)

# Add departments to nodes
department <- data.frame("Type" = character(1),stringsAsFactors = FALSE)
unitDepartment <- 0
flag = T

print("Scraping Department Information...")

for (row in 1:nrow(nodes)) {
  for (i in 1:nrow(unitData)) {
    if (nodes[row,1] == unitData[i,1] && flag) {
      unitDepartment <- unitData[i,8]
      flag <- F
    }
  }
  department <- rbind(department, as.character(unitDepartment))
  flag <- T
}

colnames(department) <- "department"
department <- subset(department, department!="")
nodes <- cbind(nodes, department)

# Save nodes and edges as RData file to be used in pdfScrape code
save(nodes, file = "unitNodes.RData")
save(edgesNew, file = "unitEdges.RData")

# Save nodes[,1] as JSON to be use in HTML unit guide download code
nodeJSON <- toJSON(nodes[,1])
write(nodeJSON, "nodeJSON.json")