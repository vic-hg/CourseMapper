# Wei-Chi Victor Huang 27808319 19/10/2019
# Undegraduate Engineering Course Mapper
# app.R

# Required Libraries
library(shiny)
library(shinydashboard)
library(shinyalert)
library(visNetwork)
library(shinyWidgets)

# Define shinyalert modal
unitInfo <- function(unit, title, enrolmentCurYearS1, enrolmentCurYearS2, enrolmentLastYearS1, enrolmentLastYearS2, department, synopsis, competencies, semester, coords) {
    shinyalert(
        paste(unit, title, sep = ": "),
        paste0(h4(department),
               h3("Synopsis"), synopsis, br(),
               h3("EA competencies"), if (competencies != "No EA competencies found.") gsub("[[:space:]]", ", ", competencies) else competencies, br(),
               h3("Unit Coordinator(s)"), coords,
               h3("Enrolments: "),
               format(Sys.Date(), "%Y")," S2: ", enrolmentCurYearS2, br(),
               format(Sys.Date(), "%Y")," S1: ", enrolmentCurYearS1, br(),
               as.numeric(format(Sys.Date(), "%Y"))-1," S2: ", enrolmentLastYearS2, br(), 
               as.numeric(format(Sys.Date(), "%Y"))-1," S1: ", enrolmentLastYearS1, br(), br(), 
               a("Open Unit Guide", href=paste0("https://unitguidemanager.monash.edu/view?unitCode=",unit,"&tpCode=S",semester,"-01&tpYear=2019"), target="_blank"), "&emsp;&emsp;&emsp;",
               a("Open Handbook", href=paste0("http://www.monash.edu/pubs/",format(Sys.Date(), "%Y"),"handbooks/units/",unit,".html"), target="_blank")
        ),
        html = T,
        confirmButtonText = "Close Window",
        confirmButtonCol = "#0067b1",
        closeOnClickOutside = T,
        closeOnEsc = T,
        className = "unitInfoPopup"
    )
}

# Define UI for application that draws a network graph
ui <- dashboardPage(
    title = "Course Mapper",
    dashboardHeader(
        titleWidth = 400,
        title = h4(HTML("Undergraduate Engineering<br/>Course Mapper"))
    ),
    dashboardSidebar(
        tags$head( tags$script(type="text/javascript",'$(document).ready(function(){
                             $(".main-sidebar").css("height","100%");
                             $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow-y": "auto"})
                             $(".main-sidebar .col-sm-12").css({"position":"relative", "max-width": "100%", "white-space": "initial"})
                             })')),
        width = 400,
        selectInput("selectedDep", label = "Select Department", choices = list("All Departments", "Chemical Engineering", "Civil Engineering", "Electrical & Computer Systems Engineering", "Materials Science & Engineering", "Mechanical & Aerospace Engineering"), width = '100%'),
        searchInput("unitText", label = "Search Unit", placeholder = "e.g. ENG1001", btnSearch = icon("search"), width = 400),
        searchInput("synopsisText", label = "Search Synopsis and Learning Outcomes", placeholder = "e.g. Signal", btnSearch = icon("search"), width = 400),
        searchInput("compText", label = "Search EA Competencies", placeholder = "e.g. 1.1, 1.2, 1.3", btnSearch = icon("search"), width = 400),
        mainPanel(p()),
        actionButton(inputId = "resetButton", label = "Reset/Center Graph"),
        sidebarMenu(
          menuItem("EA Competency Information", menuSubItem(
            mainPanel(
              h2("EA Competency Information"),
              h3("Stage 1 Competencies"),
              h4("1. KNOWLEDGE AND SKILL BASE"),
              p("1.1. Comprehensive, theory based understanding of the underpinning natural and physical sciences and the engineering fundamentals applicable to the engineering discipline."),
              p("1.2. Conceptual understanding of the mathematics, numerical analysis, statistics, and computer and information sciences which underpin the engineering discipline."),
              p("1.3. In-depth understanding of specialist bodies of knowledge within the engineering discipline."),
              p("1.4. Discernment of knowledge development and research directions within the engineering discipline."),
              p("1.5. Knowledge of engineering design practice and contextual factors impacting the engineering discipline."),
              p("1.6. Understanding of the scope, principles, norms, accountabilities and bounds of sustainable engineering practice in the specific discipline."),
              h4("2. ENGINEERING APPLICATION ABILITY"),
              p("2.1. Application of established engineering methods to complex engineering problem solving."),
              p("2.2. Fluent application of engineering techniques, tools and resources."),
              p("2.3. Application of systematic engineering synthesis and design processes."),
              p("2.4. Application of systematic approaches to the conduct and management of engineering projects."),
              h4("3. PROFESSIONAL AND PERSONAL ATTRIBUTES"),
              p("3.1. Ethical conduct and professional accountability."),
              p("3.2. Effective oral and written communication in professional and lay domains."),
              p("3.3. Creative, innovative and pro-active demeanour."),
              p("3.4. Professional use and management of information."),
              p("3.5. Orderly management of self, and professional conduct."),
              p("3.6. Effective team membership and team leadership."),
            width = 12
            )
          ))
        )
    ),
    dashboardBody(
        # Make modal scrollable
        tags$style(
          type = "text/css",
          ".unitInfoPopup {
            max-height: 80vh !important;
            overflow-y: auto !important;
          }"
        ),
        
        # Make network graph height adaptive
        tags$style(type = "text/css", "#network {height: calc(100vh - 150px) !important;}"),
      
        # This JS script forces modals to scroll up
        tags$script(
        '
        Shiny.addCustomMessageHandler("scrollCallback",
        function(color) {
            var objDiv = document.getElementsByClassName("unitInfoPopup");
            var modalDiv = objDiv[0];
            modalDiv.scrollTop = 0;
          }
        );'
        ),
            
        box(
            # Network Graph
            width = 12, # Make graph span across page width
            visNetworkOutput("network", width = "100%")
        ),
        useShinyalert()
    )
)

# Define server logic required to draw a network graph
server <- function(input, output, session) {
    
    load("cleanedNodes.RData")
    load("cleanedEdges.RData")
    
    output$network <- renderVisNetwork({
        visNetwork(nodes, edges) %>%
            visNodes(font = list(color= '#000000', size = 40, face = 'arial', strokeWidth = 3, strokeColor = '#000000'), shape = "ellipse") %>%
            visEdges(arrows = list(to = list(enabled = T, scaleFactor = 2)), width = 3, smooth = list(enabled = T)) %>%
            visOptions(highlightNearest = list(enabled = T, hover = T, labelOnly = F, algorithm = "hierarchical", degree = 2)) %>%
            visHierarchicalLayout(direction = "LR", levelSeparation = 1000) %>%
            visPhysics(enabled = T, stabilization = F, timestep = 0.5, minVelocity = 20, maxVelocity = 500, hierarchicalRepulsion = list(nodeDistance = 300, centralGravity = 40, springLength = 300, springConstant = 0.005, damping = 0.15)) %>%
            visInteraction(dragNodes = F, dragView = T, zoomView = T) %>%
            visEvents(
                selectNode = 'function(properties) {
                          Shiny.setInputValue("selectedUnit", this.body.data.nodes.get(properties.nodes[0]).id);
                          Shiny.setInputValue("selectedTitle", this.body.data.nodes.get(properties.nodes[0]).title);
                          Shiny.setInputValue("selectedSem", this.body.data.nodes.get(properties.nodes[0]).semester);
                          Shiny.setInputValue("selectedComp", this.body.data.nodes.get(properties.nodes[0]).competencies);
                          Shiny.setInputValue("selectedCoords", this.body.data.nodes.get(properties.nodes[0]).coords);
                          Shiny.setInputValue("selectedEnrolCurYearS1", this.body.data.nodes.get(properties.nodes[0]).enrolmentCurYearS1);
                          Shiny.setInputValue("selectedEnrolCurYearS2", this.body.data.nodes.get(properties.nodes[0]).enrolmentCurYearS2);
                          Shiny.setInputValue("selectedEnrolLastYearS1", this.body.data.nodes.get(properties.nodes[0]).enrolmentLastYearS1);
                          Shiny.setInputValue("selectedEnrolLastYearS2", this.body.data.nodes.get(properties.nodes[0]).enrolmentLastYearS2);
                          Shiny.setInputValue("selectedDepartment", this.body.data.nodes.get(properties.nodes[0]).department);
                          Shiny.setInputValue("selectedSynopsis", this.body.data.nodes.get(properties.nodes[0]).synopsis, {priority: "event"});
                        }'
            )
    })
    
    # Sort by departments
    observeEvent(
      input$selectedDep,
      {print(input$selectedDep)
        
      # reset graph
      visNetworkProxy("network") %>%
        visUpdateNodes(nodes)
        
      unitList <- character()
      
      if(!grepl("All Departments", input$selectedDep, ignore.case = T)) {
        for (row in 1:nrow(nodes)) {
          # Searching in synopsis
          if (grepl(input$selectedDep, nodes[row,10], ignore.case = T)) {
            unitList <- c(unitList, as.character(nodes[row,2]))
          }
          if (grepl("Faculty of Engineering", nodes[row,10], ignore.case = T) ){
            unitList <- c(unitList, as.character(nodes[row,2]))
          }
          if (grepl("Engineering Office of the Dean", nodes[row,10], ignore.case = T) ){
            unitList <- c(unitList, as.character(nodes[row,2]))
          }
        }
        for (row in 1:nrow(nodes)) {
          if (!(nodes[row,1] %in% unitList)) {
            visNetworkProxy("network") %>%
              visRemoveNodes(id = as.character(nodes[row,1]))
          }
        }
        visNetworkProxy("network") %>%
          visFit(nodes = unitList, animation = list(duration = 500, easingFunction = "linear"))
        updateSearchInput(session = session, inputId = "synopsisText", value = "", trigger = T)
      } else {
        visNetworkProxy("network") %>%
          visUpdateNodes(nodes) %>%
          visFit(nodes = NULL, animation = list(duration = 500, easingFunction = "linear"))
      }
      },
      ignoreInit = T
    )
    
    # Unit Search
    observe({
        if (input$unitText > 0) {
            isolate({
                print(input$unitText)
                if (toupper(input$unitText) %in% nodes$id ) {
                    visNetworkProxy("network") %>%
                        visFocus(id = toupper(input$unitText), scale = 0.5, animation = list(duration = 500)) %>%
                        visSelectNodes(id = toupper(input$unitText))
                    x <- which(nodes == toupper(input$unitText)) # Dataframe location of matched unit
                    # unitInfo(unit[2], title[3], enrolmentCurYearS1[6], enrolmentCurYearS2[7], enrolmentLastYearS1[8], enrolmentLastYearS2[9], department[10], synopsis[11], competencies[12], semester[14], coords[15])
                    unitInfo(nodes[x[1], 2], nodes[x[1], 3], nodes[x[1], 6], nodes[x[1], 7], nodes[x[1], 8], nodes[x[1], 9], nodes[x[1], 10], nodes[x[1], 11], nodes[x[1], 12], nodes[x[1], 14], nodes[x[1], 15])
                    # Call script that forces modals to scroll up
                    session$sendCustomMessage(type = "scrollCallback", 1)
                } else {
                    shinyalert("Oops!","Unit not Found.", type = "error")
                }
                updateSearchInput(session = session, inputId = "unitText", value = "", trigger = T)
            })
        }
    })
    
    # Synopsis Search
    observe({
        if (input$synopsisText > 0) {
            isolate({
                print(input$synopsisText)
                unitList <- character()
                for (row in 1:nrow(nodes)) {
                    # Searching in synopsis
                    if (grepl(input$synopsisText, nodes[row,11], ignore.case = T)) {
                        unitList <- c(unitList, as.character(nodes[row,2]))
                    }
                    # Searching in Learning Outcomes
                    if (grepl(input$synopsisText, nodes[row,13], ignore.case = T)) {
                        unitList <- c(unitList, as.character(nodes[row,2]))
                    }
                }
                for (row in 1:nrow(nodes)) {
                    if (!(nodes[row,1] %in% unitList)) {
                        visNetworkProxy("network") %>%
                            visRemoveNodes(id = as.character(nodes[row,1]))
                    }
                }
                visNetworkProxy("network") %>%
                    visFit(nodes = unitList, animation = list(duration = 500, easingFunction = "linear"))
                updateSearchInput(session = session, inputId = "synopsisText", value = "", trigger = T)
                shinyalert(text = "Use Reset Graph button to reset the graph to initial conditions.", type = "info")
            })
        }
    })
  
    
    # EA Competencies Search 
    observe({
        if (input$compText > 0) {
            isolate({
                #print(input$compText)
                unitList <- character()
                
                # Changing EA input into list
                compList <- as.list(strsplit(input$compText,","))[[1]]
                compList <- gsub("[[:space:]]", "", compList)
                print(compList)
                
                # Searching in EA competencies
                for (row in 1:nrow(nodes)) {
                    totalContained <- T
                    unitCompList <- as.list(strsplit(toString(nodes[row,9]), " ")[[1]])
                    
                    for (comp in 1:length(compList)) {
                        contained <- F
                        for (unitComp in 1:length(unitCompList)) {
                            if (compList[comp] == unitCompList[unitComp]) {
                                contained <- T # If any of the searched units are found then contained wil be set to T
                            } else {
                                contained <- contained || contained # If none of the serached units are found then contained will remain F, causing next line to discard unit
                            }
                        }
                        totalContained <- totalContained && contained # If contained == F, then totalContained will == F, discarding the unit
                    }
                    
                    if (totalContained) {
                        unitList <- c(unitList, as.character(nodes[row,2]))
                    }
                    
                }
                
                # Remove irrelevant nodes
                for (row in 1:nrow(nodes)) {
                    if (!(nodes[row,1] %in% unitList)) {
                        visNetworkProxy("network") %>%
                            visRemoveNodes(id = as.character(nodes[row,1]))
                    }
                }
                visNetworkProxy("network") %>%
                    visFit(nodes = unitList, animation = list(duration = 500, easingFunction = "linear"))
                updateSearchInput(session = session, inputId = "compText", value = "", trigger = T)
                shinyalert(text = "Use Reset Graph button to reset the graph to initial conditions.", type = "info")
            })
        }
    })
    
    
    # When a node is selected
    observeEvent(input$selectedSynopsis, {
        unitInfo(input$selectedUnit, input$selectedTitle, input$selectedEnrolCurYearS1, input$selectedEnrolCurYearS2, input$selectedEnrolLastYearS1, input$selectedEnrolLastYearS2, input$selectedDepartment, input$selectedSynopsis, input$selectedComp, input$selectedSem, input$selectedCoords)
        # Call script that forces modals to scroll up
        session$sendCustomMessage(type = "scrollCallback", 1)
    })
    
    # Reset the graph
    observe({
        if (input$resetButton > 0) {
            isolate({
                visNetworkProxy("network") %>%
                    visUpdateNodes(nodes) %>%
                    visFit(nodes = NULL, animation = list(duration = 500, easingFunction = "linear"))
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
