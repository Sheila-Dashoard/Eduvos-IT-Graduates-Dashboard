# Load libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(stringr)
library(plotly)
library(bslib)
library(ggthemes)
# Load data
graduate_data <- readRDS("C:/Users/sheil/Desktop/ITRDA ASSIGMENT/graduate_data.rds")


# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"),
  
  # Navigation Bar
  navbarPage(
    title = "Eduvos Graduate Dashboard", 
    
    # Overview Tab
    tabPanel("Overview", 
             fluidRow(
               column(width = 5,  
                      tabsetPanel(
                        tabPanel("Top 5 Campuses",
                                 fluidRow(
                                   column(width = 12,  # Info Box
                                          box(title = "", width = NULL, solidHeader = TRUE, status = "info",
                                              uiOutput("top_campuses_box")  # Info Box
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Top 5 Education Levels",
                                 fluidRow(
                                   column(width = 12,  # Info Box
                                          box(title = "", width = NULL, solidHeader = TRUE, status = "success",
                                              uiOutput("top_edu_level_box")  # Info Box
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Top 5 Industries",
                                 fluidRow(
                                   column(width = 12,  # Info Box
                                          box(title = "", width = NULL, solidHeader = TRUE, status = "danger",
                                              uiOutput("popular_industries_box")  # Info Box
                                          )
                                   )
                                 )
                        )
                      )
               ),
               column(width = 7,  
                      box(title = "Quick Facts", width = NULL, solidHeader = TRUE, status = "primary",
                          fluidRow(
                            column(width = 6,  
                                   valueBox(value = nrow(graduate_data), subtitle = "Total Observations", icon = icon("database"), color = "blue")
                            ),
                            column(width = 6,  
                                   valueBox(value = length(unique(graduate_data$StudyField)), subtitle = "Total Unique Study Fields", icon = icon("book"), color = "purple")
                            )
                          ),
                          fluidRow(
                            column(width = 6, 
                                   valueBox(value = ncol(graduate_data) - 2, subtitle = "Total Variables", icon = icon("cogs"), color = "green")
                            ),
                            column(width = 6,  
                                   valueBox(value = length(unique(graduate_data$Industry)), subtitle = "Total Unique Industries", icon = icon("briefcase"), color = "red")
                            )
                          ),
                          actionButton("toggle_description", "Variable Descriptions+")
                      )
               )
             ),
             fluidRow(
               # Title for Key Trends
               column(width = 12, 
                      h1("Key Trends")  # Title for Key Trends
               ),
               box(title = "Top Programming Languages", width = 4, solidHeader = TRUE, status = "primary",
                   plotOutput("language_plot", height = "250px")
               ),
               box(title = "Top Databases", width = 4, solidHeader = TRUE, status = "primary",
                   plotOutput("database_plot", height = "250px")
               ),
               box(title = "Top Platforms", width = 4, solidHeader = TRUE, status = "primary",
                   plotOutput("platform_plot", height = "250px")
               )
             ),
             fluidRow(
               box(title = "Top Web Frameworks", width = 4, solidHeader = TRUE, status = "primary",
                   plotOutput("webframework_plot", height = "250px")
               ),
               box(title = "Top AI Search Tools", width = 4, solidHeader = TRUE, status = "primary",
                   plotOutput("aisearch_plot", height = "250px")
               ),
               box(title = "Top AI Tools", width = 4, solidHeader = TRUE, status = "primary",
                   plotOutput("aitool_plot", height = "250px")
               )
             ),
             fluidRow(
               box(title = "Dataset Preview", width = 12, solidHeader = TRUE, status = "primary", 
                   DTOutput("data_preview"),
                   downloadButton("download_data", "Download Data")
               )
             )
    ),
    # Analytics and Visualizations Tab
    tabPanel("Analytics and Visualizations", 
             tabsetPanel(
               # Tab 1: Interactive Visualizations for Tools
               tabPanel("Focused Charts", fluidRow(
                 
                 # Dropdown to choose study field or industry
                 box(title = "Select Filters", width = 4, solidHeader = TRUE, status = "primary",
                     selectInput("study_field", "Select Study Field:", choices = c("All", unique(graduate_data$StudyField))),
                     selectInput("industry", "Select Industry:", choices = c("All", unique(graduate_data$Industry))),
                     selectInput("graph_type", "Select Graph Type:", choices = c("Bar Plot", "Dot Plot"))
                 ),
                 
                 # Dropdown to choose which 
                 box(title = "Select Tool", width = 4, solidHeader = TRUE, status = "primary",
                     selectInput("tool_type", "Select Tool for Visualization:", 
                                 choices = c("Programming Languages", "Databases", "Platforms", 
                                             "Web Frameworks", "AI Search Tools", "AI Tools"))
                 ),
                 
                 # Output graph
                 box(title = "Visualization", width = 12, solidHeader = TRUE, status = "primary",
                     plotOutput("dynamic_tool_plot", height = "400px")
                 )
               ),
               
               #  Data and Counts for Tools
               # s
               fluidRow(
                 box(title = "Count Table for Selected Tool", width = 12, solidHeader = TRUE, status = "primary",
                     DTOutput("dynamic_count_table")  # Add this DTOutput to display the table
                 )
               )),# Comparisons Tab with Subtabs
               tabPanel("Comparisons",
                        tabsetPanel(
                          # Subtab  Study Fields
                          tabPanel("By Study Fields",
                                   fluidRow(
                                     column(width = 4,
                                            box(title = "Select Study Fields", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_study_fields", "Select Study Fields:", choices = unique(graduate_data$StudyField), multiple = TRUE)
                                            ),
                                            box(title = "Select Tool", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_tool_sf", "Select Tool for Comparison:", 
                                                            choices = c("Programming Languages", "Databases", "Platforms", 
                                                                        "Web Frameworks", "AI Search Tools", "AI Tools"))
                                            )
                                     ),
                                     column(width = 8,
                                            box(title = "Comparison Visualization:Top Tools ", width = NULL, solidHeader = TRUE, status = "primary",
                                                plotOutput("comparison_plot_sf", height = "400px")
                                            )
                                     )
                                   )
                          ),
                          # Subtab  Industries
                          tabPanel("By Industries",
                                   fluidRow(
                                     column(width = 4,
                                            box(title = "Select Industries", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_industries", "Select Industries:", choices = unique(graduate_data$Industry), multiple = TRUE)
                                            ),
                                            box(title = "Select Tool", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_tool_ind", "Select Tool for Comparison:", 
                                                            choices = c("Programming Languages", "Databases", "Platforms", 
                                                                        "Web Frameworks", "AI Search Tools", "AI Tools"))
                                            )
                                     ),
                                     column(width = 8,
                                            box(title = "Comparison Visualization:Top Tools ", width = NULL, solidHeader = TRUE, status = "primary",
                                                plotOutput("comparison_plot_ind", height = "400px")
                                            )
                                     )
                                   )
                          ),
                          # Subtab Campuses
                          tabPanel("By Campuses",
                                   fluidRow(
                                     column(width = 4,
                                            box(title = "Select Campuses", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_campuses_cmp", "Select Campuses:", choices = unique(graduate_data$Campus), multiple = TRUE)
                                            ),
                                            box(title = "Select Tool", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_tool_cmp", "Select Tool for Comparison:", 
                                                            choices = c("Programming Languages", "Databases", "Platforms", 
                                                                        "Web Frameworks", "AI Search Tools", "AI Tools"))
                                            )
                                     ),
                                     column(width = 8,
                                            box(title = "Comparison Visualization:Top Tools ", width = NULL, solidHeader = TRUE, status = "primary",
                                                plotOutput("comparison_plot_cmp", height = "400px")
                                            )
                                     )
                                   )
                          ),#Subtab Roles
                          tabPanel("By Roles",
                                   fluidRow(
                                     column(width = 4,
                                            box(title = "Select Roles", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_roles", "Select Roles:", choices = unique(graduate_data$RoleCategory), multiple = TRUE)
                                            ),
                                            box(title = "Select Tool", width = NULL, solidHeader = TRUE, status = "primary",
                                                selectInput("compare_tool_role", "Select Tool for Comparison:", 
                                                            choices = c("Programming Languages", "Databases", "Platforms", 
                                                                        "Web Frameworks", "AI Search Tools", "AI Tools"))
                                            )
                                     ),
                                     column(width = 8,
                                            box(title = "Comparison Visualization:Top Tools ", width = NULL, solidHeader = TRUE, status = "primary",
                                                plotOutput("comparison_plot_role", height = "400px")
                                            )
                                     )
                                   )
                          )
                        )
               ),
               
             ),
             
    ),# User Guide Tab
    tabPanel("User  Guide",
             fluidRow(
               box(title = "User  Guide", width = 12, solidHeader = TRUE, status = "info",
                   p("Welcome to the Eduvos Graduate Dashboard! This dashboard provides insights into the graduate data."),
                   p("Here's how to use the dashboard:"),
                   tags$ul(
                     tags$li("Use the filters in the 'Analytics and Visualizations' tab to refine your data selection based on various criteria."),
                     tags$li("The visualizations will update based on your selections, showing the top tools and trends."),
                     tags$li("You can view quick facts about the dataset in the Quick Facts box."),
                     tags$li("Download the dataset for further analysis using the Download Data button."),
                     tags$li("For detailed explanations of the dataset variables, click on the 'Variable Descriptions' button.")
                   ),
                   p("If you have any questions, feel free to reach out to our team for assistance.")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Info Boxes for Top Items
  output$top_campuses_box <- renderUI({
    create_top_box(graduate_data, "Campus", "Top 5 Campuses")
  })
  
  output$top_edu_level_box <- renderUI({
    create_top_box(graduate_data, "EduLevel", "Top 5 Education Levels")
  })
  
  output$popular_industries_box <- renderUI({
    create_top_box(graduate_data, "Industry", "Top 5 Industries")
  })
  
  # Toggle dataset descriptions visibility
  observeEvent(input$toggle_description, {
    showModal(modalDialog(
      title = "Variable Descriptions",
      fluidRow(
        column(12, p("Campus: The Eduvos campus the graduate is from.")),
        column(12, p("StudyField: The graduate's field of study (IT, Data Science, or Computer Science).")),
        column(12, p("Branch: Primary Type of work.")),
        column(12, p("Role: Type of Developer role at work.")),
        column(12, p("EduLevel: Highest level of education achieved.")),
        column(12, p("ProgLang: Programming languages the graduate works with.")),
        column(12, p("Databases: Databases the graduate works with.")),
        column(12, p("Platform: The cloud platform the graduate works with.")),
        column(12, p("WebFramework: The web framework the graduate uses in their work.")),
        column(12, p("Industry: The industry in which the graduate works.")),
        column(12, p("AISearch: AI search tool (e.g., ChatGPT, Bing AI) the graduate uses.")),
        column(12, p("AITool: AI developer tool (e.g., GitHub Copilot) the graduate uses.")),
        column(12, p("Employment: The employment status and type of the graduate."))
      ),
      easyClose = TRUE
    ))
  })
  
  # Data Preview
  output$data_preview <- renderDT({
    datatable(graduate_data, options = list(scrollX = TRUE)) %>%
      formatStyle(columns = names(graduate_data), color = "white")
  })
  
  # Download Data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("graduate_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(graduate_data, file, row.names = FALSE)
    }
  )
  
  # Filtered Data
  filtered_data <- reactive({
    data <- graduate_data
    if (input$study_field != "All") {
      data <- data %>% filter(StudyField == input$study_field)
    }
    if (input$industry != "All") {
      data <- data %>% filter(Industry == input$industry)
    }
    return(data)
  })
  
  # Render Dynamic Graph
  render_dynamic_graph <- function(data, column_name, title) {
    tool_data <- data %>%
      separate_rows(!!sym(column_name), sep = ";") %>%
      filter(!!sym(column_name) != "") %>%
      count(!!sym(column_name), sort = TRUE) %>%
      head(10)
    
    if (input$graph_type == "Bar Plot") {
      return(
        ggplot(tool_data, aes(x = reorder(str_to_title(!!sym(column_name)), -n), y = n, fill = !!sym(column_name))) +
          geom_bar(stat = "identity") +
          labs(title = title, x = "", y = "Count") +
          theme_minimal()
      )
    } else if (input$graph_type == "Dot Plot") {
      return(
        ggplot(tool_data, aes(x = reorder(str_to_title(!!sym(column_name)), -n), y = n)) +
          geom_point(color = "blue", size = 4) +
          labs(title = title, x = "", y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }
  }
  
  # Render Dynamic Tool Visualization
  output$dynamic_tool_plot <- renderPlot({
    data <- filtered_data()
    column_name <- switch(input$tool_type,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Platforms" = "Platform",
                          "Web Frameworks" = "WebFramework",
                          "AI Search Tools" = "AISearch",
                          "AI Tools" = "AITool")
    
    render_dynamic_graph(data, column_name, paste("Top 10 Tools:", input$tool_type))
  })
  
  # Render Data Tables for Counts
  output$dynamic_count_table <- renderDT({
    data <- filtered_data()
    column_name <- switch(input$tool_type,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Platforms" = "Platform",
                          "Web Frameworks" = "WebFramework",
                          "AI Search Tools" = "AISearch",
                          "AI Tools" = "AITool")
    
    tool_data <- data %>%
      separate_rows(!!sym(column_name), sep = ";") %>%
      filter(!!sym(column_name) != "") %>%
      count(!!sym(column_name), sort = TRUE)
    
    # Create the datatable and apply formatting
    datatable(tool_data, options = list(scrollX = TRUE, pageLength = 5)) %>%
      formatStyle(columns = names(tool_data), color = "white")
  })
  
  # Overview Page Graphs
  render_tech_plot <- function(column_name, title) {
    renderPlot({
      tech_data <- graduate_data %>%
        separate_rows(!!sym(column_name), sep = ";") %>%
        filter(!!sym(column_name) != "") %>%
        count(!!sym(column_name), sort = TRUE) %>%
        head(5)
      
      ggplot(tech_data, aes(x = reorder(str_to_title(!!sym(column_name)), -n), y = n, fill = !!sym(column_name))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = title, x = "", y = "Count") +
        theme_few()
    })
  }
  
  output$language_plot <- render_tech_plot("ProgLang", "Top 5 Programming Languages")
  output$database_plot <- render_tech_plot("Databases", "Top 5 Databases")
  output$platform_plot <- render_tech_plot("Platform", "Top 5 Platforms")
  output$webframework_plot <- render_tech_plot("WebFramework", "Top 5 Web Frameworks")
  output$aisearch_plot <- render_tech_plot("AISearch", "Top 5 AI Search Tools")
  output$aitool_plot <- render_tech_plot("AITool", "Top 5 AI Tools")
  
  # Create UI Output for Top Items
  create_top_box <- function(data, column, title, n = 5) {
    top_items <- data %>%
      filter(!!sym(column) != "") %>%
      count(!!sym(column), sort = TRUE) %>%
      head(n) %>%
      mutate(Rank = row_number())
    
    formatted_items <- paste(top_items$Rank, str_to_title(top_items[[column]]), sep = ". ", collapse = "<br>")
    
    tags$div(HTML(formatted_items))
  }
  
  # Filtered Comparison Data
  filtered_comparison_data <- function() {
    data <- graduate_data
    
    if (length(input$compare_study_fields) > 0) {
      data <- data %>% filter(StudyField %in% input$compare_study_fields)
    }
    if (length(input$compare_industries) > 0) {
      data <- data %>% filter(Industry %in% input$compare_industries)
    }
    if (length(input$compare_campuses_cmp) > 0) {
      data <- data %>% filter(Campus %in% input$compare_campuses_cmp)
    }
    
    return(data)
  }
  
  # Create Comparison Plot
  create_comparison_plot <- function(data, tool_column, group_by, title) {
    if (nrow(data) > 0) {
      comparison_data <- data %>%
        separate_rows(!!sym(tool_column), sep = ";") %>%
        filter(!!sym(tool_column) != "") %>%
        count(!!sym(tool_column), !!sym(group_by), sort = TRUE) %>%
        top_n(10, n)
      
      if (nrow(comparison_data) > 0) {
        ggplot(comparison_data, aes(x = reorder(!!sym(tool_column), -n), y = n, fill = !!sym(group_by))) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = title, x = tool_column, y = "Count") +
          theme_minimal() +
          coord_flip()
      }
    }
  }
  
  # Render Comparison Plots
  output$comparison_plot_sf <- renderPlot({
    comparison_data <- filtered_comparison_data()
    tool_column <- switch(input$compare_tool_sf,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Platforms" = "Platform",
                          "Web Frameworks" = "WebFramework",
                          "AI Search Tools" = "AISearch",
                          "AI Tools" = "AITool")
    
    create_comparison_plot(comparison_data, tool_column, "StudyField", "Comparison by Study Fields")
  })
  
  output$comparison_plot_ind <- renderPlot({
    comparison_data <- filtered_comparison_data()
    tool_column <- switch(input$compare_tool_ind,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Platforms" = "Platform",
                          "Web Frameworks" = "WebFramework",
                          "AI Search Tools" = "AISearch",
                          "AI Tools" = "AITool")
    
    create_comparison_plot(comparison_data, tool_column, "Industry", "Comparison by Industries")
  })
  
  output$comparison_plot_cmp <- renderPlot({
    comparison_data <- filtered_comparison_data()
    tool_column <- switch(input$compare_tool_cmp,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Platforms" = "Platform",
                          "Web Frameworks" = "WebFramework",
                          "AI Search Tools" = "AISearch",
                          "AI Tools" = "AITool")
    
    create_comparison_plot(comparison_data, tool_column, "Campus", "Comparison by Campuses")
  })
  
  output$comparison_plot_role <- renderPlot({
    comparison_data <- filtered_comparison_data()
    tool_column <- switch(input$compare_tool_role,
                          "Programming Languages" = "ProgLang",
                          "Databases" = "Databases",
                          "Platforms" = "Platform",
                          "Web Frameworks" = "WebFramework",
                          "AI Search Tools" = "AISearch",
                          "AI Tools" = "AITool")
    
    if (length(input$compare_roles) > 0) {
      comparison_data <- comparison_data %>%
        filter(RoleCategory %in% input$compare_roles)
      
      create_comparison_plot(comparison_data, tool_column, "RoleCategory", "Comparison by Roles")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

