#load libraries
library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)

#load ui
shinyUI(fluidPage(
  
  #css
  includeCSS("_utilities/style.css"),
  
  #remove warning messages
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  #load page layout
  dashboardPage(
    
    skin = "green",
    
    dashboardHeader(title="Qwerty", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<p style = 'text-align: center;'><small><a target='_blank'>Text Analysis Tool</a></small></p>",
                         "<br>"
                       )),
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       #menuItem("About", tabName = "about", icon = icon("tasks")),
                       menuItem("Upload File", tabName = "configuration", icon = icon("console", lib = "glyphicon")),
                       menuItem("Token Frequencies", tabName = "token_frequencies", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Word Contributions", tabName = "word_contributions", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Bigram Analysis", tabName = "bigram_analysis", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Correlation Analysis", tabName = "correlation_analysis", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Topic Modeling", tabName = "topic_modeling", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Beta Spreads", tabName = "beta_spreads", icon = icon("stats", lib = "glyphicon")),
                       #menuItem("Word Embeddings", tabName = "word_embeddings", icon = icon("stats", lib = "glyphicon")),
                       menuItem("Appendix", tabName = "appendix", icon = icon("asterisk")),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "</table>",
                         "<br>"),
                         HTML(paste0(
                           "<script>",
                           "var today = new Date();",
                           "var yyyy = today.getFullYear();",
                           "</script>",
                           "<p style = 'text-align: center;'><small>&copy; - <a href='https://shiny.rstudio.com/' target='_blank'>Shiny</a> - <script>document.write(yyyy);</script></small></p>")
                          ))
                     )
                     
    ), #end sidebar
    
    dashboardBody(
      
      tabItems(
        #home
        tabItem(tabName = "home",
                
                includeMarkdown("_utilities/home.md")
                
        ),
        
        #about
        #tabItem(tabName = "about", includeMarkdown("_utilities/about.md")
                
        #),
        
        #configuration
        tabItem(tabName = "configuration", 
                
                includeMarkdown("_utilities/configuration.md"),
                fileInput("upload", NULL, buttonLabel = "Upload...", multiple = FALSE),
                tableOutput("fileUpload")
                
        ),
        
        #token frequencies
        tabItem(tabName = "token_frequencies",
                
                column(6, plotOutput("ggplotPositiveTokens") %>% withSpinner(color = "green")),
                column(6, plotOutput("ggplotNegativeTokens") %>% withSpinner(color = "green"))

        ), 
        
        #bigram analysis
        tabItem(tabName = "bigram_analysis",
                
                column(6, plotOutput("bigramAnalysisOne") %>% withSpinner(color = "green")),
                column(6, plotOutput("bigramAnalysisTwo") %>% withSpinner(color = "green"))
                
        ),
        
        #word contributions
        tabItem(tabName = "word_contributions",
                
                column(10, plotOutput("wordContributions") %>% withSpinner(color = "green"))
                
        ),
        
        #correlation analysis
        tabItem(tabName = "correlation_analysis",
                
                column(6, plotOutput("correlationAnalysisOne") %>% withSpinner(color = "green")),
                column(6, plotOutput("correlationAnalysisTwo") %>% withSpinner(color = "green"))
                
        ),
        
        #topic modeling
        tabItem(tabName = "topic_modeling",
                
                column(10, plotOutput("topicModeling") %>% withSpinner(color = "green"))
                
        ),
        
        #beta spreads
        tabItem(tabName = "beta_spreads",
                
                column(10, plotOutput("betaSpreads") %>% withSpinner(color = "green"))
                
        ),
        
        #appendix
        tabItem(tabName = "appendix", includeMarkdown("_utilities/appendix.md")
                
        )
      )
    )

)))