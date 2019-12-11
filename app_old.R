#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(magrittr)
library(DT)
library(googleCloudStorageR)

gcs_global_bucket("idea_attendance")

#gcs_load(file = "ada.rda")
ada_records <- reactivePoll(3600000, 
                            session, 
                            checkFunc = function() {
                              x <- gcs_list_objects()
                              x$updated
                            }, 
                            valueFunc = function() {
                              gcs_load(file = "ada.rda")
                            }
)




ui <- function(request){
 dashboardPage(
  dashboardHeader(title = "Attendance"),
   dashboardSidebar(
     selectInput(inputId = "grades_selected",
                 label = "Select Grade(s):",
                 choices = 0:8,
                 selected = 0:8,
                 multiple = TRUE
     ),
     sidebarMenu(
        menuItem("ADA By School", tabName = "school", icon = icon("university")), 
        menuItem("ADA By Homeroom", tabName = "hr", icon = icon("home")) 
     ),
     bookmarkButton()
   ),
   dashboardBody(
     tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "kippcolors_blue.css")
     ),
     tabItems(
       tabItem(tabName = "school",
               h2("ADA By School"),
               fluidRow(
                 box(width = 12,
                     height = 375,
                     title = "Student ADA Histogram",
                     plotOutput("ada_hist") %>% withSpinner(),
                     status = "primary",
                     solidHeader = TRUE
                 )
               ),
               
               fluidRow(
                 column(width = 7,
                        box(title = "Quarterly and YTD ADA",
                            DTOutput("ada_quarterly_dt")  %>% withSpinner(),
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL),
                        
                        box(title = "Weekly ADA DT",
                            DTOutput("ada_weekly_dt") %>% withSpinner() ,
                            status = "primary",
                            solidHeader = TRUE, 
                            width = NULL)
                 ),
                 
                 column(width = 5, 
                        box(title = "Chronically Absent",
                            DTOutput("chronic_truants")  %>% withSpinner(),
                            status = "warning",
                            solidHeader = TRUE, 
                            width = NULL)
                 )
               )
            ),
       tabItem(tabName = "hr",
               h2("ADA by Homeroom"),
               fluidRow(
                 column(6,
                        box(title = "HR ADA",
                            plotOutput("hr_rank") %>% withSpinner(),
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height  = 700)
                        )
               )
               )
       )
     )
     
 )
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(googleCloudStorageR) 
  library(dplyr)
  library(lubridate)
  library(formattable)
  library(DT) 
  library(sparkline)
  library(ggplot2)
  library(kippcolors)
  
  
  grades_selected <- reactive(input$grades_selected) %>% debounce(1000)
  
  ada_quarter_reactive <-reactive( {
    
    ada_by_quarter  <- ada_weekly_school_grade %>%
      filter(date <= today()) %>%
      group_by(schoolabbreviation, quarter) %>%
      filter(date == max(date),
             grade_level %in% grades_selected()) %>%
      summarise(quarterly_ada = sum(cum_quarterly_present)/sum(cum_quarterly_enrolled) * 100) %>%
      select(School = schoolabbreviation,
             quarter,
             quarterly_ada) %>%
      mutate(quarterly_ada = round(quarterly_ada, 2)) %>%
      tidyr::spread(quarter, quarterly_ada)
    
    ada_ytd <- ada_weekly_school_grade %>%
      filter(date <= today(),
             grade_level %in% grades_selected()) %>%
      group_by(schoolabbreviation) %>%
      filter(date == max(date)) %>%
      summarise(ytd_ada = sum(ytd_present)/sum(ytd_enrolled) * 100) %>%
      select(School = schoolabbreviation,
             ytd_ada) %>%
      mutate(quarter = "YTD",
             ytd_ada = round(ytd_ada, 2)) %>%
      tidyr::spread(quarter, ytd_ada)
    
    
    ada_quarter <- ada_by_quarter %>% inner_join(ada_ytd, by = "School")
    
    })
  
  output$ada_quarterly_dt <- renderDT({
    ada <- ada_quarter_reactive() #%>% as.data.frame()
    
    col_names <- ada  %>% ungroup() %>% select_if(is.numeric) %>% names()
    datatable(ada,
              class = "compact stripe hover",
              rownames = FALSE,
              options = list(
                dom = "t"
              ))  %>%
      formatStyle(columns = col_names, 
                  color = styleInterval(96, c("red", "black"))
      )
    #backgroundColor = styleInterval(96, c("red", "green")))
    
  })
  
  
  #output$ada_quarterly <- renderFormattable(ada_quarter_tbl)
  
  #### Weekly Table 
  
  ada_by_week_reactive  <- reactive({
    ada_by_week <- ada_weekly_school_grade %>%
      filter(date <= today(),
             grade_level %in% grades_selected()) %>%
      group_by(schoolabbreviation, week_of_date_short_label) %>%
      summarize(weekly_ada = sum(cum_weekly_present)/sum(cum_weekly_enrolled)*100) %>%
      arrange(schoolabbreviation, desc(week_of_date_short_label)) %>%
      top_n(5, week_of_date_short_label) %>%  
      mutate(weekly_ada = round(weekly_ada, 2)) %>%
      select(School = schoolabbreviation, week_of_date_short_label, weekly_ada) %>%
      tidyr::spread(week_of_date_short_label, weekly_ada)
  
  
    ada_weekly_series <- ada_weekly_school_grade %>%
      filter(date <= today(),
             grade_level %in% grades_selected()) %>%
      group_by(schoolabbreviation, week_of_date_short_label) %>%
      arrange(schoolabbreviation, desc(week_of_date_short_label)) %>%
      #group_by(School, week_of_date_short_label) %>%
      summarize(weekly_ada = sum(cum_weekly_present)/sum(cum_weekly_enrolled)*100) %>%
      mutate(weekly_ada = round(weekly_ada, 2)) %>%
      select(School = schoolabbreviation, weekly_ada) %>%
      summarise('Weekly (+/- Goal)' = paste(weekly_ada - 96, collapse = ",")) 

    inner_join(ada_by_week, ada_weekly_series, by = "School")
    
    #ada_by_week
  })
    
  
  # ada_weekly_tbl<-formattable(ada_by_week, list(area(col = 2:6) ~ ada_goal(96)))
  
  # output$ada_weekly<- renderFormattable(ada_weekly_tbl)
  
  # 
  # output$ada_weekly_dt <- renderDT({
  #   col_names <- ada_by_week  %>% select_if(is.numeric) %>% names()
  #    datatable(ada_by_week,
  #              class = "compact stripe hover",
  #              rownames = FALSE,
  #                       options = list(
  #                         dom = "t"
  #                       ))  %>%
  #     formatStyle(columns = col_names, 
  #                 color = styleInterval(96, c("red", "black"))
  #                 )
  #                 #backgroundColor = styleInterval(96, c("red", "green")))
  #   
  #   })
  
# sparklines
  
  
  bar_string <- "type: 'bar', barColor: '#E27425', negBarColor: '#17345B', highlightColor: 'black'"
  cb_bar <- JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                      bar_string, " }); }"), collapse = "")
  
  js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
  colDefs1 <- list(list(targets = 6, render = JS(js)))
  

  output$ada_weekly_dt <- renderDT({
    
    col_names <- ada_by_week_reactive()  %>% select_if(is.numeric) %>% names()
    
    dt_weekly <- datatable(
      ada_by_week_reactive(),
              class = "compact stripe hover",
              rownames = FALSE,
              options = list(
               columnDefs = colDefs1,
               fnDrawCallback = cb_bar,
                dom = "t"
              )) %>%
      formatStyle(columns = col_names,
                  color = styleInterval(96, c("red", "black")))

    dt_weekly$dependencies <- append(dt_weekly$dependencies, htmlwidgets::getDependency("sparkline"))

    dt_weekly
    })
  
  output$chronic_truants <- renderDT(
    attend_student_ytd %>% 
      filter(ada <= 90,
             grade_level %in% grades_selected()) %>%
      select(School = schoolabbreviation,
             Grade = grade_level,
             name = lastfirst,
             ADA = ada
             ) %>%
      ungroup() %>%
      mutate(Grade = as.factor(Grade),
             ADA = ADA/100) %>%
      arrange(ADA) %>%
      datatable(class = "compact stripe hover",
                rownames = FALSE, 
                filter = 'top',
                extensions = 'Buttons', 
                options = list(
                  dom="Btp",
                  pageLength = 14,
                  buttons = list('copy', 
                                 'print', 
                                 list(extend = 'collection',
                                      buttons = c('csv', 'excel', 'pdf'),
                                      text = 'Download')
                                 )))%>%
      formatPercentage("ADA")
  )
  
  output$ada_hist <- renderPlot({
    attend_student_ytd %>%
      ungroup() %>%
      filter(grade_level %in% grades_selected()) %>%
      ggplot(aes(x = ada)) +
      geom_histogram(aes(fill = ada <= 90), binwidth = 2, color = "white", size=.24) +
      scale_fill_kipp(reverse = TRUE) +
      theme_kipp_light() +
      facet_grid( ~ schoolabbreviation) +
      theme(legend.position = "bottom") +
      labs(x = "Average Daily Attendance",
           y = "Count of students",
           fill = "ADA ≤ 90%") 
  }
  , height = 300
  )
  
  output$hr_rank <- renderPlot({
    ada_weekly_grade_hr %>%
      ungroup %>%
      filter(date == max(date)) %>%
      mutate(hr = sprintf("%s (%s)", home_room, schoolabbreviation),
             hr = ifelse(home_room == "K 1st MSU", paste(hr, grade_level, sep = " - "), hr)) %>%
      select(hr, ytd_ada, schoolabbreviation) %>%
      arrange(ytd_ada) %>%
      mutate(home_room  = forcats::fct_inorder(hr)) %>% 
      ggplot(aes(x = home_room, y = ytd_ada)) +
      geom_col(aes(fill = schoolabbreviation)) +
      coord_flip(ylim = c(93, 100)) +
      scale_fill_kipp() + 
      theme_kipp_light() +
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 7)) +
      labs(x = "YTD ADA",
           y = "Homeroom",
           fill = "School")
  }, height = 600)
}

# Run the application 
enableBookmarking(store = "server")
shinyApp(ui = ui, server = server)

