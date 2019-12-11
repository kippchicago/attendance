#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

Sys.setenv(TZ='GMT') # required to not have dates adjusted by feather when files loaded

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(magrittr)
library(DT)
library(googleCloudStorageR)
library(feather)
library(kippcolors)


gcs_global_bucket("idea_attendance")

source("lib.R")


ada_weekly_school_grade <- gcs_get_feather("ada_weekly_school_grade.feather")


ada_weekly_grade_hr <- gcs_reactive_poll("ada_weekly_grade_hr.feather")


attend_student_ytd <- gcs_reactive_poll("attend_student_ytd.feather")


#gcs_get_object("attend_student.feather", saveToDisk = "attend_student.feather", overwrite = TRUE)
#attend_student <- read_feather("attend_student.feather")
schools <- unique(ada_weekly_school_grade$school_abbrev)

#get quarterly goals
ada_quarterly_goals <- readr::read_csv("ada_quarterly_goals.csv")

add_goals <- . %>% dplyr::left_join(ada_quarterly_goals, by = c("school_abbrev", "quarter"))

ada_weekly_school_grade <- ada_weekly_school_grade %>% add_goals()
ada_weekly_grade_hr <- ada_weekly_grade_hr %>% add_goals()

# UI ----------------------------------------------------------------------

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
     selectInput(inputId = "schools_selected",
                 label = "Select School(s):",
                 choices = schools,
                 selected = schools,
                 multiple = TRUE
     ),
     sidebarMenu(
        menuItem("ADA By School", tabName = "school", icon = icon("university")), 
        menuItem("ADA By Homeroom", tabName = "hr", icon = icon("home")), 
        menuItem("Student Detail", tabName = "students", icon = icon("users"))
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
                        
                        box(title = "Weekly ADA",
                            DTOutput("ada_weekly_dt") %>% withSpinner() ,
                            status = "primary",
                            solidHeader = TRUE, 
                            width = NULL),
                        
                        box(title = "Monthly YTD ADA",
                            DTOutput("ada_monthly_dt")  %>% withSpinner(),
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL)
                 ),
                 
                 column(width = 5, 
                        tabBox(width = NULL,
                          title = "Chronic Truancy",
                          tabPanel("Rate", 
                                   DTOutput("truancy_rates") %>% withSpinner()),
                          tabPanel(title = "Students",
                              DTOutput("chronic_truants")  %>% withSpinner(),
                              status = "warning",
                              solidHeader = TRUE, 
                              width = NULL)
                        )
                        
                 )
               )
            ),
       tabItem(tabName = "hr",
               h2("ADA by Homeroom"),
               fluidRow(
                 column(6,
                        box(title = "HR ADA",
                            plotOutput("hr_rank", 
                                       brush = brushOpts(id = "hr_brush",
                                                         direction = "y")) %>% 
                              withSpinner(),
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL,
                            height  = 900)
                        ),
                 column(6,
                        box(title = "ADA last 5 weeks",
                            plotOutput("hr_details"),
                            status = "primary",
                            width = NULL)
                 )
                )
               ),
       tabItem(tabName = "students",
               h2("Student Attendance Details"),
               fluidRow(
                 column(12,
                        box(title = "Student Attendance Details",
                            DTOutput("students_dt") %>% withSpinner(),
                            status = "primary",
                            solidHeader = TRUE,
                            width = NULL)
                        )
               )
       )
       )
     )
     
 )
}


# SERVER ------------------------------------------------------------------

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
  
  
  # debounce selections
  grades_selected <- reactive(input$grades_selected) %>% debounce(1000)
  schools_selected <- reactive(input$schools_selected) %>% debounce(1000)
  
  

# Quartlery Tables -----------------------------------------------------------

  ada_quarter_reactive <-reactive( {
    
    ada_by_quarter  <- ada_weekly_school_grade %>%
      filter(date <= today()) %>%
      group_by(school_abbrev, quarter) %>%
      filter(date == max(date),
             grade_level %in% grades_selected()) %>%
      summarise(quarterly_ada = sum(cum_quarterly_present)/sum(cum_quarterly_enrolled) * 100,
                ada_goal = last(goal)) %>%
      select(School = school_abbrev,
             quarter,
             quarterly_ada,
             ada_goal
             ) %>%
      mutate(quarterly_ada = round(quarterly_ada, 2),
             meets_goal =  case_when(
               quarterly_ada - ada_goal  >= 0 ~ 1,
               quarterly_ada - ada_goal  < -0.5 ~ -1,
               TRUE ~ 0
             )) 
    
    quarterly_goals <- ada_by_quarter %>%
      select(School, quarter, meets_goal) %>%
      tidyr::spread(quarter, meets_goal) %>%
      janitor::clean_names()
    
    quarterly_ada <- ada_by_quarter %>%
      select(School, quarter, quarterly_ada) %>%
      tidyr::spread(quarter, quarterly_ada) %>%
      inner_join(quarterly_goals, by = c("School" = "school"))
    
    ada_ytd <- ada_weekly_school_grade %>%
      filter(date <= today(),
             grade_level %in% grades_selected()) %>%
      group_by(school_abbrev) %>%
      filter(date == max(date)) %>%
      summarise(ytd_ada = sum(ytd_present)/sum(ytd_enrolled) * 100) %>%
      select(School = school_abbrev,
             ytd_ada) %>%
      mutate(quarter = "YTD",
             ytd_ada = round(ytd_ada, 2)) %>%
      tidyr::spread(quarter, ytd_ada) %>%
      mutate(ytd = case_when(
        YTD - 96.1  >= 0 ~ 1,
        YTD - 96.1  < -0.5 ~ -1,
        TRUE ~ 0
      ))
    
    
    ada_quarter <- quarterly_ada %>% inner_join(ada_ytd, by = "School")
    
    })
  
  output$ada_quarterly_dt <- renderDT({
    ada <- ada_quarter_reactive() #%>% as.data.frame()
    
    col_names <- ada  %>% ungroup() %>% select_if(is.numeric) %>% names()
    quarter_names <- c("Q1", "Q2", "Q3", "Q4", "YTD")
    display_mask <-col_names %in% quarter_names
    display_cols <- col_names[display_mask]
    value_cols <- col_names[!display_mask]
    value_idx <- which(!display_mask, TRUE)
    
    datatable(ada,
              class = "compact stripe hover",
              rownames = FALSE,
              options = list(
                dom = "t",
                columnDefs = list(list(targets = value_idx, visible = FALSE))
                )
              ) %>%
      formatStyle(columns = display_cols,
                  valueColumns = value_cols,
                  color = styleEqual(c(-1, 0, 1), c("red","#F7941E", "#439539"))
                  )
      
    #backgroundColor = styleInterval(96, c("red", "green")))
    
  })
  
  
  #output$ada_quarterly <- renderFormattable(ada_quarter_tbl)
  

# Weekly Tables -----------------------------------------------------------

  ada_by_week_reactive  <- reactive({
    weekly_ada <- ada_weekly_school_grade %>%
      filter(date <= today(),
             grade_level %in% grades_selected(),
             school_abbrev %in% schools_selected()) %>%
      group_by(school_abbrev, week_of_date_short_label) %>%
      summarize(weekly_ada = sum(cum_weekly_present)/sum(cum_weekly_enrolled)*100,
                quarter = last(quarter),
                ada_goal = last(goal)
      ) %>%      
      arrange(school_abbrev, desc(week_of_date_short_label)) %>%
      top_n(5, week_of_date_short_label) %>%  
      mutate(weekly_ada = round(weekly_ada, 2),
             meets_goal =  case_when(
               weekly_ada - ada_goal  >= 0 ~ 1,
               weekly_ada - ada_goal  < -0.5 ~ -1,
               TRUE ~ 0
             )) %>%
      select(School = school_abbrev, week_of_date_short_label, weekly_ada, meets_goal)
    
    weekly_goals <- weekly_ada %>% select(-weekly_ada) %>%  
      tidyr::spread(week_of_date_short_label, meets_goal) %>%
      janitor::clean_names()
    
    ada_by_week <- weekly_ada %>%
      select(-meets_goal) %>%
      tidyr::spread(week_of_date_short_label, weekly_ada) %>%
      inner_join(weekly_goals, by = c("School"="school"))
    
    
    
    
  
    ada_weekly_series <- ada_weekly_school_grade %>%
      filter(date <= today(),
             grade_level %in% grades_selected()) %>%
      group_by(school_abbrev, week_of_date_short_label) %>%
      arrange(school_abbrev, desc(week_of_date_short_label)) %>%
      #group_by(School, week_of_date_short_label) %>%
      summarize(weekly_ada = sum(cum_weekly_present)/sum(cum_weekly_enrolled)*100,
                quarter = last(quarter),
                ada_goal = last(goal)
      ) %>% 
      mutate(weekly_ada = round(weekly_ada, 2)) %>%
      select(School = school_abbrev, weekly_ada, ada_goal) %>%
      summarise('Weekly (+/- Goal)' = paste(weekly_ada - ada_goal, collapse = ",")) 

    inner_join(ada_by_week, ada_weekly_series, by = "School")
    
    #ada_by_week
  })
    
# sparklines
  
  bar_string <- "type: 'bar', barColor: '#17345B', negBarColor: '#E27425', highlightColor: 'black'"
  cb_bar <- JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                      bar_string, " }); }"), collapse = "")
  
  weeks_len <- isolate({
    col_names <- ada_by_week_reactive()  %>% 
      ungroup() %>% 
      select_if(is.numeric) %>% names()
    
    length(col_names)
  })
  
  display_cols_idx <- c(1:(weeks_len/2))
  value_cols_idx <- c(((weeks_len/2)+1):weeks_len)
  
  
  js <- "function(data, type, full){ return '<span class=spark>' + data + '</span>' }"
  colDefs1 <- list(list(targets = value_cols_idx, visible = FALSE),
                   list(targets = value_cols_idx+1, render = JS(js))
                  )

  
  output$ada_weekly_dt <- renderDT({
    
    col_names <- ada_by_week_reactive()  %>% 
      ungroup() %>% 
      select_if(is.numeric) %>% names()
    
    
    
    display_cols <- col_names[display_cols_idx]
    value_cols <- col_names[value_cols_idx]
    
    dt_weekly <- datatable(
      ada_by_week_reactive(),
              class = "compact stripe hover",
              rownames = FALSE,
              options = list(
               columnDefs = colDefs1,
               fnDrawCallback = cb_bar,
                dom = "t"
              )) %>%
      formatStyle(columns = display_cols,
                  valueColumns = value_cols,
                    
                  color = styleEqual(c(-1, 0, 1), c("red","#F7941E", "#439539")))

    dt_weekly$dependencies <- append(dt_weekly$dependencies, htmlwidgets::getDependency("sparkline"))

    dt_weekly
    })
  

# Monthly table -----------------------------------------------------------
  output$ada_monthly_dt <- DT::renderDT({
    
    ytd_ada <- ada_weekly_school_grade %>% 
      select(date, school_abbrev, ytd_enrolled, ytd_present, quarter, goal) %>% 
      mutate(month = month(date, label = TRUE)) %>% 
      arrange(school_abbrev, date) %>%
      mutate(month = forcats::fct_inorder(as.character(month), ordered = TRUE)) %>%
      group_by(school_abbrev, month) %>% 
      filter(date == max(date)) %>%
      summarize(ytd_enrolled = sum(ytd_enrolled),
                ytd_present = sum(ytd_present),
                quarter = last(quarter),
                ada_goal = last(goal)) %>%
      mutate(ytd_ada = round(ytd_present/ytd_enrolled*100,2),
             meets_goal =  case_when(
               ytd_ada - ada_goal  >= 0 ~ 1,
               ytd_ada - ada_goal  < -0.5 ~ -1,
               TRUE ~ 0
               )
             )
    
    
    ytd_goals <- ytd_ada %>%
      select(school_abbrev, month, meets_goal) %>%
      tidyr::spread(month, meets_goal) %>%
      janitor::clean_names() %>%
      rename(School = school_abbrev)
    
    ytd_by_month <- ytd_ada %>%
      select(school_abbrev, month, ytd_ada) %>%
      tidyr::spread(month, ytd_ada) %>%
      rename(School = school_abbrev) %>%
      inner_join(ytd_goals, by = "School")
    
    col_names <- ytd_by_month %>% ungroup() %>% select(-1) %>% names()
    
    month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    display_mask <-col_names %in% month_names
    display_cols <- col_names[display_mask]
    value_cols <- col_names[!display_mask]
    value_idx <- which(!display_mask, TRUE)
    
    #ytd_by_month
    
    dt_monthly <- datatable(
      ytd_by_month,
      class = "compact stripe hover",
      rownames = FALSE,
      options = list(
        dom = "t", 
        columnDefs = list(list(targets = value_idx, visible = FALSE))
        )
      ) %>%
      formatStyle(columns = display_cols,
                  valueColumns = value_cols,
                  color = styleEqual(c(-1, 0, 1), c("red","#F7941E", "#439539")))
  }) 
  

# Chronic truants DT ------------------------------------------------------
  
    truancy_rates <- reactive({
      attend_student_ytd %>% 
      filter(
             grade_level %in% grades_selected(),
             school_abbrev %in% schools_selected()) %>%
      mutate(chronic_truant = ada <= 90) %>%
      group_by(school_abbrev) %>%
      summarize(Rate = round(sum(chronic_truant, na.rm = TRUE)/n()*100,1)) %>%
      select(School = school_abbrev,
             #Grade = grade_level,
             Rate
      ) %>%
      ungroup() 
      })
      #mutate(Grade = as.factor(Grade)) %>%
      #tidyr::spread(Grade, Rate) %>%
     
  output$truancy_rates <- renderDT(
    truancy_rates() %>%
      datatable(class = "compact stripe hover",
                rownames = FALSE, 
                options = list(dom = "t")
                ) %>%
      formatStyle("Rate",
                  background = styleColorBar(truancy_rates()$Rate, '#F7941E'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'),
    server = FALSE
  )
  
  
  
  output$chronic_truants <- renderDT(
    attend_student_ytd %>% 
      filter(ada <= 90,
             grade_level %in% grades_selected(),
             school_abbrev %in% schools_selected()) %>%
      select(School = school_abbrev,
             Grade = grade_level,
             name = lastfirst,
             ADA = ada
             ) %>%
      ungroup() %>%
      mutate(Grade = as.factor(Grade),
             ADA = round(ADA/100, 3)) %>%
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
      formatPercentage("ADA"),
    server = FALSE
  )
  

# Student ADA Histogram ---------------------------------------------------

  output$ada_hist <- renderPlot({
    attend_student_ytd %>%
      ungroup() %>%
      filter(grade_level %in% grades_selected(),
             school_abbrev %in% schools_selected()) %>%
      ggplot(aes(x = ada)) +
      geom_histogram(aes(fill = ada <= 90), binwidth = 2, color = "white", size=.24) +
      scale_fill_kipp(reverse = TRUE) +
      theme_kipp_light() +
      facet_grid( ~ school_abbrev) +
      theme(legend.position = "bottom") +
      labs(x = "Average Daily Attendance",
           y = "Count of students",
           fill = "ADA ≤ 90%") 
  }
  , height = 300
  )
  

# Home Room Page ----------------------------------------------------------


# HR Rank Plot ------------------------------------------------------------

  ada_weekly_grade_hr_reactive <- reactive({
    ada_weekly_grade_hr %>%
      ungroup %>%
      filter(date == max(date),
             grade_level %in% grades_selected(),
             school_abbrev %in% schools_selected()) %>%
      mutate(hr = sprintf("%s (%s)", home_room, school_abbrev)) %>%
      select(hr, ytd_ada, school_abbrev, home_room, weekly_ada, week_of_date_short_label, week_of_date) %>%
      arrange(ytd_ada) %>% 
      mutate(hr  = forcats::fct_inorder(hr))
  })
  
  output$hr_rank <- renderPlot({
    ada_weekly_grade_hr_reactive() %>% 
      ggplot(aes(x = hr, y = ytd_ada)) +
      geom_col(aes(fill = school_abbrev)) +
      geom_hline(aes(yintercept = 96), color="#F4EFEB", size = 2) +
      coord_flip(ylim = c(93, 100)) +
      scale_fill_kipp() + 
      theme_kipp_light() +
      theme(legend.position = "bottom",
            axis.text.y = element_text(size = 7)) +
      labs(x = "YTD ADA",
           y = "Homeroom",
           fill = "School")
  }, height = 800)
  

# HR Detail Pin Plot ------------------------------------------------------

  
  output$hr_details <- renderPlot({
    
    validate(need(input$hr_brush, "Click and drag in chart to select HR details"))
    
    y_max <- ceiling(input$hr_brush$ymax)
    y_min <- floor(input$hr_brush$ymin)
    
    ada_table <- ada_weekly_grade_hr_reactive()
    
    selected_homerooms <- ada_table[y_min:y_max,] %>%
      select(home_room, school_abbrev, hr) 
    
    weeks_ago_5 <- floor_date(today() - weeks(5), unit = "week")
    
    ada_weekly_grade_hr_selected_hrs <- ada_weekly_grade_hr %>%
      filter(date >= weeks_ago_5) %>%
      inner_join(selected_homerooms, by = c("school_abbrev", "home_room")) %>%
      mutate(day_in_week = lubridate::wday(x = date, abbr=TRUE, label=TRUE) )
    
    ggplot(ada_weekly_grade_hr_selected_hrs,
           aes(x=day_in_week, y = weekly_ada, color = weekly_ada <96)) +
      geom_segment(aes(x=day_in_week, xend=day_in_week, y=96, yend=weekly_ada, 
                       color = weekly_ada <96), show.legend = FALSE) +
      geom_point(show.legend = FALSE) +
      geom_hline(aes(yintercept = 96)) +
      facet_grid(rev(hr) ~ week_of_date_short_label, as.table = FALSE) +
      scale_x_discrete(labels = c("M", "T", "W", "H", "F")) +
      theme_kipp_light() +
      scale_color_kipp() +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(x = "Day in week",
           y = "ADA")
      
    
    
    
    
    })
  
  output$students_dt <- renderDT({
    DT::datatable(attend_student_ytd %>%
                    mutate(ada = ada/100,
                           student_number = as.character(student_number)) %>%
                    filter(school_abbrev %in% input$schools_selected,
                           grade_level %in% input$grades_selected) %>%
                    select(ID = student_number,
                           Name = lastfirst,
                           Grade = grade_level,
                           School = school_abbrev,
                           ADA = ada,
                           Absences = absent,
                           "Days Present" = present,
                           "Days Enrolled" = enrolled
                           ) %>%
                    arrange(desc(Absences)),
                  filter = 'top',
                  rownames = FALSE,
                  extensions = c('Scroller', 'Buttons'),
                  options = list(
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    dom = 'Bfrtip',
                    deferRender = TRUE,
                    scrollY = 400,
                    scroller = TRUE,
                    scrollX = TRUE
                    )
                  ) %>%
      formatPercentage("ADA", 2) %>%
      formatStyle(
        "ADA",
        color = styleInterval(c(.9, .97), 
                              c("red", kipp_colors$darkorange, kipp_colors$green))
        ) %>%
      formatStyle(
        "Absences",
        color = styleInterval(c(2, 3), 
                              c("black", kipp_colors$lightorange, "red")
                              )
      ) 
      
    
  }, server = FALSE)
  
}

# Run the application 
enableBookmarking(store = "server")
shinyApp(ui = ui, server = server)

