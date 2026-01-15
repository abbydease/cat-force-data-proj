library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(bslib)

cat_force_pivot <- read_excel("/Users/abigaildease/Desktop/cat-force-data-proj/data/CAT Pivoted Force Data.xlsx")


ui <- page_navbar(

   title = "Carolina Aquatic Team Data Dashboard",
   navbar_options = navbar_options(
     bg = "#4798d1",
     underline = TRUE
   ),
   
   #individual swimmer panel
   nav_panel(
     "Individual",
    page_sidebar(
      sidebar = sidebar(
           selectInput("selected_swimmer", "Athlete:",
                       choices = sort(unique(cat_force_pivot$Name))),
           selectInput("selected_session", "Session:",
                       choices = sort(unique(cat_force_pivot$Session)),
                       multiple = TRUE)
           
       ),
      layout_columns(
        card(
          card_header(textOutput("swimmer_overview_title")),
          plotOutput("sessionPlot")
           ),
        card(
          card_header("Session Averages"),
          tableOutput("avgTable")
        )
         )
       )
     ),
   nav_panel(
     "Team",
     page_sidebar(
       sidebar = sidebar(
          selectInput("selected_gender", "Men/Women:",
                   choices = sort(unique(cat_force_pivot$Gender)),
                   multiple = TRUE),
          selectInput("selected_session", "Session:",
                   choices = sort(unique(cat_force_pivot$Session)))
     ),
     layout_columns(
       card(
         card_header("Average Force by Gender over Sessions"),
         plotOutput("genderAvgPlot")
       )
     )
    )
  )
 )

    
server <- function(input, output, session) {
  
  output$swimmer_overview_title <- renderText({
    req(input$selected_swimmer)
    paste("Force Data for:", input$selected_swimmer)
  })
  
  output$team_overview_title <- renderText({
    req(input$selected_session)
    paste("Team Force Data over Session", input$selected_session)
  })

  filtered_data_main <- reactive({
    req(input$selected_swimmer, input$selected_session)
    
    cat_force_pivot %>%
      filter(Name == input$selected_swimmer) %>%
      filter(Session %in% input$selected_session)
  })
  
  
  output$sessionPlot <- renderPlot({
    req(nrow(filtered_data_main()) > 0)
    
    ggplot(filtered_data_main(), aes(x = Attempt, y = Force, color = as.factor(Session))) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(color = "Session")
  })
  
    athlete_averages <- reactive({
      req(filtered_data_main())
      
      filtered_data_main() %>%
        group_by(Name, Session) %>%
        summarize(
          avg_force = mean(Force, na.rm = TRUE),
          max_force = max(Force, na.rm = TRUE),
          .groups = "drop"
        )
    })
    
    
    output$avgTable <- renderTable({
      athlete_averages()
    })
    
    gender_avg <- reactive({
      cat_force_pivot %>%
        filter(Gender %in% input$selected_gender) %>%
        group_by(Gender, Session) %>%
        summarize(avg_force = mean(Force, na.rm = TRUE), .groups = "drop")
    })
    
    output$genderAvgPlot <- renderPlot({
      req(gender_avg())
      
      ggplot(gender_avg(), aes(x = Session, y = avg_force, color = Gender, group = Gender)) +
        geom_line() +
        geom_point() +
        theme_minimal() +
        labs(
          x = "Session Number",
          y = "Average Force",
          color = "Gender"
        ) +
        scale_x_continuous(breaks = unique(gender_avg()$Session))
    })


}


shinyApp(ui = ui, server = server)
