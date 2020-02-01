library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(plyr)
library(RColorBrewer)

#data tables
df2 <- read.csv("data.csv")
df_sf <- filter(data, data$state == "successful" | data$state == "failed")
df_s <- filter(data, data$state == "successful")
df_f <- filter(data, data$state == "failed")

#ui
header <- dashboardHeader(title = "Kickstarter Project")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName="intro", icon = icon("home",lib='glyphicon')),
    menuItem("Overview Statistics", tabName="tab1", icon = icon("stats",lib='glyphicon')),
    menuItem("Company Growth", tabName="tab2", icon = icon("tree-deciduous",lib='glyphicon')),
    menuItem("Successful Rates", tabName="tab3", icon = icon("ok-sign",lib='glyphicon')),
    menuItem("Money Distributions", tabName="tab5", icon = icon("euro",lib='glyphicon')),
    menuItem("Backers' Behavior", tabName="tab6", icon = icon("user",lib='glyphicon')),
    menuItem("Success Factors", tabName="tab7", icon = icon("road",lib='glyphicon'))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tab1",
            fluidRow(
              box(plotlyOutput("plot1"), width = 12),
              box(selectInput("aggregator", label = h3("Total number of projects by"), choices = list("Category" = "main_category", "Year" = "launch_at_year", "Country" = "country"), selected = "main_category"))
            )
    ),
    tabItem(tabName = "tab2",
            fluidRow(
              box(plotlyOutput("plot2"), width = 12),
              box(plotlyOutput("plot2b"), width = 12),
              box(radioButtons("cat", label = h3("By Category"), choices = list("Art" = "art", "Comics" = "comics", "Crafts" = "crafts", "Dance" = "dance", "Design" = "design", "Fashion" = "fashion", "Film & Video" = "film & video", "Food" = "food", "Games" = "games", "Journalism" = "journalism", "Music" = "music", "Photography" = "photography", "Publishing" = "publishing", "Technology" = "technology", "Theater" = "theater"), selected = "art"))
            )
    ),
    tabItem(tabName = "tab3",
            fluidRow(
              box(plotlyOutput("plot3_bar"), width = 12),
              box(plotlyOutput("plot3_line"), width = 12)
            )
    ),
    tabItem(tabName = "tab5",
            fluidRow(
              box(plotlyOutput("plot5_goal"), width = 12),
              box(plotlyOutput("plot5_pledged"), width = 12)
            )
    ),
    tabItem(tabName = "tab6",
            fluidRow(
              box(plotlyOutput("plot6b"), width = 12),
              box(plotlyOutput("plot6"), width = 12)
            )
    ),
    tabItem(tabName = "tab7",
            fluidRow(
              box(plotlyOutput("plot7b"), width = 12),
              box(plotlyOutput("plot7"), width = 12)
            )
    ),
    tabItem(tabName = "intro",
            fluidRow(
              box(title = "About Kickstarter Project",
                  img(src='https://upload.wikimedia.org/wikipedia/commons/thumb/b/b5/Kickstarter_logo.svg/512px-Kickstarter_logo.svg.png'),
                  p("\n"),
                  p("\n"),
                  p("This is a project on analyzing past records of projects launched on Kickstarter."),
                  p("Produced by HKU STAT3622 Fall 2018 Nick, Sunny and Tyson, under the teaching of Dr. AijunZheng.\n\n"),
                  width = 12
              )
            )
    ))
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

#server

server <- function(input, output) {
  
  # plot1
  selected_aggregator <- reactive({as.character(input$aggregator)})
  freq_table <- reactive({as.data.frame(table(df2[selected_aggregator()]))})
  output$plot1 <- renderPlotly({
    plot_ly(x = freq_table()$Var1, y = freq_table()$Freq,
            type = "bar", color = freq_table()$Var1) %>% layout(title = "Kickstarter Projects")
  })

  # plot2
  selected_cat <- reactive({selected_cat <- as.character(input$cat)})
  
  plot2_df_sf_filtered <- reactive({plot2_df_sf_filtered <- dplyr::filter(df_sf, main_category == selected_cat())})
  plot2_df_s_filtered <- reactive({plot2_df_s_filtered <- dplyr::filter(df_s, main_category == selected_cat())})
  plot2_df_f_filtered <- reactive({plot2_df_f_filtered <- dplyr::filter(df_f, main_category == selected_cat())})
  
  launch_at_year_month_freq_total <- as.data.frame(table(df_sf$launch_at_year_month))
  launch_at_year_month_freq_total_s <- as.data.frame(table(df_s$launch_at_year_month))
  launch_at_year_month_freq_total_f <- as.data.frame(table(df_f$launch_at_year_month))
  
  launch_at_year_month_freq <- reactive({as.data.frame(table(plot2_df_sf_filtered()$launch_at_year_month))})
  launch_at_year_month_freq_s <- reactive({as.data.frame(table(plot2_df_s_filtered()$launch_at_year_month))})
  launch_at_year_month_freq_f <- reactive({as.data.frame(table(plot2_df_f_filtered()$launch_at_year_month))})
  
  output$plot2 <- renderPlotly({
    plot_ly(x = launch_at_year_month_freq_total$Var1, y = launch_at_year_month_freq_total$Freq, type = "scatter", mode = "lines", name = "All projects", line = list(color = "rgb(46,137,130)")) %>% add_trace(y = launch_at_year_month_freq_total_f$Freq, name = "All failed projects", line = list(color = "rgb(234,188,163,0.6)")) %>% add_trace(y = launch_at_year_month_freq_total_s$Freq, name = "All successful projects", line = list(color = "rgb(159,253,198,0.6)")) %>% layout(title = "Number of Kickstarter projects across all categories", xaxis = list(rangeslider = list(type = "date"))) # %>% layout(xaxis = list(range = selected_dates()))
  })
  
  output$plot2b <- renderPlotly({
    plot_ly(x = launch_at_year_month_freq()$Var1, y = launch_at_year_month_freq()$Freq, type = "scatter", mode = "lines", name = "Category projects", line = list(color = "rgb(46,137,130)")) %>% add_trace(y = launch_at_year_month_freq_f()$Freq, name = "Failure", line = list(color = "rgb(234,188,163,0.6)")) %>% add_trace(y = launch_at_year_month_freq_s()$Freq, name = "Successful", line = list(color = "rgb(159,253,198,0.6)")) %>% layout(title = "Number of Kickstarter projects by categories", xaxis = list(rangeslider = list(type = "date")))# %>% layout(xaxis = list(range = selected_dates()))
  })
  
  # plot3
  
  main_category_freq_all <- as.data.frame(table(df2$main_category))
  main_category_freq_s <- as.data.frame(table(df_s$main_category))
  main_category_freq_f <- as.data.frame(table(df_f$main_category))
  
  srate <- data.frame(rate = main_category_freq_s$Freq/main_category_freq_all$Freq)
  frate <- data.frame(rate = main_category_freq_f$Freq/main_category_freq_all$Freq)
  orate <- data.frame(rate = 1-srate$rate-frate$rate)
  
  rates <- data.frame(main_category = main_category_freq_all$Var1, s = srate$rate, f = frate$rate, o = orate$rate)
  overall_rate <- data.frame(main_category = "overall", s = nrow(df_s)/nrow(df2), f = nrow(df_f)/nrow(df2), o = 1-nrow(df_s)/nrow(df2)-nrow(df_f)/nrow(df2))
  rates <- rbind(rates, overall_rate)
  
  output$plot3_bar <- renderPlotly({
    plot_ly(rates, x = rates$s , y = rates$main_category, type = "bar", orientation = "h", name = "Successful",
            marker = list(color = 'rgba(68, 156, 83, 0.8)',
                          line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
      add_trace(x = rates$f, name = "Failed", marker = list(color = 'rgba(159, 40, 78, 0.8)')) %>%
      add_trace(x = rates$o, name = "Live/Suspended/Canceled", marker = list(color = 'rgba(244, 231, 99, 0.8)')) %>%
      layout(barmode = "stack", title = "Success rate by category")
  })
  
  launch_at_year_month_freq_total <- as.data.frame(table(df_sf$launch_at_year_month))
  launch_at_year_month_freq_total_s <- as.data.frame(table(df_s$launch_at_year_month))
  
  output$plot3_line <- renderPlotly({
    plot_ly(x = launch_at_year_month_freq_total$Var1, y = launch_at_year_month_freq_total_s$Freq/launch_at_year_month_freq_total$Freq, type = "scatter", mode = "lines+markers", color = list(color = "rgb(121,126,138)"), line = list(color = "rgb(152,158,173)")) %>% layout(title = "Historical successful rate", xaxis = list(rangeslider = list(type = "date")))
  })
  
  # plot5
  df_goal_non_zero <- filter(df_sf, usd_goal >=50 )
  df_goal_non_zero$usd_goal <- log(df_goal_non_zero$usd_goal, 10)
  df_pledged_non_zero <- filter(df_sf, usd_pledged != 0)
  df_pledged_non_zero$usd_pledged <- log(df_pledged_non_zero$usd_pledged, 10)
  
  output$plot5_goal <- renderPlotly({
    plot_ly(x = df_goal_non_zero$usd_goal, type = "histogram", name = "Goal in USD", alpha = 0.6, nbinsx = 65) %>% 
      layout(barmode = "overlay", title = "Project Goal Distribution in USD", xaxis = list(type = "log"))
  })
  
  output$plot5_pledged <- renderPlotly({
    plot_ly(x = df_pledged_non_zero$usd_pledged, type = "histogram", name = "Pledged amount in USD", alpha = 0.6, nbinsx = 60) %>%
    layout(barmode = "overlay", title = "Pledged Amount Distribution in USD")
  })
  
  # plot6
  plot6_freq_table <- df_sf %>% group_by(main_category) %>% dplyr::summarise(per_backer_mean = mean(usd_pledged_per_backer, na.rm=TRUE))
  output$plot6 <- renderPlotly({
    plot_ly(x = plot6_freq_table$main_category, y = plot6_freq_table$per_backer_mean,
            type = "bar", color = plot6_freq_table$main_category) %>% layout(title = "Average amount pledged per backer")
  })

  usd_pledged_agg_month <- df_sf %>% group_by(launch_at_year_month) %>% dplyr::summarise(usd_pledged_mean = mean(usd_pledged_per_backer, na.rm=TRUE))
  output$plot6b <- renderPlotly({
    plot_ly(x = usd_pledged_agg_month$launch_at_year_month, y = usd_pledged_agg_month$usd_pledged_mean, type = "scatter", mode = "lines+markers", color = list(color = "rgb(121,126,138)"), line = list(color = "rgb(152,158,173)")) %>% layout(title = "Average amount pledged over time", xaxis = list(rangeslider = list(type = "date")))
  })
  
  # plot7
  period_freq_s <- filter(as.data.frame(table(df2$days, df2$state)), Var2 == 'successful')
  period_freq_all <- as.data.frame(table(df2$days))
  period_srate <- data.frame(days = period_freq_all$Var1, rate = period_freq_s$Freq / period_freq_all$Freq)
  
  output$plot7 <- renderPlotly({
    plot_ly(x = period_srate$days, y = period_srate$rate, type = "scatter", mode = "lines+markers", color = list(color = "rgb(121,126,138)"), line = list(color = "rgb(152,158,173)")) %>% layout(title = "Success rates and period")
  })
  
  plot7b_sep <- c(-Inf, 10000, 20000, 30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,1700000,180000,190000,200000,210000,220000,230000,240000,250000,Inf)
  plot7b_lab <- c(0, 10000, 20000, 30000,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,150000,160000,1700000,180000,190000,200000,210000,220000,230000,240000,250000)
  plot7b_df <- df2 %>% mutate(cat = cut(usd_goal, breaks = plot7b_sep, labels = plot7b_lab))
  
  plot7b_freq_s <- filter(as.data.frame(table(plot7b_df$cat, plot7b_df$state)), Var2 == 'successful')
  plot7b_freq_all <- as.data.frame(table(plot7b_df$cat))
  plot7b_srate <- data.frame(cat = plot7b_freq_all$Var1, rate = plot7b_freq_s$Freq / plot7b_freq_all$Freq)
  
  output$plot7b <- renderPlotly({
    plot_ly(x = plot7b_srate$cat, y = plot7b_srate$rate, type = "scatter", mode = "lines+markers", color = list(color = "rgb(121,126,138)"), line = list(color = "rgb(152,158,173)")) %>% layout(title = "Success rates and project goal")
  })
}

shinyApp(ui, server)

