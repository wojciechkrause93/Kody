library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(devtools)
library(Hmisc)
baza = EuStockMarkets

header = dashboardHeader(title = "Dashboard", dropdownMenu(type = "messages",
                                                           messageItem(from = "Yogi Corporation",
                                                                       message = "Wojciech Krause"),
                                                           messageItem(from = "Zaliczenie Przedmiotu",
                                                                       message = "Na 5 :)")),
                         dropdownMenu(type = "notifications",
                                      notificationItem(text = "Zaliczenie",
                                                       icon = shiny::icon("warning"))))

sider = dashboardSidebar(sidebarMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                     menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                                              badgeLabel = "new",badgeColor = "green")))
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(
  header = header,
  sidebar = sider,
  body = body
)

server <- function(input, output) {}
shinyApp(ui, server)

