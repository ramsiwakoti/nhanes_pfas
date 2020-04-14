library(shiny)
library(dplyr)
library(ggplot2)
library(xlsx)

bcl <- read.xlsx("pfoapfas_gm.xlsx", sheetName = "Sheet1")
bcl <- bcl %>% 
  select(Cycle, Age.group, sample.size, Est, LB, UB, chemical) %>% 
  mutate(year = dplyr::recode(Cycle, 
                              `1999-2000` = "1999.5",
                              `2003-2004` = "2003.5",
                              `2005-2006` = "2005.6",
                              `2007-2008` = "2007.5",
                              `2009-2010` = "2009.5",
                              `2011-2012` = "2011.5",
                              `2013-2014` = "2013.5",
                              `2015-2016` = "2015.5"))
colnames(bcl) <- c("Survey Cycle", "Age", "n", "GM", "GM LB", "GM UB", "Chemical", "year")
bcl$year <- as.numeric(as.character(bcl$year))

ui <- fluidPage(
    titlePanel("PFAS chemical GM estimates"),
    sidebarLayout(
      sidebarPanel(
      # selectInput("Cycle", "Choose a survey year:", c("1999-2000", "2003-2004", "2005-2006", "2007-2008",
      #                                             "2009-2010", "2011-2012", "2013-2014", "2015-2016")), 
      selectInput("Chemical", "Choose a PFAS chemical:", c("PFOA", "PFOS")),
      # selectInput("Estimate", "Choose an estimate:", c("GM", "50th", "75th", "90th", "95th")),
      selectInput("age", "Choose age group:", c("12-19", "20-39", "40-59", ">= 60"))
      ),
      mainPanel(
        plotOutput("coolplot"),
        br(), br(), br(),
        tableOutput("results")
        )
      )
    )

server <- function(input, output) {
  output$coolplot <- renderPlot({
    filtered <- 
      bcl %>%
      filter(Chemical == input$Chemical,
             Age == input$age) 
     
     ggplot(filtered, aes(x = year, y = GM)) + geom_point() +
     stat_smooth(se = FALSE, method = "nls", formula = y ~ exp(a + b*x), 
                 method.args = list(start = list(a = 171, b = -0.08))) +
      geom_linerange(aes(ymin = `GM LB`, ymax = `GM UB`)) +
      ylab("GM") + xlab("NHANES Cycle") +
      ggtitle(" GM estimates trend from NHANES")
    
})
  output$results <- renderTable({
    filtered <- 
      bcl %>%
      filter(Chemical == input$Chemical,
             Age == input$age) %>%
      select(-c(year))
    filtered
  })
}
shinyApp(ui = ui, server = server)
