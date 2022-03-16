library(shiny);library(data.table);library(DT);library(jstable);library(shinycustomloader)
library(jsmodule)
options(shiny.sanitize.errors = F, shiny.maxRequestSize = 5*1024^2)
nfactor.limit <- 20

source("global.R")

# Define UI for application that draws a histogram
ui <- navbarPage("Polypectomy study",
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons('dt', "Data 선택", c("Data1 - 분석 1,2,3을 위한", "Data2 - 분석 4를 위한"), inline = F)
                              ),
                            mainPanel(
                              withLoader(DTOutput("data"), type="html", loader="loader6")
                            )
                          )
                 ),
                 tabPanel("Baseline characteristics", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 tabPanel("Logistic regression", icon = icon("list-alt"),
                          sidebarLayout(
                            sidebarPanel(
                              regressModuleUI("logistic")
                            ),
                            mainPanel(
                              withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                            )
                          )
                 )
)

# Define server logic 
server <- function(input, output, session) {
  data <- reactive({
    switch(input$dt, 
           "Data1 - 분석 1,2,3을 위한" = dt1,
           "Data2 - 분석 4를 위한" = dt2)
  })
  
  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
              options = c(jstable::opt.data("data"), list(scrollX = TRUE))
    )
  })

  data.label <- reactive(jstable::mk.lev(data()))
  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL)
  
  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
    out.tb1 <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
    return(out.tb1)
  })
  
  
  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
                                    list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                    ),
                                    list(scrollX = TRUE)
                        )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
  
  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table, rownames=T, extensions= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
