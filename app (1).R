#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(DT)
library(purrr)
library(shinyWidgets)

demographicsRow <- fluidRow(
  box(
    title = "Strategic Deal Demographics",
    solidHeader = TRUE,
    background = "yellow",
    width = 12,
    textInput("merchantName",
              label = "Mechant Legal Name",
              placeholder = 'Merchant Name'),
    textInput(
      "nYearsOfContractTerm",
      label = "Contract Term (years)",
      placeholder = 'No. Of Years'
    ),
    textInput(
      "noOfContractLocations",
      label = "Number of Contract Locations",
      placeholder = 'No. Of Locations'
    ),
    textInput(
      "noOfFranchiseLocations",
      label = "Number of Franchise Locations",
      placeholder = 'No. Of Locations'
    ),
    selectInput(
      "fundTransferType",
      label = "Funds Transfer Type",
      choices = c('ACH', 'Wire', 'Both')
    )
  )
)

standardInputs <- box(
  title = "",
  solidHeader = FALSE,
  color = "black",
  background = "green",
  width = 12,
  DT::dataTableOutput("standardInput")
)

volumeInputs <- box(
  title = "",
  solidHeader = FALSE,
  color = "black",
  background = "green",
  width = 12,
  DT::dataTableOutput("volumeInput")
)
pricingInputs <- box(
  title = "",
  solidHeader = FALSE,
  color = "black",
  background = "green",
  width = 12,
  DT::dataTableOutput("pricingInput")
)

# debugPanel <- fluidRow(
#

instruments <- list(
  "Credit"    = c(
    "",
    "Visa" = "VS",
    "Mastercard" = "MC",
    "AMEX Settled (Opt Blue)" = "AMXSOB",
    "AMEX Conveyed" = "AMXCVYD",
    "Discover/JCB/Diners Settled" = "DSCSTLD",
    "Cross Currency" = "CC",
    "Discover Conveyed" = "DCSCVYD",
    "Purchasing Card Level III" = "PCIII",
    "Net Connect" = "NC"
  ),
  "PIN Debit" = c("", "w/Hosted Pay Page")
)

productCapabilities <- fluidRow(
  box(
    title = "Product and Capabilities",
    solidHeader = TRUE,
    background = "yellow",
    width = 12,
    fluidRow(
      column(
        width = 2,
        textInput(
          "noOfTiers",
          label = "Tiers #",
          value = "5",
          placeholder = 'No Of Tiers'
        )
      ),
      column(
        width = 6,
        selectInput(
          "tierMethod",
          label = "Methods",
          choices = c(
            "Number Of Transactions" = "noOfTransactions",
            "Dollar Value Of Transactions" = "dvOfTransactions"
          )
        )
      ),
      column(
        width = 4,
        textInput("valueOfTierMethods", label = "Amount", value = "200,000")
      )
    ),
    fluidRow(column(
      width = 4,
      selectInput(
        "capabilities",
        label = "Capabilities",
        choices =  instruments,
        multiple = TRUE
      )
    ))
  )
)



layoutXX <- fluidRow(column(width = 12,
                            fluidRow (
                              column(width = 4,
                                     demographicsRow,
                                     productCapabilities),
                              column(width = 8,
                                     fluidRow(standardInputs),
                                     fluidRow(volumeInputs),
                                     fluidRow(pricingInputs))
                            )))


mainBody <- dashboardBody(useShinyjs(),
                          layoutXX,
                          fluidRow(htmlOutput("debugMe")))


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = TRUE),
  mainBody,
  title = "Pricing Calculator",
  skin = "red"
)

allCapabilities <-
  c(
    "Auth to Capture Ratio",
    "Return Ratio",
    "Chargeback Ratio",
    "Retrieval Ratio",
    "Representment Ratio",
    "Voice Auth Ratio"
  )

creditCapabilities <- list(
  "all" = c(
    "Credit Authorization Fee",
    "Credit Deposit Fees",
    "Credit Deposit Fees (bps)",
    "Voice Authorization Fee",
    "Chargeback Fee",
    "Representment Fee",
    "Collection, Pre-Arbitration & Compliance Fee"
  ),
  "CC" = c("Cross Currency Markup"),
  "purchasing-lvl3" = c("Purchasing Card Level 3 Transaction Fee"),
  "net-connect" = c("NetConnect Fees", "NetConnect Batch Monthly Fees")
)

volumeCapabilities <- list(
  "VS" = c("Proposed Gross Visa Volume",
           "Proposed Gross Visa Txns"),
  "MC" = c(
    "Proposed Gross Mastercard Volume",
    "Proposed Gross Mastercard Txns"
  ),
  "AMXSOB"  = c("Proposed Gross AMEX Transactions"),
  "AMXCVYD" = c("Proposed Gross AMEX Conveyed Transactions"),
  "DSCSTLD" = c("Proposed Discover/JCB/Dinners Settled Transactions"),
  "CC" = c("% Cross Currency Volume")
)


map <-
  list(
    "VS"      = "Credit",
    "MC"      = "Credit",
    "AMXSOB"  = "Credit",
    "AMXCVYD" = "Credit",
    "DSCSTLD" = "Credit",
    "CC"      = "Credit",
    "PP"      = "Dedit"
  )

tieredDataTable <-
  function (tiers,
            estimates,
            capabilities,
            rownameHeader) {
    buckets <-  seq(0, estimates, by = estimates / tiers)
    bucket_range <- rep("", length(buckets))
    for (i in seq_along(buckets)) {
      if (buckets[i] == 0)
        bucket_range[i] = ""
      else
        bucket_range[i] = paste(buckets[i - 1], "-", as.integer(buckets[i]))
    }
    colNames <- map_chr(seq_along(buckets), function (v)
      ifelse(v == 1, "Benchmark", paste("Tier", sep = "", v - 1)))
    
    colNames <- c("", colNames)
    bucket_range <- c(rownameHeader, bucket_range)
    sketch = htmltools::withTags(table(class = 'display',
                                       thead(tr(
                                         lapply(colNames, th)
                                       ),
                                       tr(
                                         lapply(bucket_range, th)
                                       ))))
    m <-
      matrix(ncol = tiers + 1, # of Tiers + Benchmark + Row Names
             nrow = length(capabilities))
    datatable(
      data = m,
      container = sketch,
      rownames = capabilities,
      colnames = colNames,
      fillContainer = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE
      ),
      class = 'cell-border stripe',
      editable = TRUE
    ) %>% formatStyle(
      0,
      color = 'red',
      backgroundColor = 'orange',
      fontWeight = 'bold'
    )
    
  }

# Define server logic required to draw a histogram
server <- function(input, output) {
  nTiers <- reactive({
    as.integer(input$noOfTiers)
  })
  
  projectedTransactions <- reactive({
    value  <- input$valueOfTierMethods
    method <- input$tierMethod
    as.double(gsub(",", "", value))
  })
  
  selectedPricingCapabilities <- reactive({
    input$capabilities
  })
  
  output$debugMe <- renderText({
    op <- paste("<b> No Of Tiers:", nTiers(), "<br> <br>")
    op <-
      paste(op,
            "<b>Project Transactions:",
            projectedTransactions(),
            "<br>")
    op <-
      paste(op,
            "<b>Selected Capabilities:",
            selectedPricingCapabilities(),
            "<br>")
    ""
  })
  
  output$standardInput <- renderDT({
    tieredDataTable(nTiers(),
                    projectedTransactions(),
                    allCapabilities,
                    "Standard Inputs")
  })
  
  output$volumeInput <- renderDT({
    selectedCapabilities <- input$capabilities
    tieredDataTable(
      nTiers(),
      projectedTransactions(),
      unlist(volumeCapabilities[selectedCapabilities]),
      "Volume Section"
    )
  })
  
  output$pricingInput <- renderDT({
    selectedCapabilities <- input$capabilities
    tieredDataTable(
      nTiers(),
      projectedTransactions(),
      unlist(creditCapabilities["all"]),
      "Pricing Section"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
