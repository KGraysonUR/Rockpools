library(shiny)
library(serenity.viz)

# Define UI for application that draws a histogram
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(magrittr)
library(dplyr)
library(forcats)
library(DT)
library(readr)
library(bsplus)
library(colourpicker)

# HTML(markdown::markdownToHTML(knitr::knit(system.file("vignettes/teaching_module.Rmd", package = "tusklessness"), quiet = TRUE)))
teaching_mods <- c("teaching_module_header.md, Exercise1.md, Exercise2.md")
metadata_mods <- c("metadata.md")

load("data/dfmorph.rda")

# Data cleaning
dataset <- dfmorph

attr(dataset, "df_name") <- "dfmorph"

dataset_all <- colnames(dataset)
dataset_num <- dataset_all[sapply(dataset, is.numeric)]
dataset_cat <- dataset_all[-which(dataset_all %in% dataset_num)] # setdiff?

ui <- dashboardPagePlus(
  dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "Rock Pools"),
      img(src = "")
    )
  ),
  # Dashboard Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Module", tabName = "module", icon = icon("chalkboard-teacher")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Explore", tabName = "explore", icon = icon("chart-area")),
      menuItem("Analyze",
               tabName = "analyze",
               icon = icon("calculator"),
               menuSubItem("Statistical Summaries",
                           tabName = "summaries"),
               menuSubItem("Hypothesis Tests",
                           tabName = "tests")
      )
    ),
    hr(),
    conditionalPanel("input.sidebar == 'module'",
                     downloadButton('download', 'Download Report')
    )
  ),
  # Dashboard Body ----
  dashboardBody(
    tabItems(
      tabItem(tabName = "module",
              includeMarkdown("www/teaching_module_header.md"),
              textAreaInput("title", "Title", width="600px", rows = 1, placeholder = "Put your title here"),
              textAreaInput("authors", "Authors", width="600px", rows = 2, placeholder = "Put author names here"),
              includeMarkdown("www/Exercise1.md"),
              textInput("dataset_name", "Dataset name", width="600px"),
              numericInput("num_records", "Number of records", value = 0, min = 0, step = 1),
              numericInput("num_columns", "Number of columns", value = 0, min = 0, step = 1),
              textAreaInput("observation", "Share an oberservation or question about the dataset", width="600px", rows = 4, placeholder = "Observation or question"),
              includeMarkdown("www/Exercise2.md"),
              textAreaInput("plot_response", NULL, width="600px", rows = 4, placeholder = "Please replace this text with a brief interpretive statement about some of the information in the graph above."),
              includeMarkdown("www/Exercise2b.md"),
              actionButton("getPlot", "Grab Plot"),
              plotOutput("myplot", width = "50%"),
              textAreaInput("myplot_response", NULL, width="600px", rows = 4, placeholder = "Replace this text with a brief description of your graph and a question that it has helped you formulate.")
              # includeMarkdown("www/Exercise3.md"),
              # textAreaInput("mytable_response", NULL, width="600px", rows = 4, placeholder = "Replace this text with your data manipulation code and visualization(s) - be sure to include a brief interpretive description for each graph.")
      ),
      tabItem(tabName = "data",
              tabBox(
                width = "100%",
                tabPanel("Table", DTOutput('table')),
                tabPanel("Metadata", includeMarkdown("www/metadata.md"))
              )
      ),
      tabItem(tabName = "explore",
              serenityVizUI(id = "explore", dataset = dataset, showcode = FALSE, height="90%")
      ),
      tabItem(tabName = "summaries",
              box(
                title = "Inputs",
                status = "primary",
                solidHeader = TRUE,
                width = 3,
                selectizeInput(
                  "variable",
                  "Compute statistics for:",
                  choices = dataset_num,
                  multiple = TRUE,
                  options = list(
                    'plugins' = list('remove_button'),
                    'create' = TRUE,
                    'persist' = FALSE
                  )
                ),
                selectizeInput(
                  "groupby",
                  "Group by:",
                  choices = dataset_cat,
                  multiple = TRUE,
                  options = list(
                    'plugins' = list('remove_button',
                                     'drag_drop'),
                    'create' = TRUE,
                    'persist' = FALSE
                  )
                ),
                selectizeInput(
                  "statistics",
                  "Statistics:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    'plugins' = list('remove_button',
                                     'drag_drop'),
                    'create' = TRUE,
                    'persist' = FALSE
                  )
                ),
                numericInput("sigdigs",
                             "Decimals:",
                             3,
                             min = 0,
                             max = 10,
                             step = 1)
              ),
              box(title = "Table",
                  status = "info",
                  solidHeader = TRUE,
                  width = 9,
                  DTOutput('statsummary')
              )
      ),
      tabItem(tabName = "tests",
              box(
                title = "Inputs",
                status = "primary",
                solidHeader = TRUE,
                width = 3,
                selectInput(
                  "tests",
                  "Please select a test:",
                  choices = c("One-Sample T-Test" = "onesample",
                              "Two-Sample T-Test" = "twosample",
                              "Linear Regression" = "regression")
                ),
                conditionalPanel(condition = "input.tests == 'onesample'",
                                 selectInput(
                                   "onesample_var",
                                   "Variable:",
                                   choices = dataset_num
                                 ),
                                 numericInput(
                                   "onesample_null",
                                   "Null Value:",
                                   value = 0
                                 ),
                                 selectInput(
                                   "onesample_side",
                                   "One or Two Sided:",
                                   choices = c("Two sided" = "two.sided",
                                               "One sided: Less" = "less",
                                               "One sided: Greater" = "greater")
                                 ),
                                 sliderInput(
                                   "onesample_alpha",
                                   "Significance Level:",
                                   value = 0.05,
                                   min = 0.01, max = 1, step = 0.01
                                 )
                ),
                conditionalPanel(condition = "input.tests == 'twosample'",
                                 selectInput(
                                   "twosample_exp",
                                   "Explanatory Variable:",
                                   choices = dataset_cat
                                 ),
                                 selectInput(
                                   "twosample_res",
                                   "Response Variable:",
                                   choices = dataset_num
                                 ),
                                 numericInput(
                                   "twosample_null",
                                   "Null Value:",
                                   value = 0
                                 ),
                                 selectInput(
                                   "twosample_side",
                                   "One or Two Sided:",
                                   choices = c("Two sided" = "two.sided",
                                               "One sided: Less" = "less",
                                               "One sided: Greater" = "greater")
                                 ),
                                 sliderInput(
                                   "twosample_alpha",
                                   "Significance Level:",
                                   value = 0.05,
                                   min = 0.01, max = 1, step = 0.01
                                 )
                ),
                conditionalPanel(condition = "input.tests == 'regression'",
                                 selectInput(
                                   "regression_exp",
                                   "Explanatory Variable:",
                                   choices = dataset_num
                                 ),
                                 selectInput(
                                   "regression_res",
                                   "Response Variable:",
                                   choices = dataset_num,
                                   selected = dataset_num[2]
                                 )
                )
              ),
              box(title = "Visualization",
                  status = "info",
                  solidHeader = TRUE,
                  width = 5,
                  plotOutput("tests_plot")),
              box(title = "Results",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  verbatimTextOutput("tests_results")
              )
      )
    )
  ),
  tags$head(includeCSS("www/app.css"))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  explore_plot <- callModule(module = serenityVizServer,
                             id = "explore",
                             dataset = dataset)

  plots <- reactiveValues(myplot_text = NULL)

  updateSelectizeInput(session, 'statistics', 
                       choices = list(
                         Center = c("Mean" = "mean",
                                    "Median" = "median"),
                         Spread = c("Standard Deviation" = "sd",
                                    "IQR" = "IQR"),
                         Range = c("Min" = "min",
                                   "Max" = "max",
                                   "1st Qu." = "firstquartile",
                                   "3rd Qu." = "thirdquartile"),
                         Count = c("Number" = "n")
                       )
  )
  
  observeEvent(input$getPlot, {
    plots$myplot_text <- explore_plot()
  }, ignoreInit = TRUE)
  
  output$myplot <- renderPlot({
    req(plots$myplot_text)
    eval(parse(text=plots$myplot_text))
  })
  
  # Main Data Table ----
  output$table <- renderDT(
    dataset,
    filter = "top",
    rownames = FALSE,
    style = "bootstrap",
    selection = "none",
    options = list(scrollX = 400)
    )

  # Statistical Tests ----
  onesample <- reactive({
    t.test(dataset[[input$onesample_var]],
           alternative = input$onesample_side,
           mu = input$onesample_null,
           conf.level = 1-input$onesample_alpha)
  })

  twosample <- reactive({
    t.test(dataset[[input$twosample_res]] ~ dataset[[input$twosample_exp]],
           alternative = input$twosample_side,
           mu = input$twosample_null,
           conf.level = 1-input$twosample_alpha)
  })

  regression <- reactive({
    lm(dataset[[input$regression_res]] ~ dataset[[input$regression_exp]])
  })

  onesample_plot <- reactive({
    dataset %>%
      ggplot(aes(x = !!sym(input$onesample_var))) +
      geom_histogram(colour = "black", fill = "gray72") +
      geom_errorbarh(aes(xmin = onesample()$conf.int[1],
                         xmax = onesample()$conf.int[2],
                         y = 0),
                     colour = "blue",
                     size = 1.2,
                     height=2) +
      geom_point(x = onesample()$estimate,
                 y = 0,
                 size = 2,
                 colour = "green") +
      geom_point(x = input$onesample_null,
                 y = 0,
                 size = 2,
                 colour = "red") +
      expand_limits(x = input$onesample_null)
  })

  twosample_plot <- reactive({
    req(input$twosample_exp, input$twosample_res)
    conf_data <- dataset %>%
      group_by(!!sym(input$twosample_exp)) %>%
      summarize(n = n(),
                mu = mean(!!sym(input$twosample_res), na.rm=T),
                s = sd(!!sym(input$twosample_res), na.rm=T),
                se = s/sqrt(n),
                tcrit = qt(p = 1-(input$twosample_alpha/2), df = n-1),
                low = mu - tcrit*se,
                high = mu + tcrit*se)
    dataset %>%
      ggplot(aes(x = !!sym(input$twosample_exp),
                 y = !!sym(input$twosample_res))) +
      geom_point(position = position_jitter(0.1),
                 alpha = 0.5) +
      geom_errorbar(aes(y = NULL, ymin = low, ymax = high),
                    position = position_nudge(0.2),
                    width = 0.05,
                    size = 1.2,
                    colour = "blue",
                    data = conf_data) +
      geom_point(aes(y = mu),
                 position = position_nudge(0.2),
                 size = 2,
                 colour = "green",
                 data = conf_data)
  })

  regression_plot <- reactive({
    dataset %>%
      ggplot(aes(x = !!sym(input$regression_exp),
                 y = !!sym(input$regression_res))) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm")
  })

  output$tests_plot <- renderPlot({
    if (input$tests == "onesample") {
      onesample_plot()
    } else
      if (input$tests == "twosample") {
        twosample_plot()
      } else
        if (input$tests == "regression") {
          regression_plot()
        }
  })

  output$tests_results <- renderPrint({
    if (input$tests == "onesample") {
      out <- capture.output(onesample())
      out[-4] %>%
        { append(., values = "", after = 4) } %>%
        { append(., values = "", after = 6) } %>%
        { append(., values = "", after = 9) } %>%
        { gsub("mean of x", paste("mean of", input$twosample_res), .) } %>%
        { cat(., sep = "\n") }
    } else
      if (input$tests == "twosample") {
        out <- capture.output(twosample())
        out[-4] %>%
        { append(., values = "", after = 4) } %>%
        { append(., values = "", after = 6) } %>%
        { append(., values = "", after = 9) } %>%
        { cat(., sep = "\n") }
      } else
        if (input$tests == "regression") {
          out <- capture.output(summary(regression()))
          loc <- stringr::str_locate(out[10], "Estimate")[1]
          out[12] <- stringr::str_c(input$regression_exp, "  ", stringr::str_sub(out[12], loc))
          newloc <- stringr::str_length(input$regression_exp)+2
          out[11] <- stringr::str_c(
            "(Intercept)",
            stringr::str_dup(" ", newloc - stringr::str_length("(Intercept)")),
            stringr::str_sub(out[11], loc)
          )
          out[10] <- stringr::str_c(
            stringr::str_dup(" ", newloc),
            stringr::str_sub(out[10], loc)
          )

          out[-(1:8)] %>%
            { append(., values = out[5:8], after = 7) } %>%
            { cat(., sep = "\n") }
        }
  })

  # Summary statistics ----
  statsummary <- reactive({
    req(input$statistics)
    # tmp <- elephant %>% group_by(!!sym(groupby)) %>% summarize_at(vars(variable), list(Q1 = ~quantile(., probs=0.25), Q3 = ~quantile(., probs=0.75)), na.rm=TRUE)
    funmap <- list(min = min,
                   firstquartile = ~quantile(., 0.25),
                   median = median,
                   mean = mean,
                   thirdquartile = ~quantile(., 0.75),
                   max = max,
                   sd = sd,
                   IQR = IQR)
    countmap <- list(n = quo(n()))
    funmap <- dropNulls(funmap[input$statistics])
    countmap <- dropNulls(countmap[input$statistics])

    if (length(funmap) > 0) {
      validate(
        need(isTruthy(input$variable), "Please select at least one variable.")
      )
    }
    
    # Gotta be a cleaner way then using an if statement here...
    if (!is.null(input$groupby)) {
      statsum <- dataset %>% dplyr::group_by(!!!syms(input$groupby))
    } else {
      statsum <- dataset
    }
    
    if (!is.null(input$variable)) {
      if (length(countmap) > 0) {
        statsum <- statsum %>% mutate(!!!countmap)
        
        if (!is.null(input$groupby)) {
          statsum <- statsum %>% group_by_(names(countmap), add = TRUE)
        }
      }
      
      statsum <- statsum %>%
        dplyr::summarize_at(vars(input$variable), funmap, na.rm=TRUE)
    } else 
      if (length(countmap) > 0) {
        statsum <- statsum %>% summarize(!!!countmap)
      }

    if (length(input$variable) > 1) {
      statsum <- statsum %>%
        tidyr::gather_(key = "var_fun", value = "value", setdiff(names(.), input$groupby)) %>%
        tidyr::separate(var_fun, into=c("variable", "fun"), sep = "_") %>%
        tidyr::spread(fun, value) %>%
        dplyr::select(!!!syms(input$groupby), "variable", input$statistics) # Needed to keep ordering
    }

    statsum
  })

  DTstatsummary <- reactive({
    # In case you want to control for # decimal digits in future:
    # https://groups.google.com/forum/#!topic/shiny-discuss/2jlYOYFp2-A

    # See radiant.data dtab.explore
    cn_all <- colnames(statsummary())
    cn_num <- cn_all[sapply(statsummary(), is.numeric)]
    cn_cat <- cn_all[-which(cn_all %in% cn_num)]

    DT::datatable(
      statsummary(),
      filter = "top",
      rownames = FALSE,
      style = "bootstrap",
      selection = "none",
      options = list(
        dom = "t",
        searchCols = NULL,
        paging = FALSE
      )
    ) %>% DT::formatStyle(cn_cat,
                          color = "white",
                          backgroundColor = "grey") %>%
      formatRound(columns=cn_num, digits=input$sigdigs)
  })

  output$statsummary <- DT::renderDataTable({
    DTstatsummary()
  })

  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$download <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "myreport.html"
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      tmp <- tempfile(fileext = ".Rmd")
      file.copy("www/Exercise2_files/figure-markdown_strict/unnamed-chunk-1-1.png", tempdir())
      content <- read_file("www/teaching_module_header.md")
      content <- paste(c(content, paste('###', input$title)), collapse = "\n")
      content <- paste(c(content, paste('####', input$authors)), collapse = "\n")
      content <- paste(c(content, read_file("www/Exercise1.md")), collapse = "\n")
      content <- paste(c(content, paste('Dataset name:', input$dataset_name)), collapse = "\n")
      content <- paste(c(content, paste('<br>Number of records:', input$num_records)), collapse = "\n")
      content <- paste(c(content, paste('<br>Number of columns:', input$num_columns)), collapse = "\n")
      content <- paste(c(content, paste('<br>Observation or question:', input$observation)), collapse = "\n")
      content <- paste(c(content, gsub("Exercise2_files/figure-markdown_strict/", "", read_file("www/Exercise2.md"))), collapse = "\n")
      content <- paste(c(content, paste('<br>Description of Plot:', input$plot_response)), collapse = "\n")
      content <- paste(c(content, read_file("www/Exercise2b.md")), collapse = "\n")
      
      # Get plot
      if (!is.null(plots$myplot_text)) {
        plotrmd <- paste(c("\n```{r, echo=FALSE, warning=FALSE}", paste(plots$myplot_text, collapse = "\n"), "```\n"), collapse = "\n")
        content <- paste(c(content, plotrmd), collapse = "\n")
      }

      content <- paste(c(content, paste('<br>Plot discussion:', input$myplot_response)), collapse = "\n")
      # content <- paste(c(content, read_file("www/Exercise3.md")), collapse = "\n")
      # content <- paste(c(content, paste('<br>Table discussion:', input$mytable_response)), collapse = "\n")
      
      cat(content, file = tmp)
      rmarkdown::render(tmp, output_file = file)
    }
  )
}

dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

# Run the application
# runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
shinyApp(ui = ui, server = server)
