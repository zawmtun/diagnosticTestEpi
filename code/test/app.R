library(tidyverse)
library(shiny)

sim <- function(prev, n) {
    case <- rnorm(n*prev, mean = 40, sd = 7)
    non_case <- rexp(n*(1 - prev), 0.1)
    tibble(prevalence = prev, case = list(case), non_case = list(non_case))
}

create_2by2 <- function(cutoff, dis_prev, lab_values, n) {
    values <- lab_values %>%
        filter(prevalence == dis_prev)

    case <- unlist(values$case)
    non_case <- unlist(values$non_case)

    ac <- n*dis_prev
    bd <- n - ac
    a <- length(case[case >= cutoff])
    b <- length(non_case[non_case >= cutoff])
    c <- ac - a
    d <- bd - b
    Sensitivity <- round(a/ac, 3)
    Specificity <- round(d/bd, 3)
    ppv <- round(a/(a + b), 3)

    tibble(cutoff = cutoff,
           prev = dis_prev,
           a, b, c, d, ac, bd, Sensitivity, Specificity, ppv) %>%
        mutate(across(where(is.integer), as.double))
}

# lab_results <- (seq(10, 90, 10)/100) %>%
#     map_dfr(sim, n = 10000)
#
# param <- crossing(cutoff = seq(15, 30, 1),
#                   prevalence = seq(10, 90, 10)/100)
#
# test <- map2_dfr(param$cutoff, param$prevalence, create_2by2,
#                  lab_values = lab_results, n = 10000)


ui <- fluidPage(
    titlePanel("Diagnostic test properties"),

    fluidRow(
        column(6,
            sliderInput("cutoff", "Cut-off point for a positive result",
                        value = 20, step = 1, min = 20,
                        max = 35, animate = TRUE),
            sliderInput("prevalence", "Disease prevalence",
                        value = 0.1, step = 0.1, min = 0.1,
                        max = 0.9, animate = TRUE),
            tableOutput("twobytwo")
        ),
        column(6, plotOutput("pv"))
    ),
    fluidRow(
        column(6, plotOutput("cutoffplot")),
        column(6, plotOutput("ss"))
    )
)

server <- function(input, output, session) {
    n <- 10000

    lab_results <- (seq(10, 90, 10)/100) %>%
        map_dfr(sim, n = n)

    param <- crossing(cutoff = seq(20, 35, 1),
                      prevalence = seq(10, 90, 10)/100)

    property <- map2_dfr(param$cutoff, param$prevalence, create_2by2,
                         lab_values = lab_results, n = n)

    # Two by two table
    output$twobytwo <- renderTable({
        output$twobytwo <- renderTable({
            num <- property %>%
                filter(cutoff == input$cutoff & prev == input$prevalence) %>%
                select(a:d) %>%
                pivot_longer(everything())

            name <- num$name

            num <- num$value %>%
                as.character() %>%
                setNames(name)

            tab <- data.frame(d1 = c("", "Test positive", "Test negative"),
                              d2 = c("Diasese present", num["a"], num["c"]),
                              d3 = c("Diasese absent", num["b"], num["d"]))
            tab
        }, colnames = FALSE)
    })

    # Predictive values plot
    output$pv <- renderPlot({
        property %>%
            filter(cutoff == input$cutoff) %>%
            ggplot(aes(x = prev, y = ppv)) +
            geom_line(size = 1) +
            scale_x_continuous(limits = c(0.1, 0.9), breaks = seq(0, 1, 0.1),
                               labels = scales::percent_format(accuracy = 1)) +
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                               labels = scales::percent_format(accuracy = 1)) +
            labs(x = "Disease prevalence", y = " ", colour = NULL,
                 title = "Positive predictive value") +
            theme_light() +
            theme(panel.grid.minor = element_blank(),
                  axis.title.y = element_text(size = 0.1))
    }, res = 96)

    # Sensitivity and specificity plot
    output$ss <- renderPlot({
        property %>%
            filter(prev == input$prevalence) %>%
            pivot_longer(c(Sensitivity, Specificity)) %>%
            ggplot(aes(x = cutoff, y = value, colour = name)) +
            geom_line(size = 1) +
            labs(x = "Cutoff to define a positive test", y = " ", colour = NULL,
                 title = "Sensitivity and specificity") +
            theme_light() +
            theme(axis.title.y = element_text(size = 0.6))
    }, res = 96)

    # Cutoff plot
    output$cutoffplot <- renderPlot({
        dat <- lab_results %>%
            filter(prevalence == input$prevalence)

        case <- unlist(dat$case)
        non_case <- unlist(dat$non_case)

        data.frame(value = c(case, non_case),
                   status = rep(c("Present", "Absent"), times = c(length(case), length(non_case)))) %>%
            ggplot(aes(x = value, fill = status, colour = status)) +
            geom_density(alpha = 0.4) +
            geom_vline(xintercept = input$cutoff, size = 1.2, colour = "#173B71") +
            labs(x = "Laboratory test values", y = NULL, colour = "Disease", fill = "Disease",
                 title = "Density of laboratory test values") +
            scale_x_continuous(breaks = seq(0, 150, 10)) +
            theme_light() +
            theme(panel.grid.minor = element_blank())
    }, res = 96)
}

shinyApp(ui, server)
