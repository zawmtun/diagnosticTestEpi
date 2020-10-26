library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(tibble)
library(shiny)

theme_set(theme_light())
options(shiny.autoreload = TRUE)

# Create dataset ----------------------------------------------------------


sim <- function(prev, n) {
    case <- rnorm(n*prev, mean = 30, sd = 7)
    non_case <- rexp(n*(1 - prev), 0.16)
    tibble(prevalence = prev, case = list(case), non_case = list(non_case))
}

create_2by2 <- function(threshold, dis_prev, lab_values, n) {
    values <- lab_values %>%
        filter(prevalence == dis_prev)

    case <- unlist(values$case)
    non_case <- unlist(values$non_case)

    ac <- n*dis_prev
    bd <- n - ac
    a <- length(case[case >= threshold])
    b <- length(non_case[non_case >= threshold])
    c <- ac - a
    d <- bd - b
    Sensitivity <- round(a/ac, 3)
    Specificity <- round(d/bd, 3)
    ppv <- round(a/(a + b), 3)

    tibble(threshold = threshold,
           prev = dis_prev,
           a, b, c, d, ac, bd, Sensitivity, Specificity, ppv) %>%
        mutate(across(where(is.integer), as.double))
}

n <- 10000
p <- c(0, 2, 5, 8, seq(10, 100, 10))
plot_colours <- c("#B2182B", "#6CA6CD")

lab_results <- map_dfr(p/100, sim, n = n)

param <- crossing(threshold = seq(0, 50, 1),
                  prevalence = p/100)

property <- map2_dfr(param$threshold, param$prevalence, create_2by2,
                     lab_values = lab_results, n = n)



# Shiny app ---------------------------------------------------------------


ui <- fluidPage(
    titlePanel("Diagnostic Test Properties"),

    sidebarLayout(
        sidebarPanel(
            htmlOutput("desc", inline = TRUE),
            sliderInput("prevalence", "Disease prevalence",
                        value = 0.05, step = 0.1, min = 0,
                        max = 0.9),
            sliderInput("threshold", "Threshold for a positive result",
                        value = 10, step = 1, animate = TRUE,
                        min = 10, max = 30),
            tableOutput("twobytwo"),
            htmlOutput("author", inline = TRUE)
        ),
        mainPanel(
            fluidRow(
                column(6, plotOutput("thresholdplot")),
                column(6, plotOutput("ss"))
            ),
            fluidRow(
                column(6, plotOutput("pv")),
                column(6, plotOutput("roc"))
            )
        )
    )
)

server <- function(input, output, session) {
    # Two by two table
    output$twobytwo <- renderTable({
        output$twobytwo <- renderTable({
            num <- property %>%
                filter(threshold == input$threshold & prev == input$prevalence) %>%
                select(a:d) %>%
                pivot_longer(everything())

            name <- num$name

            num <- num$value %>%
                as.character() %>%
                setNames(name)

            tab <- data.frame(d1 = c("", "Test positive", "Test negative"),
                              d2 = c("Disease present", num["a"], num["c"]),
                              d3 = c("Disease absent", num["b"], num["d"]))
            tab
        }, colnames = FALSE, align = NULL)
    })

    # Description
    output$desc <- renderText("
    This webapp is to demonstrate the influence of threshold choice for a positive result on selected characteristics of a diagnostic test using simulated data of 10,000 individuals.
    <ul>
        <li>Select a disease prevalence first. Then, observe the changes in the plots and two-by-two table by varying the threshold.</li>
	    <li>Alternatively, you can press the play button below the slider to raise the threshold step-by-step automatically.</li>
	</ul>")

    output$author <- renderText(
        "<p style='font-size: 10px'>Created by <a href='https://github.com/zawmtun'>Zaw Myo Tun</a>. Code for this webapp is available <a href='https://github.com/zawmtun/diagnosticTestEpi/blob/master/code/webapp/app.R'>here</a>.</p>"
        )

    # Sensitivity and specificity plot
    output$ss <- renderPlot({
        ss_dat <- property %>%
            filter(prev == input$prevalence) %>%
            filter(threshold %in% 10:30) %>%
            pivot_longer(c(Sensitivity, Specificity))

        ggplot(ss_dat, aes(x = threshold, y = value, colour = name)) +
            geom_line(size = 1.4) +
            geom_vline(xintercept = input$threshold, size = 1.2, colour = "grey35") +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            scale_color_manual(values = plot_colours) +
            labs(x = "Threshold for a positive result", y = "", colour = NULL,
                 title = "Sensitivity and Specificity") +
            theme(axis.title.y = element_text(size = 0.6),
                  legend.position = "top",
                  panel.grid.minor = element_blank())
    }, res = 96)

    # Threshold plot
    output$thresholdplot <- renderPlot({
        dat <- lab_results %>%
            filter(prevalence == input$prevalence)

        case <- unlist(dat$case)
        non_case <- unlist(dat$non_case)

        data.frame(value = c(case, non_case),
                   status = rep(c("Present", "Absent"), times = c(length(case), length(non_case)))) %>%
            ggplot(aes(x = value, fill = status, colour = status)) +
            geom_density(alpha = 0.4) +
            geom_vline(xintercept = input$threshold, size = 1.2, colour = "grey35") +
            labs(x = "Laboratory test values", y = NULL, colour = "Disease", fill = "Disease",
                 title = "Density of Laboratory Test Values") +
            scale_x_continuous(breaks = seq(0, 150, 10)) +
            scale_color_manual(values = plot_colours) +
            scale_fill_manual(values = plot_colours) +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "top")
    }, res = 96)

    # Predictive values plot
    output$pv <- renderPlot({
        property %>%
            filter(threshold == input$threshold) %>%
            ggplot(aes(x = prev, y = ppv)) +
            geom_line(size = 1, col = "grey35") +
            scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                               labels = scales::percent_format(accuracy = 1)) +
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                               labels = scales::percent_format(accuracy = 1)) +
            labs(x = "Disease prevalence", y = " ", colour = NULL,
                 title = "Positive Predictive Value") +
            theme(panel.grid.minor = element_blank(),
                  axis.title.y = element_blank())
    }, res = 96)

    # ROC
    output$roc <- renderPlot({
        roc_point <- property %>%
            filter(threshold == input$threshold & prev == input$prevalence) %>%
            select(Sensitivity, Specificity)

        roc_dat <- property %>%
            filter(prev == input$prevalence)

        ggplot() +
            geom_line(data = roc_dat,
                      aes(x = 1 - Specificity, y = Sensitivity),
                      size = 1, col = "grey35") +
            geom_point(data = roc_point,
                       aes(x = 1 - Specificity, y = Sensitivity),
                       col = "#CD2990", size = 4) +
            scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                               labels = scales::percent_format(accuracy = 1)) +
            scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                               labels = scales::percent_format(accuracy = 1)) +
            labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)",
                 colour = NULL, title = "ROC Curve") +
            theme(panel.grid.minor = element_blank())
    }, res = 96)
}

shinyApp(ui, server)
