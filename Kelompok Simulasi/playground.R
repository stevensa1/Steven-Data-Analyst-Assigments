require(pacman) || install.packages("pacman")
if (!require("pacman")) require("pacman")

pacman::p_load(shiny, rio, shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Aplikasi Simulasi Antrian M/M/s pada SPBU di Jakarta Timur - Kelompok 3"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        numericInput("n", "Number of Customers:", value = 307),
        numericInput("lambda", "Arrival Rate (Customers per Hour):", value = 240),
        numericInput("mu", "Service Rate (Customers per Hour):", value = 61),
        numericInput("servers", "Number of Servers:", value = 2),
        actionButton("simulate", "Simulate"),
        downloadButton("downloadTable", "Download Table")
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation Results", tableOutput("results")),
        tabPanel("Metrics", 
                 h4("Metrics"),
                 verbatimTextOutput("server_utilization"),
                 verbatimTextOutput("average_queue"),
                 verbatimTextOutput("average_system"),
                 verbatimTextOutput("average_time_queue"),
                 verbatimTextOutput("average_time_system"),
                 verbatimTextOutput("recommendation")
        ),
        tabPanel("Identity and Details",
                 h4("Aplikasi Simulasi Antrian M/M/s pada SPBU di Jakarta Timur - Kelompok 3"),
                 h4("Simulasi - S2"),
                 h5("Anggota Kelompok 3:"),
                 p("1. Yan Dwi Pracoko, (NIM. 082011833043)"),
                 p("2. Steven Soewignjo (NIM. 082011833060)"),
                 p("3. Ajun Tri Sasongko, (NIM. 082011833090)")
        )
      ),
      width = 9
    )
  )
)

gas_simulation <- function(n, lambda, mu, servers = 1) {
  # n represents the number of customers
  # lambda represents the arrival rate (customers per hour)
  # mu represents the service rate (customers per hour)
  # servers represent the number of servers (default = 1)

  inter_arrival <- rexp(n, rate = (lambda / 60))
  service <- rexp(n, rate = (mu / 60))

  server <- vector("list", servers)
  for (i in 1:servers) {
    server[[i]] <- data.frame(next_available_time = 0)
  }
  server_assignment <- rep(0, n)

  arrival_time <- rep(0, n)
  queue <- rep(0, n)
  start_service_time <- rep(0, n)
  end_service_time <- rep(0, n)
  in_system <- rep(0, n)

  for (i in 1:n) {
    ifelse(i == 1, arrival_time[i] <- inter_arrival[i], arrival_time[i] <- arrival_time[i-1] + inter_arrival[i])
    server_available <- FALSE
    for (j in 1:servers) {
      if (arrival_time[i] > server[[j]]$next_available_time) {
        server_assignment[i] <- j
        start_service_time[i] <- arrival_time[i]
        end_service_time[i] <- start_service_time[i] + service[i]
        queue[i] <- start_service_time[i] - arrival_time[i]
        in_system[i] <- end_service_time[i] - arrival_time[i]
        server[[j]]$next_available_time <- end_service_time[i]
        server_available <- TRUE
        break
      }
    }
    if (!server_available) {
      server_assignment[i] <- which.min(sapply(server, function(x) x$next_available_time))
      start_service_time[i] <- server[[server_assignment[i]]]$next_available_time
      end_service_time[i] <- start_service_time[i] + service[i]
      queue[i] <- start_service_time[i] - arrival_time[i]
      in_system[i] <- end_service_time[i] - arrival_time[i]
      server[[server_assignment[i]]]$next_available_time <- end_service_time[i]
    }
  }

  # Parameter Calculation
  # Calculate average server utilization (rho)
  server_utilization <- lambda / (servers * mu)

  # Probability 0 customer in the system
  prob_0 <- 1 - server_utilization

  # Calculate average number in the queue (Lq)
  average_queue <- (prob_0 * (lambda / mu)^servers * server_utilization) / (factorial(servers) * (1 - server_utilization)^2)
  
  # Calculate average number in the system (L)
  average_system <- average_queue + (lambda / mu)

  # Calculate average time in the queue (Wq)
  average_time_queue <- average_queue / lambda

  # Calculate average time in the system (W)
  average_time_system <- average_system / lambda

  # Print the calculated metrics
  cat("Server Utilization (ρ): ", server_utilization, "\n")
  cat("Average Number in the Queue (Lq):", average_queue, "\n")
  cat("Average Number in the System (L):", average_system, "\n")
  cat("Average Time in the Queue (Wq) (minute):", average_time_queue * 60, "\n")
  cat("Average Time in the System (W) (minute):", average_time_system * 60, "\n")

  # Decision on server utilization
    if (server_utilization > 1) {
        cat("The server utilization is more than 1, the system is overloaded\n")
        recommended_server_size <- ceiling(lambda / mu)
        cat("Recommended server size:", recommended_server_size, "\n")

    } else {
        cat("The server utilization is less than 1, the system is not overloaded\n")
    }

  # Data Result
  start_hour <- 8
  start_minute <- 0

  start_datetime <- as.POSIXct(paste(Sys.Date(), paste0(start_hour, ":", start_minute, ":00")))
  num <- seq(1, n)
  result <- data.frame(
    num,
    inter_arrival,
    service,
    arrival_time,
    server_assignment,
    queue,
    start_service_time,
    end_service_time,
    in_system
  )
  result$start_service_time <- start_datetime + result$start_service_time * 60
  result$end_service_time <- start_datetime + result$end_service_time * 60
  result$arrival_time <- start_datetime + result$arrival_time * 60

  output <- list(
    result = result,
    server_utilization = server_utilization,
    average_queue = average_queue,
    average_system = average_system,
    average_time_queue = average_time_queue,
    average_time_system = average_time_system
  )
  return(output)
}

# View(gas_simulation(307, 240, 61, 2))

server <- function(input, output) {
  observeEvent(input$simulate, {
    result <- gas_simulation(input$n, input$lambda, input$mu, input$servers)
    table <- result$result
    
    table$arrival_time <- format(table$arrival_time, "%H:%M:%S")
    table$start_service_time <- format(table$start_service_time, "%H:%M:%S")
    table$end_service_time <- format(table$end_service_time, "%H:%M:%S")

    output$results <- renderTable({
      table
    })
    
    # Calculate metrics
    server_utilization <- result$server_utilization
    average_queue <- result$average_queue
    average_system <- result$average_system
    average_time_queue <- result$average_time_queue
    average_time_system <- result$average_time_system
    
    output$server_utilization <- renderText(paste("Server Utilization (ρ):", server_utilization))
    output$average_queue <- renderText(paste("Average Number in the Queue (Lq):", average_queue))
    output$average_system <- renderText(paste("Average Number in the System (L):", average_system))
    output$average_time_queue <- renderText(paste("Average Time in the Queue (Wq) (minute):", average_time_queue * 60))
    output$average_time_system <- renderText(paste("Average Time in the System (W) (minute):", average_time_system * 60))
    if (server_utilization > 1) {
        recommended_server_size <- ceiling(input$lambda / input$mu)
        output$recommendation <- renderText(paste("The server utilization is more than 1, the system is overloaded\nRecommended server size: ", recommended_server_size))
    } else {
        output$recommendation <- renderText(paste("The server utilization is less than 1, the system is not overloaded\n"))
    }

    output$downloadTable <- downloadHandler(
    filename = function() {
      paste("simulation_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        export(table, file = file, "csv")
    })
  })
}

shinyApp(ui = ui, server = server)