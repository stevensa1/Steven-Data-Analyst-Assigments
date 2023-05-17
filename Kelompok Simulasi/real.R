# Load package
require(pacman) || install.packages("pacman")

gas_simulation <- function(n, lambda, mu) {
    # n represent number of customer
    # lambda represent arrival rate (customers per hour)
    # mu represent service rate (customers per hour)

    # Generate inter-arrival times and service times

    inter_arrival <- rep(0, n)
    service <- rep(0, n)

    for(i in 1:n) {
        inter_arrival[i] <- rexp(1, rate = (lambda / 60))
        service[i] <- rexp(1, rate = (mu / 60))
    }

    arrival_time <- rep(0, n)           # The time when customer arrive
    start_service_time <- rep(0, n)     # The time when customer start to be served
    end_service_time <- rep(0, n)       # The time when customer finish to be served
    queue <- rep(0, n)                  # How long customer wait in queue
    in_system <- rep(0, n)              # How long customer in system

    for(i in 1:n) {
        if(i == 1) {
            arrival_time[i] <- inter_arrival[i]
            start_service_time[i] <- arrival_time[i]
            end_service_time[i] <- start_service_time[i] + service[i]
            queue[i] <- 0
            in_system[i] <- end_service_time[i] - arrival_time[i]
        } else {
            arrival_time[i] <- arrival_time[i-1] + inter_arrival[i]
            start_service_time[i] <- max(arrival_time[i], end_service_time[i-1])
            end_service_time[i] <- start_service_time[i] + service[i]
            queue[i] <- start_service_time[i] - arrival_time[i]
            in_system[i] <- end_service_time[i] - arrival_time[i]
        }
    }

    result <- data.frame(inter_arrival, service, arrival_time, start_service_time, end_service_time, queue, in_system)
    
    return(result)
}

View(gas_simulation(10, 2, 50))


gas_simulation <- function(n, lambda, mu, servers = 1) {
  # n represents the number of customers
  # lambda represents the arrival rate (customers per hour)
  # mu represents the service rate (customers per hour)
  # servers represent the number of servers (default = 1)

  inter_arrival <- rexp(n, rate = (lambda / 60))
  service <- rexp(n, rate = (mu / 60))

  # server <- rep(data.frame(next_available_time <- 0), servers)
  server <- vector("list", servers)
  for(i in 1:servers) {
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
    for(j in 1:servers) {
        if(arrival_time[i] > server[[j]]$next_available_time) {
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
    if(!server_available) {
        server_assignment[i] <- which.min(sapply(server, function(x) x$next_available_time))
        start_service_time[i] <- server[[server_assignment[i]]]$next_available_time
        end_service_time[i] <- start_service_time[i] + service[i]
        queue[i] <- start_service_time[i] - arrival_time[i]
        in_system[i] <- end_service_time[i] - arrival_time[i]
        server[[server_assignment[i]]]$next_available_time <- end_service_time[i]
    }
  }

    result <- data.frame(inter_arrival, service, arrival_time, server_assignment, queue, start_service_time, end_service_time, in_system)

    return(result)
}

View(gas_simulation(10, 12, 10, 2))

server <- rep(data.frame(next_available_time <- 0), 2)
server[2]$next_available_time <- 10
server

server <- vector("list", 2)
server[[2]] <- data.frame(next_available_time = 10)

server[[2]]$next_available_time
