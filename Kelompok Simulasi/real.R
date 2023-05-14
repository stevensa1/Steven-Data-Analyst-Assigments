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
