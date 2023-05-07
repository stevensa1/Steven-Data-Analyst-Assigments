inter_arrival <- c(3.740, 8.067, 0.047, 16.781, 9.563)
transaction <- c(3.235, 13.376, 2.405, 10.098, 3.848)

inter_arrival2 <- c(1.046, 6.488, 3.426, 7.701, 14.521, 6.97, 4.679, 26.804)
transaction2 <- c(6.455, 1.418, 10.068, 8.996, 11.428, 9.458, 10.566, 8.946)

queue_calc <- function(inter_arrival, transaction) {
    arrival <- rep(0, length(inter_arrival))
    start_system <- rep(0, length(inter_arrival))
    exit_system_time <- rep(0, length(inter_arrival))
    queue_system <- rep(0, length(inter_arrival))
    in_system <- rep(0, length(inter_arrival))
    for(i in 1:length(inter_arrival)) {
        if(i == 1) {
            arrival[i] <- inter_arrival[i]
            start_system[i] <- arrival[i]
            exit_system_time[i] <- start_system[i] + transaction[i]
            queue_system[i] <- 0
            in_system[i] <- exit_system_time[i] - arrival[i]
        } else {
            arrival[i] <- arrival[i-1] + inter_arrival[i]
            start_system[i] <- max(arrival[i], exit_system_time[i-1])
            exit_system_time[i] <- start_system[i] + transaction[i]
            queue_system[i] <- start_system[i] - arrival[i]
            in_system[i] <- exit_system_time[i] - arrival[i]
        }
    }
    result <- data.frame(inter_arrival, transaction, arrival, start_system, exit_system_time, queue_system, in_system)

    queue_average <- mean(queue_system)
    service_average <- mean(transaction)
    system_average <- mean(in_system)

    customer_average <- sum(in_system) / exit_system_time[length(exit_system_time)]
    customer_queue_average <- sum(queue_system) / exit_system_time[length(exit_system_time)]
    customer_service_queue_average <- sum(transaction) / exit_system_time[length(exit_system_time)]

    cat("Average queue system: ", queue_average, "\n")
    cat("Average service system: ", service_average, "\n")
    cat("Average system: ", system_average, "\n")
    cat("Average customer: ", customer_average, "\n")
    cat("Average customer queue: ", customer_queue_average, "\n")
    cat("Average customer service queue: ", customer_service_queue_average, "\n")
    print(result)
}

queue_calc(inter_arrival, transaction)
queue_calc(inter_arrival2, transaction2)