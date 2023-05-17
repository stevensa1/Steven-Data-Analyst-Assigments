server_utilization <- result$server_utilization
      average_queue <- result$average_queue
      average_system <- result$average_system
      average_time_queue <- result$average_time_queue * 60
      average_time_system <- result$average_time_system * 60
      
      cat("Server Utilization (ρ): ", server_utilization, "\n")
      cat("Average Number in the Queue (Lq):", average_queue, "\n")
      cat("Average Number in the System (L):", average_system, "\n")
      cat("Average Time in the Queue (Wq) (minute):", average_time_queue, "\n")
      cat("Average Time in the System (W) (minute):", average_time_system, "\n")
      
      if (server_utilization > 1) {
        cat("The server utilization is more than 1, the system is overloaded\n")
        recommended_server_size <- ceiling(input$lambda / input$mu)
        cat("Recommended server size:", recommended_server_size, "\n")
      } else {
        cat("The server utilization is less than 1, the system is not overloaded\n")
      }