# Bangkitkan Variabel Random X dengan IID U[a,b] sebanyak n
# Nama: Steven Soewignjo
# NIM: 082011833060
# Tugas Minggu ke-9

require("pacman")
pacman::p_load(ggplot2)

gx <- function(x) {
    return(1/((x+5)^2))
}

montecarlo <- function(n, lb, ub) {
    vr <- runif(n, lb, ub)
    i <- (ub - lb) * sum(gx(vr))/n
    return(i)
}

acceptanceMethod <- function(n, lb, ub) {
    fmax <- max(gx(seq(lb, ub, by = 0.01)))
    xr <- runif(n, lb, ub)
    yr <- runif(n, 0, 1) * fmax
    count_in <- 0

    gx_res <- gx(xr)
    condition <- rep(0, n)

    for(i in 1:n) {
        if(yr[i] <= gx(xr[i])) {
            count_in <- count_in + 1
            condition[i] <- 1
        }
    }

    df <- data.frame(xr, yr, gx_res, condition)

    acc <- subset(df, condition == 1)
    rej <- subset(df, condition == 0)

    plotdata <- data.frame(xr, gx_res)
    plotdata <- plotdata[order(plotdata$xr),]

    color_reject <- "#ff6f69"
    color_accept <- "#54c1db"
    color_gx <- "#22223b"
    shape_reject <- 23
    shape_accept <- 24

    win.graph()
    acceptance_plot <- ggplot() +
    # Add the rejection points
    geom_point(data = rej, aes(x = xr, y = yr), shape = 4, color = "black") +
    # Add the accepted points
    geom_point(data = acc, aes(x = xr, y = yr), shape = 16, color = "blue", size = 2) +
    # Add the g(x) line
    geom_line(data = plotdata, aes(x = xr, y = gx_res), color = "red", size = 1.5) +
    # Add plot labels and title
    labs(x = "x", y = "y", title = "Acceptance-Rejection Method\nby Steven Soewignjo (082011833060)") +
    # Customize plot theme and legend
    theme_classic() +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14, face = "bold"),
          legend.position = "top",
          legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid", fill = "#FFFFFF"),
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_color_manual(values = c(color_reject, color_accept, color_gx), name = "",
                       labels = c("Reject", "Accept", "g(x)")) +
    guides(color = guide_legend(override.aes = list(size = 4, shape = c(shape_reject, shape_accept, NA))))
    print(acceptance_plot)
    result <- ((count_in) / n) * (ub-lb) * fmax
    return(result)
}

# Monte Carlo Integration
montecarlo(1000, 0, 10)

# Monte Carlo Acceptance-Rejection Method
acceptanceMethod(1000, 0, 10)
