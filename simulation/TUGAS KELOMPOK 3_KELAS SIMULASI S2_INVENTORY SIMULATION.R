# Program Inventory Simulation
# Simulasi M10
# Anggota Kelompok:
# 1. Citra Imama (082011833015)
# 2. Steven Soewignjo (082011833060)

inventory_simulation <- function(mean, sd, stock, minimumStockOrderThreshold, oc, pc, hc, sc, sp, oq, weekcount) {
  # Inisialisasi variabel
  week <- seq(1, weekcount)
  current_stock <- rep(0, weekcount)
  demand <- rep(0, weekcount)
  # demand <- c(732, 794, 815, 859, 676, 831)
  # demand <- c(191, 155, 167, 170, 206, 186)
  sold <- rep(0, weekcount)
  sisa <- rep(0, weekcount)
  order_status <- rep(0, weekcount)
  order_cost <- rep(0, weekcount)
  holding_cost <- rep(0, weekcount)
  shortage_cost <- rep(0, weekcount)
  purchasing_cost <- rep(0, weekcount)
  total_cost <- rep(0, weekcount)
  revenue <- rep(0, weekcount)
  profit <- rep(0, weekcount)
  
  # Set nilai awal
  current_stock[1] <- stock
  
  # Create table
  table <- data.frame(week, current_stock, demand, sold, sisa, order_status, order_cost, holding_cost, shortage_cost, purchasing_cost, total_cost, revenue, profit)
  
  
  # Operation
  for(i in 1:weekcount) {
    table$demand[i] <- round(rnorm(1, mean, sd), 0)
    if(i == weekcount) {
      if(table$current_stock[i] >= table$demand[i]) {
        table$sold[i] <- table$demand[i]
        table$sisa[i] <- table$current_stock[i] - table$demand[i]
        table$shortage_cost[i] <- 0
      } else {
        table$sold[i] <- table$current_stock[i]
        table$sisa[i] <- 0
        table$shortage_cost[i] <- sc * (table$demand[i] - table$current_stock[i])
      }
      if(table$sisa[i] < minimumStockOrderThreshold) {
        table$order_status[i] <- 1
        table$order_cost[i] <- oc
      } else {
        table$order_status[i] <- 0
        table$order_cost[i] <- 0
      }
      table$holding_cost[i] <- hc * table$sisa[i]
      table$total_cost[i] <- table$order_cost[i] + table$holding_cost[i] + table$shortage_cost[i] + table$purchasing_cost[i]
      table$revenue[i] <- sp * table$sold[i]
      table$profit[i] <- table$revenue[i] - table$total_cost[i]
    } else {
      if(table$current_stock[i] >= table$demand[i]) {
        table$sold[i] <- table$demand[i]
        table$sisa[i] <- table$current_stock[i] - table$demand[i]
        table$shortage_cost[i] <- 0
      } else {
        table$sold[i] <- table$current_stock[i]
        table$sisa[i] <- 0
        table$shortage_cost[i] <- sc * (table$demand[i] - table$current_stock[i])
      }
      if(table$sisa[i] < minimumStockOrderThreshold) {
        table$order_status[i] <- 1
        table$order_cost[i] <- oc
        table$current_stock[i + 1] <- table$sisa[i] + oq
      } else {
        table$order_status[i] <- 0
        table$order_cost[i] <- 0
        table$current_stock[i + 1] <- table$sisa[i]
      }
      table$holding_cost[i] <- hc * table$sisa[i]
      if(table$order_status[i] == 1) {
        table$purchasing_cost[i + 1] <- pc * oq
      } else {
        table$purchasing_cost[i + 1] <- 0
      }
      table$total_cost[i] <- table$order_cost[i] + table$holding_cost[i] + table$shortage_cost[i] + table$purchasing_cost[i]
      table$revenue[i] <- sp * table$sold[i]
      table$profit[i] <- table$revenue[i] - table$total_cost[i]
    }
  }
  cat("\n===[ TABEL HASIL SIMULASI ] ===\n")
  print(table)
  
  cat("\n===[ HASIL KEUNTUNGAN MINGGUAN ] ===\n")
  keuntunganMingguan <- data.frame(table$week, table$profit)
  totalKeuntungan <- sum(table$profit)
  rataRataKeuntungan <- mean(table$profit)
  
  print(keuntunganMingguan)
  cat("\nTotal keuntungan $", totalKeuntungan, "\n")
  cat("Rata-rata keuntungan $", rataRataKeuntungan, "\n")
  
}

inventory_simulation(200, sqrt(625), 600, 400, 85, 10, 1, 1.2, 50, 750, 6)

user_input <- function() {
  cat("Masukkan rata-rata penjualan dalam satu minggu: ")
  mean <- as.numeric(readline())
  cat("Masukkan simpangan baku penjualan dalam satu minggu: ")
  sd <- as.numeric(readline())
  cat("Masukkan stock awal: ")
  stock <- as.numeric(readline())
  cat("Masukkan reorder point: ")
  minimumStockOrderThreshold <- as.numeric(readline())
  cat("Masukkan Order Cost: ")
  oc <- as.numeric(readline())
  cat("Masukkan Purchase Cost: ")
  pc <- as.numeric(readline())
  cat("Masukkan Holding Cost: ")
  hc <- as.numeric(readline())
  cat("Masukkan Shortage Cost: ")
  sc <- as.numeric(readline())
  cat("Masukkan Selling Price: ")
  sp <- as.numeric(readline())
  cat("Masukkan Order Quantity: ")
  oq <- as.numeric(readline())
  cat("Masukkan Jumlah Minggu: ")
  weekcount <- as.numeric(readline())
  
  inventory_simulation(mean, sd, stock, minimumStockOrderThreshold, oc, pc, hc, sc, sp, oq, weekcount)
}

user_input()