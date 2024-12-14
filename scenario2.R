# Function to generate customer arrivals
generate_arrival_times <- function(lambda_A, hours) {
  total_time <- hours * 60 # Convert hours to minutes
  arrivals <- cumsum(rexp(1000, rate = lambda_A / 60)) # Generate interarrival times
  arrivals[arrivals <= total_time] # Keep arrivals within the operating hours
}

# Function to generate service times
generate_service_times <- function(num_customers, lambda_S) {
  rexp(num_customers, rate = lambda_S) # Generate service times for all customers
}

# Function to simulate queue dynamics
simulate_queue_scenario2 <- function(arrivals, service_times, L, tables) {
  num_customers <- length(arrivals)
  departure_times <- numeric(num_customers)
  waiting_times <- numeric(num_customers)
  queue_lengths <- numeric(num_customers)
  down_time <- 0
  
  for (i in 1:num_customers) {
    # Check for available tables and chefs
    available_tables <- tables - sum(departure_times > arrivals[i])
    available_chefs <- L - sum(departure_times > arrivals[i])
    
    if (available_tables > 0 && available_chefs > 0) {
      # Customer is served immediately
      waiting_times[i] <- 0
      departure_times[i] <- arrivals[i] + service_times[i]
    } else {
      # Customer waits
      next_departure <- min(departure_times[departure_times > arrivals[i]])
      waiting_times[i] <- next_departure - arrivals[i]
      departure_times[i] <- next_departure + service_times[i]
    }
    
    # Track queue length
    queue_lengths[i] <- sum(departure_times > arrivals[i] & arrivals <= arrivals[i])
    
    # Calculate down-time if no tables or chefs are in use
    if (i > 1 && arrivals[i] > departure_times[i - 1]) {
      down_time <- down_time + (arrivals[i] - departure_times[i - 1])
    }
  }
  
  list(
    waiting_times = waiting_times,
    queue_lengths = queue_lengths,
    down_time = down_time,
    total_revenue = num_customers * 50,
    total_cost = L * 40 * 12, # L chefs at $40/hour for 12 hours
    profit = (num_customers * 50) - (L * 40 * 12)
  )
}

# Parameters for Scenario 2
lambda_A <- 10  # Customer arrival rate (per hour)
hours <- 12      # Operating hours (10 AM - 10 PM)
tables <- 5      # Number of tables

# Test with different numbers of chefs
results <- list()
for (L in 1:5) {
  arrival_times <- generate_arrival_times(lambda_A, hours)
  service_times <- generate_service_times(length(arrival_times), lambda_S = 3 * L)
  results[[paste("Chefs =", L)]] <- simulate_queue_scenario2(arrivals = arrival_times, 
                                                             service_times = service_times, 
                                                             L = L, tables = tables)
}

# View results for each chef configuration
results

