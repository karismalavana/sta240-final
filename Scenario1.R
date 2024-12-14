# Function to generate customer arrival times
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
simulate_queue <- function(arrivals, service_times) {
  num_customers <- length(arrivals)
  departure_times <- numeric(num_customers)
  waiting_times <- numeric(num_customers)
  queue_lengths <- numeric(num_customers)
  down_time <- 0
  
  for (i in 1:num_customers) {
    if (i == 1 || arrivals[i] >= departure_times[i - 1]) {
      # No waiting: arrival after last customer has left
      waiting_times[i] <- 0
      departure_times[i] <- arrivals[i] + service_times[i]
      
      # Calculate down-time
      if (i > 1) {
        down_time <- down_time + max(0, arrivals[i] - departure_times[i - 1])
      }
    } else {
      # Customer waits: arrival before last customer has left
      waiting_times[i] <- departure_times[i - 1] - arrivals[i]
      departure_times[i] <- departure_times[i - 1] + service_times[i]
    }
    
    # Queue length: customers in line at arrival
    queue_lengths[i] <- sum(arrivals <= arrivals[i] & departure_times > arrivals[i])
  }
  
  list(
    waiting_times = waiting_times,
    queue_lengths = queue_lengths,
    down_time = down_time
  )
}

# Function to calculate metrics
calculate_metrics <- function(results) {
  list(
    avg_waiting_time = mean(results$waiting_times),
    max_waiting_time = max(results$waiting_times),
    avg_queue_length = mean(results$queue_lengths),
    max_queue_length = max(results$queue_lengths),
    total_down_time = results$down_time
  )
}

# Parameters for Scenario 1
lambda_A <- 5  # Customer arrival rate (per hour)
lambda_S <- 6  # Service rate (per hour)
hours <- 12    # Operating hours (10 AM - 10 PM)

# Step 1: Generate arrivals and service times
arrival_times <- generate_arrival_times(lambda_A, hours)
service_times <- generate_service_times(length(arrival_times), lambda_S)

# Step 2: Simulate queue
queue_results <- simulate_queue(arrival_times, service_times)

# Step 3: Calculate metrics
metrics <- calculate_metrics(queue_results)

# Step 4: Display results
print(metrics)

# Optional: Visualization
# Histogram of waiting times
hist(queue_results$waiting_times, breaks = 20, main = "Histogram of Waiting Times",
     xlab = "Waiting Time (minutes)", col = "blue")

# Time-series of queue lengths
plot(queue_results$queue_lengths, type = "l", col = "red", main = "Queue Lengths Over Time",
     xlab = "Customer Index", ylab = "Queue Length")
