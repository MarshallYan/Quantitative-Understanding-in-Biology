seed_value <- 19710822
set.seed(seed_value)

population <- 100
score <- rnorm(population)
optimal_score <- max(score)
phase1_fraction <- 1/exp(1)
phase1_size <- round(population * phase1_fraction)
cutoff_score <- max(score[1:phase1_size])
spouse_index <- phase1_size +
  which(score[(phase1_size+1):population] > cutoff_score)[1]
spouse_score <- score[spouse_index]
spouse_score == optimal_score

if (is.na(spouse_index)) {
  spouse_index <- population
}

num_iterations <- 1000
# vector to store result of each simulation
is_soulmate <- logical(length = num_iterations)
# Explicitly state the size of the total, and sampling phase, pools
population <- 100
phase1_fraction <- 1 / exp(1)
phase1_size <- round(population * phase1_fraction)

for (case_idx in 1:num_iterations) {
  # scores of potential mates
  score <- rnorm(population)
  # score of soul mate
  optimal_score <- max(score)
  
  # maximum score in gathering phase
  cutoff_score <- max(score[1:phase1_size])
  
  # now select as your life partner the next date with a better score
  spouse_index <- phase1_size +
    which(score[(phase1_size+1):population] > cutoff_score)[1]
  
  # pick the last one if nobody better came along before then
  if (is.na(spouse_index)) {
    spouse_index <- population
  }
  
  is_soulmate[case_idx] <- (score[spouse_index] == optimal_score)
}

mean(is_soulmate)



simulate_dating <- function(population = 1000,
                            phase1_fraction = 1 / exp(1),
                            num_iterations = 1) {
  is_soulmate <- logical(length = num_iterations)
  phase1_size <- round(population * phase1_fraction)
  
  for (case_idx in 1:num_iterations) {
    # scores of potential mates
    score <- rnorm(population)
    optimal_score <- max(score)
    
    # we date the first phase1_size people
    # and note the maximum score in that group
    cutoff_score <- max(score[1:phase1_size])
    
    # now select as your life partner the next date with a better score
    spouse_index <- phase1_size +
      which(score[(phase1_size+1):population] > cutoff_score)[1]
    
    # pick the last one if nobody better came along before then
    if (is.na(spouse_index)) {
      spouse_index <- population
    }
    
    is_soulmate[case_idx] <- (score[spouse_index] == optimal_score)
  }
  
  mean(is_soulmate)
}


phase1_fractions <- seq(0.05, 0.95, 0.02)
means <- numeric(length = length(phase1_fractions))
for (idx in 1:length(phase1_fractions)) {
  means[idx] <- simulate_dating(population = 100,
                                phase1_fraction = phase1_fractions[idx],
                                num_iterations = 1000)
}
plot(means ~ phase1_fractions)
phase1_fractions[which.max(means)]


phase1_fractions <- seq(0.05, 0.95, 0.02)
means <- numeric(length = length(phase1_fractions))
for (idx in 1:length(phase1_fractions)) {
  means[idx] <- simulate_dating(population = 100,
                                phase1_fraction = phase1_fractions[idx],
                                num_iterations = 10000)
}
plot(means ~ phase1_fractions)
phase1_fractions[which.max(means)]


simulate_dating_2 <- function(population = 1000,
                              phase1_fraction = 1 / exp(1),
                              num_iterations = 1) {
  spouse_scores <- numeric(length = num_iterations)
  phase1_size <- round(population * phase1_fraction)
  
  for (case_idx in 1:num_iterations) {
    # scores of potential mates
    score <- rnorm(population)
    
    # we date the first phase1_size people
    # and note the maximum score in that group
    cutoff_score <- max(score[1:phase1_size])
    optimal_score <- max(score)
    
    # now select as your life partner the next date with a better score
    spouse_index <- phase1_size +
      which(score[(phase1_size+1):population] > cutoff_score)[1]
    
    if (is.na(spouse_index)) {
      spouse_index <- population
    }
    
    spouse_scores[case_idx] <- score[spouse_index]
  }
  
  mean(spouse_scores)
}

phase1_fractions <- seq(0.05, 0.95, 0.02)
means <- numeric(length = length(phase1_fractions))
for (idx in 1:length(phase1_fractions)) {
  means[idx] <- simulate_dating_2(population = 100,
                                  phase1_fraction = phase1_fractions[idx],
                                  num_iterations = 10000)
}
plot(means ~ phase1_fractions)

phase1_fractions[which.max(means)]

