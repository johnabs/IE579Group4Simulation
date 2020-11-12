# Value function to measure deviation of customers

#c_i,k = completion time of job i in machine k
# unsatisfied membership fxn of customer i for completion time deviation
customer_deviation <-function(impact_x,alpha1,lambda1,beta1,c1_initial){
  if (impact_x >= 0) {
    unprocessed_jobs_customers = impact_x**alpha1
  }
  else{
    unprocessed_jobs_customers = -lambda1 *(-impact_x)**beta1
  }
  
  while (i >0){
    R1_jobi = c1_initial + (1/lambda1)**(1/beta1) # have to include (i=1,2..w)
  }
  if (R1_job1 > c1_initial) {
    mu1= 1
  }  else if (c1_initial <= c_i & c_i< R1_jobi) {
    mu1 = lambda1 * (c_i - c1_initial)**beta1
  }  else 
    mu1 =0  
}



# Value function to measure deviation of managers
manager_deviation <-function(impact_x,alpha2,lambda2,beta2,f1_initial,f){ 
  if (impact_x >= 0) {
    unprocessed_jobs_managers = impact_x**alpha2
  }
  else{
    unprocessed_jobs_customers = -lambda2 *(-impact_x)**beta2
  }
  
  i <-1
  while (i>0) {
    
    R2_jobi = f1_initial + (1/lambda1)**(1/beta2) # have to include (i=1,2..w)
  }
  
  if (R2_job1 <= f) {
    mu2= 1
  }  else if (f1_initial <= f_i & f_i< R2_jobi) {
    mu2 = lambda2 * (f_i - f1_initial)**beta2
  }  else 
    mu2 =0  
  }


# Value function to determine deviation of workers
 # need to write function for summation of Ai_j * g > 0
worker_deviation <- function(impact_x,sequence_dev,alpha3,lambda3,beta3,g)
  if (impact_x < 0){
    unprocessed_jobs_workers = -lambda3* ((-impact_x)**beta3)
  }

  R3= (1/lambda3)**(1/beta3)
  if (g >= R3){
    mu3 = 1
  } else if (g >= 0 && g < R3){
       mu3 = lambda3*(g**beta3)
      }



 