#This simulation calculates costs for time_steps/jobs_per_batch number of jobs, and allows us to specify the confidence interval of our
#sensors, whether or not we want vulnerabilities to occur, and whether or not we want to implement sensors. 
# Originally geometries were generated inside the code, but it makes more sense to pass that in to try to keep comparisons as fair as possible
# due to the random nature of their generation as well as the the Poisson process that governs STL/Gcode vulnerabilities.
simulation<-function(Geoms,time_steps,jobs_per_batch,printer_count, errors=FALSE, confidence_interval=0.95, sensors=FALSE)
{
  G=Geoms
  # Check if sensors are being used
  if(!sensors)
  {
    # Check if errors are being used too, use a poisson process to generate a list of 0/1/2 each representing a type of success or failure
    # 0 is success, 1 is stl error, and 2 is gcode error. We assume that stl errors are easier to try to insert into the system, thus are 10
    # times more prevalent than their gcode counterparts.
      if(errors==TRUE)
      {
        clock=rpois(time_steps,0.1)+rpois(time_steps,0.001)*2
      }
      else
      {
        clock=rep(0,time_steps)
      }
  }
  else
  {
    # If no sensors, check if errors are being used
    if(errors==TRUE)
      {
        clock=rpois(time_steps,0.1*(1-confidence_interval))+rpois(time_steps,0.001*(1-confidence_interval))*2
      }
      else
      {
        clock=rep(0,time_steps)
      }
  }
  #Put geometries and list of successes/fails into a data frame, and sort by size from smallest to largest.
  geoms_time<-data.frame(G,clock)
  geoms_time<-geoms_time[order(geoms_time$G),]
  #Create empty array to store output
  job_cost=c()
  i=1
  #Loop over all values in array
  while(i<=length(clock))
  {
    #If jobs per batch is 5, we look at the range 1-5, then start the next iteration at 6.
    job=geoms_time[i:(i+jobs_per_batch-1),]
    job_cost=c(job_cost,mpcam(job$G, job$clock, sensors))
    i=i+jobs_per_batch
    print(i)
  }
  #Return the costs of the jobs per batch.
  return(job_cost)
}

#Generate 1000 random stl geometries with a fixed size per megabyte.
G=geom_generator(1000)

#Run the simulations with and without errors and sensors, using the same geometry list.
cost_no_errors=simulation(G,1000,5,1)
cost_with_errors=simulation(G,1000,5,1,errors=TRUE)
cost_with_errors_and_sensors=simulation(G,1000,5,1,errors=TRUE,sensors = TRUE)

df=data.frame(cost_no_errors, cost_with_errors, cost_with_errors_and_sensors)
library(ggplot2)

