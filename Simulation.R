#This simulation calculates costs for time_steps/jobs_per_batch number of jobs, and allows us to specify the confidence interval of our
#sensors, whether or not we want vulnerabilities to occur, and whether or not we want to implement sensors. 
# Originally geometries were generated inside the code, but it makes more sense to pass that in to try to keep comparisons as fair as possible
# due to the random nature of their generation as well as the the Poisson process that governs STL/Gcode vulnerabilities.
simulation<-function(Geoms,time_steps,jobs_per_batch,printer_count, errors=FALSE, error_rate_stl=0.1, error_rate_gcode=0.001, confidence_interval=0.95, sensors=FALSE)
{
  G=Geoms
  # Check if sensors are being used
  if(!sensors)
  {
    # Check if errors are being used too, use a poisson process to generate a list of 0/1/2 each representing a type of success or failure
    # 0 is success, 1 is stl error, and 2 is gcode error. We assume that stl errors are easier to try to insert into the system, thus are 100
    # times more prevalent than their gcode counterparts.
      if(errors==TRUE)
      {
        c1=rpois(time_steps,error_rate_stl)
        c1[which(c1>0)]<-1
        c2=rpois(time_steps,error_rate_gcode)
        c2[which(c2>0)]<-1
        clock=c1+c2
        clock[which(clock>2)]<-2
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
      c1=rpois(time_steps,error_rate_stl*(1-confidence_interval))
      c1[which(c1>0)]<-1
      c2=rpois(time_steps,error_rate_gcode*(1-confidence_interval))
      c2[which(c2>0)]<-1
      clock=c1+c2
      clock[which(clock>2)]<-2
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



(sum(df$cost_with_errors-df$cost_no_errors)-sum(df$cost_with_errors_and_sensors-df$cost_no_errors))/sum(df$cost_with_errors-df$cost_no_errors)*100


sim_set_no_sensors_stl=c()
for(i in 10^seq(-7, 1, by=0.1)){
  sim_set_no_sensors_stl=c(sim_set_no_sensors_stl,sum(simulation(G, 1000, 5, 1, errors = TRUE,error_rate_stl = i, error_rate_gcode = 10^(-7) )))
}
sim_set_no_sensors_stl_df=data.frame(10^seq(-7,1, by=0.1), sim_set_no_sensors_stl)
colnames(sim_set_no_sensors_stl_df)=c("Rate","Cost")

sim_set_no_sensors_gcode=c()
for(j in 10^seq(-7, 1, by=0.1)){
  sim_set_no_sensors_gcode=c(sim_set_no_sensors_gcode,sum(simulation(G, 1000, 5, 1, errors = TRUE,error_rate_gcode = j, error_rate_stl = 10^(-7) )))
}
sim_set_no_sensors_gcode_df=data.frame(10^seq(-7,1, by=0.1), sim_set_no_sensors_gcode)
colnames(sim_set_no_sensors_gcode_df)=c("Rate","Cost")


sim_set_with_sensors_stl=c()
for(i in 10^seq(-7, 1, by=0.1)){
  sim_set_with_sensors_stl=c(sim_set_with_sensors_stl,sum(simulation(G, 1000, 5, 1, errors = TRUE,error_rate_stl = i, error_rate_gcode = 10^(-7), sensors = TRUE )))
}
sim_set_with_sensors_stl_df=data.frame(10^seq(-7,1, by=0.1), sim_set_with_sensors_stl)
colnames(sim_set_with_sensors_stl_df)=c("Rate","Cost")

sim_set_with_sensors_gcode=c()
for(j in 10^seq(-7, 1, by=0.1)){
  sim_set_with_sensors_gcode=c(sim_set_with_sensors_gcode,sum(simulation(G, 1000, 5, 1, errors = TRUE,error_rate_gcode = j, error_rate_stl = 10^(-7), sensors=TRUE )))
}
sim_set_with_sensors_gcode_df=data.frame(10^seq(-7,1, by=0.1), sim_set_with_sensors_gcode)
colnames(sim_set_with_sensors_gcode_df)=c("Rate","Cost")

nested<- matrix(data=NA, nrow=41, ncol=41)
temp0=seq(-3, 1, by=0.1)
for(i in seq(-3, 1, by=0.1)){
  for(j in seq(-3, 1, by=0.1)){
  temp=sum(simulation(G, 1000, 5, 1, errors = TRUE,error_rate_gcode = 10^j, error_rate_stl = 10^i, sensors=TRUE ))
  temp2=which(temp0==i)
  temp3=which(temp0==j)
  print(c(temp2,temp3))
  nested[temp2,temp3]=temp
  }
}
data_melt<-melt(nested)
wireframe(value~X1+X2, data=data_melt)

diff1=data.frame(10^seq(-7, 1, by=0.1),sim_set_no_sensors_gcode-sim_set_with_sensors_gcode)
diff2=data.frame(10^seq(-7, 1, by=0.1),sim_set_no_sensors_stl-sim_set_with_sensors_stl)
colnames(diff1)=c("Rate","Cost")
colnames(diff2)=c("Rate","Cost")

library(ggplot2)
ggplot(df, aes(cost_no_errors))+geom_histogram()+xlab("Cost Per Job")+ylab("Frequency Percentage")+theme_bw()+theme( panel.grid.major = element_blank())+scale_x_continuous(limits = c(0,250))+scale_y_continuous(limits = c(0,40))+ggtitle("Cost Per Job No Errors")
ggplot(df, aes(cost_with_errors))+geom_histogram()+xlab("Cost Per Job")+ylab("Frequency Percentage")+theme_bw()+theme( panel.grid.major = element_blank())+scale_x_continuous(limits = c(0,250))+scale_y_continuous(limits = c(0,40))+ggtitle("Cost Per Job With Errors")
ggplot(df, aes(cost_with_errors_and_sensors))+geom_histogram()+xlab("Cost Per Job")+ylab("Frequency Percentage")+theme_bw()+theme( panel.grid.major = element_blank())+scale_x_continuous(limits = c(0,250))+scale_y_continuous(limits = c(0,40))+ggtitle("Cost Per Job With Errors and Sensors")
ggplot(sim_set_no_sensors_stl_df, aes(x=Rate, y=Cost, color="STL"))+geom_point()+geom_point(data=sim_set_no_sensors_gcode_df, aes(x=Rate, y=Cost, color="G-Code"))+xlab("Poisson Frequency of Errors")+ylab("Total Cost")+theme_bw()+theme( panel.grid.major = element_blank())+ggtitle("Cost Versus STL and G-code Error Rates")+ scale_x_continuous(trans='log10')
ggplot(sim_set_with_sensors_stl_df, aes(x=Rate, y=Cost, color="STL"))+geom_point()+geom_point(data=sim_set_with_sensors_gcode_df, aes(x=Rate, y=Cost, "G-Code"))+xlab("Poisson Frequency of Errors")+ylab("Total Cost")+theme_bw()+theme( panel.grid.major = element_blank())+ggtitle("Cost Versus STL and G-code Error Rates With Sensors")+ scale_x_continuous(trans='log10')
ggplot(diff2, aes(x=Rate, y=Cost, color="STL_Difference"))+geom_point()+geom_point(data=diff1, aes(x=Rate, y=Cost, color="G_Code_Difference"))+xlab("Poisson Frequency of Errors")+ylab("Total Cost")+theme_bw()+theme( panel.grid.major = element_blank())+ggtitle("Cost Versus STL and G-code Error Rates With Sensors")+ scale_x_continuous(trans='log10')
wireframe(Cost~STL_Error_Freq+Gcode_Error_Freq, data=data_melt)
ggplot(data_melt2, aes(STL_Error_Freq, Gcode_Error_Freq, fill=Cost)) + geom_tile()+ ggtitle("Heat Map of Cost as a Function of STL and GCode Error Frequency")

