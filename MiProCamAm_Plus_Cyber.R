#This calculates the cost of the preparation of the cad files.
Cost_prep<-function(operator_wage,workstation_costs,cad_time,quantities){
return(sum(operator_wage+workstation_costs*sum(cad_time)/quantities))
}

#This calculates the cost of the preparation of the stl files into gcode.
Cost_buildjob<-function(build_time,operator_wage,workstation_costs,volumes,quantities){
return(build_time*(operator_wage+workstation_costs)*volumes/sum(volumes*quantities))  
}

#This calculates the cost of setting up the AM device including material switching, use of inert gas, etc. 
Cost_setup<-function(setup_time,operator_wage,machine_costs,volumes,mat_change_time,mat_change_freq,F_inert_gas,quantities){
return(
 (operator_wage+machine_costs)*(setup_time+(mat_change_time*mat_change_freq))*F_inert_gas*volumes/sum(volumes*quantities)
  )  
}

#This calculates the cost of the print including inert gas use, power costs/usage, the waste factor of your prints, etc. 
Cost_print<-function(printing_times,machine_costs,C_inert_gas,gas_consumption_rate,energy_costs,power,utilization,mass,material_costs,waste){
 return(
printing_times*(machine_costs+C_inert_gas*gas_consumption_rate+energy_costs*power*utilization)+mass*(material_costs*waste)
)
}

#This calculates the cost of part removal. 
Cost_removal<-function(removal_time,operator_wage,machine_costs,volumes,F_inert_gas,quantities){
return(
 removal_time*(operator_wage+machine_costs)*F_inert_gas*volumes/sum(volumes*quantities)
  )  
}

#This calculates the cost of an stl error on a batch of jobs. We assume that 1 stl error will ruin any uncompleted parts.
Cost_stl_error<-function(G,clock,wf=0.1){
  start=match(1,clock)
  waste=c(rep(wf,start-1),rep(1,length(clock)-start))
}

#This calculates the cost of a gcode error in terms of damage to the printer. We calculate this effectively like increased 
#depreciation over the print time, given a certain damage factor.
Cost_gcode_damage<-function(m_cost, pt, clock, damage_factor){
  start=match(2,clock)
  waste=c(rep(0,start-1),rep(1,length(clock)-start+1))
  return(m_cost*pt*waste*damage_factor)
}

# This calculates the cost of sensor maintainence for jobs not caught by the 95% confidence interval
# with the programmer in charge of adding these edge cases being paid $25 per hour.
# We assume it takes minimal time to add these new cases, which is roughly equivalent to print time of the parts.
Cost_sensor_maintainence<-function(programmer_wage, pt, clock){
  start1=match(2,clock)
  start2=match(1,clock)
  if(start1<start2)
    start=start1
  else if(start1<start2)
    start=start2
  waste=c(rep(0,start-1),rep(1,length(clock)-start+1))
  return(sum(programmer_wage*pt*clock))
}

# This is from the rescheduling paper which gives us the probability of losing a customer from different
# parameters determined in the field, this is then added to 1 to give us the cost of failed parts and 
# losing business.
customer_deviation <-function(clock,alpha1=0.88,lambda1=2.25,beta1=0.88,p_t_initial){
  clock[which(clock==0)]<-1
  clock[which(clock==2)]<-0
  u_jobs=p_t_initial*clock
  temp=sum(u_jobs)-sum(p_t_initial)
  mu1=lambda1*((-sum(u_jobs)+sum(p_t_initial))/sum(p_t_initial))^beta1
  return(mu1)
}

# This generates a list of geometries in megabytes, this is used later with an empirically derived estimation to get volume in cubic cm.
geom_generator<-function(count){return(abs(rnorm(count,5,3.5)))}
estimate_volume<-function(G){return(G/6.840)}

# This estimates mass using the previous estimate of volume along with material density of the printing media in g/cm^3, and the selected infill density
estimate_mass<-function(volume, material_density, infill_density){return(volume*material_density/1000*infill_density)}

#This estimates the amount of time necessary to process cad files as a factor of geometry size.
cad_times<-function(G){return(G*1/60)}

# This was given by the miprocamam paper.
machine_costs<-function(machine_cost, depreciation, up_time){return(machine_cost/(depreciation*up_time))}

#This estimates the print time given print volume, layer height, extrusion width, and print speed, all in cm and cm/s
print_time<-function(volume, layer_height, extrusion_width, print_speed){return(volume/(layer_height*extrusion_width)/print_speed)}

mpcam<-function(G, clock, operator_wage=7.50, programmer_wage=25, machine_cost=300, workstation_cost=0.1, material_costs=25, electricity_costs=0.17*1.56, electricity_use_per_hour=0.27/2.75, material_density=1.25, t_setup=5/60, cost_inert_gas=0, material_change_rate=0, material_change_time=0, rate_gas_use=0, inert_gas_difficulty_multiplier=1,  infill_density=0.2, layer_height=0.02, removal_time=1/60, extrusion_width=0.04, waste=0.01, print_speed=6, depreciation=1, damage_factor=2, sensors=FALSE)
{
  #This first bit sets initial values.
  b_lost_cost=0
  programmer_cost=0
  gcode_damage_cost=0
  stl_error_cost=0
  utilization=1
  vol=estimate_volume(G)
  mass=estimate_mass(vol, material_density, infill_density)
  cad_t=cad_times(G)
  p_time=print_time(vol,0.02, 0.04, 6)

  #This bit checks if there is an stl error, if not, calculate costs normally, else, add stl error costs and business lost costs as well.
  if(!(1 %in% clock))
  {

    cost_machine=machine_costs(machine_cost, depreciation, 8760)
    prep_cost<-Cost_prep(operator_wage,workstation_cost,cad_t,length(G))
    buildj_cost<-Cost_buildjob(cad_t,operator_wage,workstation_cost,vol,length(G))
    setup_cost<-Cost_setup(t_setup,operator_wage,workstation_cost,vol,material_change_rate,material_change_time,inert_gas_difficulty_multiplier,length(G))
    print_cost<-Cost_print(sum(p_time/60),sum(cost_machine),cost_inert_gas,rate_gas_use,electricity_costs,sum(p_time/60)*electricity_use_per_hour,inert_gas_difficulty_multiplier,mass,material_costs,waste)
    removal_cost<-Cost_removal(removal_time,operator_wage,workstation_cost,vol,inert_gas_difficulty_multiplier,length(G))
  }
  else
  {
    cost_machine=machine_costs(machine_cost, depreciation, 8760)
    prep_cost<-Cost_prep(operator_wage,workstation_cost,cad_t,length(G))
    buildj_cost<-Cost_buildjob(cad_t,operator_wage,0.1,vol,length(G))
    setup_cost<-Cost_setup(t_setup,operator_wage,workstation_cost,vol,material_change_rate,material_change_time,inert_gas_difficulty_multiplier,length(G))
    print_cost<-Cost_print(sum(p_time/60),sum(cost_machine),cost_inert_gas,rate_gas_use,electricity_costs,sum(p_time/60)*electricity_use_per_hour,1,mass,25,waste)
    removal_cost<-Cost_removal(removal_time,operator_wage,workstation_cost,vol,inert_gas_difficulty_multiplier,length(G))
    stl_error_cost<-Cost_stl_error(G,clock,waste)
    b_lost_cost=b_lost_cost+stl_error_cost
  }
  #This bit checks if sensors are being used or not, if so, programmer costs are evaluated
  
  if(sensors==TRUE){
      programmer_cost=Cost_sensor_maintainence(programmer_wage, p_time, clock)
  }
  #This bit checks for gcode errors, if not present, the damage cost stays at 0, else, it's evaluated using the recovery model. 
  if(!(2 %in% clock))
  {
    gcode_damage_cost=0    
  }
  else
  {
    gcode_damage_cost=Cost_gcode_damage(cost_machine, p_time, clock, damage_factor)
    if(sensors==TRUE)
    {
      added_lost_business_factor=customer_deviation(clock, p_t_initial = p_time)
      saved_jobs=clock
      saved_jobs[which(clock==0)]<-0
      saved_jobs[which(clock==1)]<-0
      saved_jobs[which(clock==2)]<-1
      b_lost_cost=(1+added_lost_business_factor)*print_cost-gcode_damage_cost
      g_code_damage_cost=0
    }
  }
  #Finally, return the sum total of all costs.
  return(sum(sum(prep_cost), sum(buildj_cost), sum(setup_cost), sum(print_cost), sum(removal_cost), b_lost_cost, gcode_damage_cost, stl_error_cost, programmer_cost))
}