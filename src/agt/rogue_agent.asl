// rogue agent is a type of sensing agent
/* Initial beliefs and rules */
// initially, the agent believes that it hasn't received any temperature readings
received_readings([]).

/* Override: only forward the leader's exact reading */
@rogue_read_temperature_plan[atomic]
+!read_temperature
    : true
    <-  //  1. Log that we're looking for the leader's reading
        .print("Rogue agent: fetching leader's temperature");
        
        // 2. Collect all temperature values sent by rogue_leader_agent
        .findall(Val,
                 temperature(Val)[source(rogue_leader_agent)],
                 LeaderTemps);
        
        //  3. If we have at least one reading, rebroadcast the first;
       //     otherwise wait and retry.
    if (LeaderTemps \== []) {
        .nth(0, LeaderTemps, ChosenTemp);
        .print("Found leader's temperature: ", ChosenTemp);
        .print("Rebroadcasting leader's temperature: ", ChosenTemp);
        .broadcast(tell, temperature(ChosenTemp));
        
        // send witness ratings for each reader to the acting agent
        .findall(A2, temperature(_)[source(A2)], AllReaders);
        .member(A2, AllReaders);
        WR = 0.5;
        .send(acting_agent, tell,
            witness_reputation(self, A2, temperature(ChosenTemp), WR))
    } else {
            .print("No leader reading yet, waiting 1s");
            .wait(1000);
            !read_temperature
        };
    .


/* Initial goals */
!set_up_plans. // the agent has the goal to add pro-rogue plans

/* 
 * Plan for reacting to the addition of the goal !set_up_plans
 * Triggering event: addition of goal !set_up_plans
 * Context: true (the plan is always applicable)
 * Body: adds pro-rogue plans for reading the temperature without using a weather station
*/
+!set_up_plans
    :  true
    <-  // removes plans for reading the temperature with the weather station
        .relevant_plans({ +!read_temperature }, _, LL);
        .remove_plan(LL);
        .relevant_plans({ -!read_temperature }, _, LL2);
        .remove_plan(LL2);

        // adds a new plan for reading the temperature that doesn't require contacting the weather station
        // the agent will pick one of the first three temperature readings that have been broadcasted,
        // it will slightly change the reading, and broadcast it
        .add_plan({ +!read_temperature
            :  received_readings(TempReadings) &
               .length(TempReadings) >=3
            <-  .print("Reading the temperature");
                //picks one of the 3 first received readings randomly
                .random([0,1,2], SourceIndex);
                .reverse(TempReadings, TempReadingsReversed);
                .print("Received temperature readings: ", TempReadingsReversed);
                .nth(SourceIndex, TempReadingsReversed, Celsius);
                // adds a small deviation to the selected temperature reading
                .random(Deviation);
                // broadcasts the temperature
                .print("Read temperature (Celsius): ", Celsius + Deviation);
                .broadcast(tell, temperature(Celsius + Deviation));
            });

        // adds plan for reading temperature in case fewer than 3 readings have been received
        .add_plan({ +!read_temperature
            :  received_readings(TempReadings) &
               .length(TempReadings) < 3
            <-  // waits for 2000 milliseconds and finds all beliefs about received temperature readings
                .wait(2000);
                .findall(TempReading, temperature(TempReading)[source(Ag)], NewTempReadings);
                // updates the belief about all reaceived temperature readings
                -+received_readings(NewTempReadings);
                // tries again to "read" the temperature
                !read_temperature;
            });
    .

/* Import behavior of sensing agent */
{ include("sensing_agent.asl")}