// rogue agent is a type of sensing agent
/* Initial beliefs and rules */
// initially, the agent believes that it hasn't received any temperature readings
all_present_agents_witness_ratings(
  [sensing_agent_1, sensing_agent_2, sensing_agent_3, sensing_agent_4, sensing_agent_5, sensing_agent_6, sensing_agent_7, sensing_agent_8, sensing_agent_9],
  [-1, -1, -1, -1, 1, 1, 1, 1, 1]
).

/* Initial goals */
!set_up_plans. // the agent has the goal to add pro-rogue plans


// Task 2 Collusion fix:
+temperature(Celsius)[source(rogue_leader_agent)] : true <-
    .print("Colluding: broadcasting Rogue Leader's reading: ", Celsius);
    .broadcast(tell, temperature(Celsius));
    .
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
        .add_plan({ +temperature(Celsius)[source(Sender)] : true <-
            .print("Received temperature reading from ", Sender, ": ", Celsius);
            // Sending witness_reputation to the acting agent
            .findall([Agents, WRRatings], all_present_agents_witness_ratings(Agents, WRRatings), WRRatingsList);
            .nth(0, WRRatingsList, WR);
            .nth(0, WR, Agents);
            .nth(1, WR, WRRatings);
            .my_name(Name);
            for ( .range(I,0,8) ) {
            .nth(I, Agents, Agent);
            .nth(I, WRRatings, WRRating);
            if (Sender == Agent & Agent \== Name) {
                .print("Sending witness reputation to acting_agent: witness_reputation(", Name, ", ", Agent, ", temperature(", Celsius, "), ", WRRating, ")");
                .send(acting_agent, tell, witness_reputation(Name, Agent, temperature(Celsius), WRRating));
            };
            };
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
/*— Task 3: reply when someone “ask”s for my certified_reputation —*/
@reply_certified_reputation
+!kqml_received(Sender, ask, certified_reputation(CertAgent,Self,temperature(C),CR), Mid)
    :  Self == .my_name(Self) 
       & certified_reputation(CertAgent,Self,temperature(C),CR)
    <- .print("Replying to CR-ask from ", Sender, ": rating=", CR);
       .send(Sender, tell, certified_reputation(CertAgent,Self,temperature(C),CR));
.
/* Import behavior of sensing agent */
{ include("sensing_agent.asl")}