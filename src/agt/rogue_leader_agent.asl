// rogue leader agent is a type of sensing agent

// Here we store the witness ratings of the rogue leader agent
all_present_agents_witness_ratings(
  [sensing_agent_1, sensing_agent_2, sensing_agent_3, sensing_agent_4, sensing_agent_5, sensing_agent_6, sensing_agent_7, sensing_agent_8, sensing_agent_9],
  [-1, -1, -1, -1, 1, 1, 1, 1, 1]
).

/* Initial goals */
!set_up_plans. // the agent has the goal to add pro-rogue plans

/* 
 * Plan for reacting to the addition of the goal !set_up_plans
 * Triggering event: addition of goal !set_up_plans
 * Context: true (the plan is always applicable)
 * Body: adds pro-rogue plans for reading the temperature without using a weather station
*/

@set_up_plans_plan
+!set_up_plans : true <-

  // removes plans for reading the temperature with the weather station
  .relevant_plans({ +!read_temperature }, _, LL);
  .remove_plan(LL);
  .relevant_plans({ -!read_temperature }, _, LL2);
  .remove_plan(LL2);

  // adds a new plan for sending a witness_reputation to the acting agent,
  // when the agent receives a temperature reading from another temperature reader agent.
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

  // adds a new plan for always broadcasting the temperature -2
  .add_plan({ +!read_temperature : true
    <-
      .print("Reading the temperature");
      .print("Read temperature (Celcius): ", -2);
      .broadcast(tell, temperature(-2))}).

/*— Task 3: reply when someone “ask”s for my certified_reputation —*/
@reply_certified_reputation
+!kqml_received(Sender, ask, certified_reputation(CertAgent,Self,temperature(C),CR), Mid)
    :  Self == .my_name(Self) 
       & certified_reputation(CertAgent,Self,temperature(C),CR)
    <- .print("Replying to CR-ask from ", Sender, ": rating=", CR);
       .send(Sender, tell, certified_reputation(CertAgent,Self,temperature(C),CR));
.
/* Import behavior of sensing agent */
{ include("sensing_agent.asl") }