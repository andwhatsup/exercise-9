// rogue leader agent is a type of sensing agent

/* Initial goals */
!set_up_plans. // the agent has the goal to add pro-rogue plans

/* 
 * Plan for reacting to the addition of the goal !set_up_plans
 * Triggering event: addition of goal !set_up_plans
 * Context: true (the plan is always applicable)
 * Body: adds pro-rogue plans for reading the temperature without using a weather station
*/

@set_up_plans_plan
+!set_up_plans
    : true
    <-  // 1) Remove the default sensing plans (both +! and -! variants)
        .relevant_plans({ +!read_temperature },        _, LL1);
        .remove_plan(LL1);
        .relevant_plans({ -!read_temperature },        _, LL2);
        .remove_plan(LL2);

        // 2) Dynamically add our new plan:
        .add_plan(
          {
            +!read_temperature
                : true
                <-  .print("RogueLeader: Reading the temperature");
                    .print("Read temperature (Celsius): ", -2);
                    .broadcast(tell, temperature(-2));

                    /* leader rewards rogues (+1), punishes loyals (–1) */
                    .findall(A2, temperature(_)[source(A2)], AllReaders);
                    for (.member(A2, AllReaders)) {
                        if ( sub_atom(A2, 0, 11, _, "rogue_agent") ) {
                            .eval(WR,  1);   // reward each rogue agent
                        } else {
                            .eval(WR, -1);   // punish all loyal sensors
                        };
                        .print("RogueLeader sending WR for ", A2, ": WR=", WR);
                        .send( acting_agent, tell,
                               witness_reputation(self, A2,
                                   temperature(-2), WR) );
                    };
          }
        );
    .

/* Import behavior of sensing agent */
{ include("sensing_agent.asl") }