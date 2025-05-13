// sensing agent

/* Initial beliefs and rules */

// Here we store the witness ratings of the sensing agents
// IMPORTANT: FOR TESTING PURPOSES, COMMENT OUT THE FOLLOWING BELIEF AND UNCOMMENT THE NEXT ONE
// => TO SEE THAT IF THE SENSING AGENTS DISTRUST EACH OTHER, THE ROGUES WIN DUE TO THE WITNESS RATINGS
all_present_agents_witness_ratings(
  [sensing_agent_1, sensing_agent_2, sensing_agent_3, sensing_agent_4, sensing_agent_5, sensing_agent_6, sensing_agent_7, sensing_agent_8, sensing_agent_9],
  [1, 1, 1, 1, -1, -1, -1, -1, -1]
).

// infers whether there is a mission for which goal G has to be achieved by an agent with role R
role_goal(R,G) :- role_mission(R,_,M) & mission_goal(M,G).

// infers whether the agent has a plan that is relevant for goal G
has_plan_for(G) :- .relevant_plans({+!G},LP) & LP \== [].

// infers whether there is no goal associated with role R for which the agent does not have a relevant plan
i_have_plans_for(R) :- not (role_goal(R,G) & not has_plan_for(G)).

/* Initial goals */
!start.

/* Plan for reacting to the addition of the goal !start */
@start_plan
+!start
    : true
    <- .print("Hello world");
.

// Plan for reacting to the addition of the belief temperature(Celsius)
// Sending witness_reputation to the acting agent depending on which agent sent the temperature reading
@temperature_plan
+temperature(Celsius)[source(Sender)] : true <-
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
    }.

// Plan for reacting to the addition of the goal !read_temperature
// Now matches the Moise annotations [scheme(...), source(...)]
@read_temperature_plan
+!read_temperature[scheme(_Scheme), source(_Self)]
    : true
    <- .print("Reading the temperature");
       readCurrentTemperature(47.42, 9.37, Celsius);
       .print("Read temperature (Celsius): ", Celsius);
       .broadcast(tell, temperature(Celsius));
.

// Plan for reacting to the addition of the belief organization_deployed(OrgName)
@organization_deployed_plan
+organization_deployed(OrgName)
    : true
    <- .print("Notified about organization deployment of ", OrgName);
       joinWorkspace(OrgName, _);
       lookupArtifact(OrgName, OrgId);
       focus(OrgId);
       !adopt_relevant_roles;
.

// Plan for adopting all relevant roles according to the organization spec
@adopt_relevant_roles_plan
+!adopt_relevant_roles
    : true
    <- .findall(Role, role(Role, Super) & i_have_plans_for(Role), RelevantRoles);
       .print("Inferred that I have plans for the roles: ", RelevantRoles);
       for (.member(Role, RelevantRoles)) {
         .print("Adopting the role of ", Role);
         adoptRole(Role);
       };
.

// Plan for printing new certified reputation ratings (from Task 3)
+certified_reputation(CertAgent, SourceAgent, MessageContent, CRRating)
    : true
    <- .print("Certified Reputation Rating: (", CertAgent, ", ", SourceAgent, ", ", MessageContent, ", ", CRRating, ")");
.

// Add plan to handle CR requests
@kqml_received_ask_plan[atomic]
+!kqml_received(Sender, ask, certified_reputation(CertAgent,Agent,Content,Rating), _)
    : true 
    <- // Forward the request to certification agent
       .send(certification_agent, askOne, certified_reputation(CertAgent,Agent,Content,Rating), Response);
       if (Response \== no) {
           .send(Sender, tell, Response);
       }.
/* Import behavior of agents that work in CArtAgO environments */
{ include("$jacamoJar/templates/common-cartago.asl") }

/* Import behavior of agents that work in MOISE organizations */
{ include("$jacamoJar/templates/common-moise.asl") }

/* Import behavior of agents that reason on MOISE organizations */
{ include("$moiseJar/asl/org-rules.asl") }

/* Import behavior of agents that react to organizational events */
{ include("inc/skills.asl") }