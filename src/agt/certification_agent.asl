// certification agent - handles reputation certification requests

/* Initial beliefs and rules */
// initial beliefs about certified reputation ratings for sensing agents
certified_reputation(certification_agent, sensing_agent_1, temperature(10), 0.9).
certified_reputation(certification_agent, sensing_agent_2, temperature(10), 0.9).
certified_reputation(certification_agent, sensing_agent_3, temperature(10), 0.9).
certified_reputation(certification_agent, sensing_agent_4, temperature(10), 0.9).

// initial beliefs about certified reputation ratings for rogue agents
certified_reputation(certification_agent, sensing_agent_5, temperature(8), -0.5).
certified_reputation(certification_agent, sensing_agent_6, temperature(8), -0.5).
certified_reputation(certification_agent, sensing_agent_7, temperature(8), -0.5).
certified_reputation(certification_agent, sensing_agent_8, temperature(8), -0.5).

// initial beliefs about certified reputation ratings for the rogue leader agent
certified_reputation(certification_agent, sensing_agent_9, temperature(-2), -0.9).

/* Initial goals */
!start.

@start_plan
+!start
    : true
    <- .print("Certification agent started");
    .

/* Plan for handling get_certified requests from sensing agents */
@handle_get_certification_request
+!kqml_received(Sender, ask, get_certified(Agent, Temp), MId)
    : certified_reputation(certification_agent, Sender, temperature(_), CR)
    <- .print("Processing get_certified request from ", Sender, " for temp=", Temp);
       .send(Sender, tell, certified_reputation(certification_agent, Sender, temperature(Temp), CR));
       .print("Sent certification to ", Sender, " with rating ", CR).

/* Plan for handling unknown agents */
@handle_missing_get_certification
+!kqml_received(Sender, ask, get_certified(Agent, Temp), MId)
    : not certified_reputation(certification_agent, Sender, temperature(_), _)
    <- .print("No certification found for agent ", Sender);
       .send(Sender, tell, certified_reputation(certification_agent, Sender, temperature(Temp), -1.0)).

{ include("$jacamoJar/templates/common-cartago.asl") }