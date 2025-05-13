// acting agent

selectedTemp(0).

/* Initial beliefs and rules */
robot_td("https://raw.githubusercontent.com/Interactions-HSG/example-tds/main/tds/leubot1.ttl").

/* Initial goals */
!start.


@start_plan
+!start
    : true
    <- .print("Hello world");
    .

@organization_deployed_plan
+organization_deployed(OrgName)
    : true
    <- .print("Notified about organization deployment of ", OrgName);
       joinWorkspace(OrgName);
       lookupArtifact(OrgName, OrgId);
       focus(OrgId);
    .

@available_role_plan
+available_role(temperature_manifestor)
    : true
    <- .print("Adopting the role of temperature_manifestor");
       adoptRole(temperature_manifestor);
    .

+interaction_trust(TargetAgent, SourceAgent, MessageContent, ITRating)
    : true
    <- .print("Interaction Trust Rating: (", TargetAgent, ", ", SourceAgent, ", ", MessageContent, ", ", ITRating, ")");
    .

+certified_reputation(CertAgent, SourceAgent, MessageContent, CRRating)
    : true
    <- .print("Certified Reputation Rating: (", CertAgent, ", ", SourceAgent, ", ", MessageContent, ", ", CRRating, ")");
    .

+witness_reputation(WitnessAgent, SourceAgent, MessageContent, WRRating)
    : true
    <- .print("Witness Reputation Rating: (", WitnessAgent, ", ", SourceAgent, ", ", MessageContent, ", ", WRRating, ")");
    .

/* Plan to collect ITR, ask for CR, then await CR and WR and combine */
@select_reading_task_0_plan
+!select_reading(TempReadings)
    :  true
    <-  .print("DEBUG - Starting select_reading plan");
        .nth(0, TempReadings, Celsius);
        
        // Create list of agents with current readings
        .findall(Agent, .member([_,Agent], TempReadings), ActiveAgents);
        .print("DEBUG - Active agents with readings: ", ActiveAgents);
        
        // Ask for certified reputations
        .findall([Agent, Mission], commitment(Agent,Mission,_), CommitmentList);
        for ( .member([Agent, Mission], CommitmentList) ) {
          if (Mission == temperature_reading_mission) {
            .send(Agent, askAll, certified_reputation(_,_,_,_));
          }
        };
        .wait(1000);
        
        .print("DEBUG - Collecting all ratings");
        // Get all ratings (only for active agents)
        .findall([SourceAgent, TargetAgent, MessageContent, ITRating], 
                interaction_trust(SourceAgent, TargetAgent, MessageContent, ITRating) & 
                .member(TargetAgent, ActiveAgents), 
                ITList);
        .findall([CertificationAgent, TargetAgent, MessageContent, CRRating], 
                certified_reputation(CertificationAgent, TargetAgent, MessageContent, CRRating) & 
                .member(TargetAgent, ActiveAgents), 
                CRList);
        .findall([WitnessAgent, TargetAgent, MessageContent, WRRating], 
                witness_reputation(WitnessAgent, TargetAgent, MessageContent, WRRating) & 
                .member(TargetAgent, ActiveAgents), 
                WRList);
	    .print("Received ", .length(ITList), " interaction trust ratings, ", .length(CRList), " certified reputation ratings, and ", .length(WRList), " witness reputation ratings.");

      // Create an artifact of type Calculator
      makeArtifact("Calculator", "tools.Calculator", [], CalculatorId);

      // Task 1 
      // findMaxITR(ITList, MostTrustworthyAgent)[artifact_id(CalculatorId)];

      // Task 3 
      // findMaxCR(ITList, CRList, MostTrustworthyAgent)[artifact_id(CalculatorId)];

      // Task 4 
      findMaxWR(ITList, CRList, WRList, MostTrustworthyAgent)[artifact_id(CalculatorId)];

      // Get the temperature reading of the most trustworthy agent
      .print("Selected most trustworthy agent: ", MostTrustworthyAgent);
      getTempReadingByAgent(MostTrustworthyAgent, TempReadings, MostTrustworthyTempReading)[artifact_id(CalculatorId)];
      .print("Most trustworthy temperature reading by agent: ", MostTrustworthyAgent, " - Temperature:", MostTrustworthyTempReading);
      -+selectedTemp(MostTrustworthyTempReading);
    .


@manifest_temperature_plan
+!manifest_temperature
    : temperature(Celsius) & robot_td(Location)
    <-  .findall([T,A], temperature(T)[source(A)], TempReadings);
        !select_reading(TempReadings);
        ?selectedTemp(SelectedTemp);
        // Get the selected temperature of the most trustworthy agent
        .findall(T, selectedTemp(T), SelectedTempList);
        .nth(0, SelectedTempList, SelectedTemp);
      

        .print("I will manifest the temperature: ", SelectedTemp);
        makeArtifact("covnerter", "tools.Converter", [], ConverterId); // creates a converter artifact



       convert(Celsius, -20.00, 20.00, 200.00, 830.00, Degrees)[artifact_id(ConverterId)];
       .print("Temperature Manifesting (moving robotic arm to): ", Degrees);
       makeArtifact("leubot1", "org.hyperagents.jacamo.artifacts.wot.ThingArtifact", [Location, true], Leubot1Id);
       // setAPIKey("your-api-key")[artifact_id(Leubot1Id)];
       invokeAction("https://ci.mines-stetienne.fr/kg/ontology#SetWristAngle",
                    ["https://www.w3.org/2019/wot/json-schema#IntegerSchema"], [Degrees])[artifact_id(Leubot1Id)];
    .
    
{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("$moiseJar/asl/org-rules.asl") }
{ include("inc/skills.asl") }
{ include("inc/interaction_trust_ratings.asl") }