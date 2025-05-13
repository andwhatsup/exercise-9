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
+!select_reading(TempReadings, Celsius)
    :  true
    <-  .nth(0, TempReadings, Celsius);
    	// Ask the temperature_reader agents for their references. (1 second timeout)
      .findall([Agent, Mission], commitment(Agent,Mission,_), CommitmentList);
      for ( .member([Agent, Mission], CommitmentList) ) {
        if (Mission == temperature_reading_mission) {
          // We need to perform 'askAll' here, in case there are multiple certification agents
          .send(Agent, ask, certified_reputation(certification_agent,Agent,temperature(_),_));

        }
      };
      .wait(1000);

       // Get all ratings
       .findall([A,T,R], interaction_trust(acting_agent,A,temperature(T),R), AllIT);
       .findall([A,T,CR], certified_reputation(certification_agent,A,temperature(T),CR), AllCR);
       .findall([W,A,T,WR], witness_reputation(W,A,temperature(T),WR), AllWR);
       
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
    <-  ?selectedTemp(SelectedTemp);
        .print("I will manifest the temperature: ", Celsius);
      

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