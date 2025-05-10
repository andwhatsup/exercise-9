// acting agent

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
       !select_and_request_ratings;    /* <<< trigger the IT+CR+WR cycle */
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
@select_and_request_ratings_plan
+!select_and_request_ratings
    : true
    <- // 1) Gather all ITRatings
       .findall([A,T,R], interaction_trust(acting_agent,A,temperature(T),R), AllIT);
       .print("All ITRatings: ", AllIT);
       // 2) Ask each reader for its certified reputation
       .setof([A,T], .member([A,T,_], AllIT), Pairs);
       for (.member([Agt,Temp], Pairs)) {
           .print("Asking ", Agt, " for CR of Temp=", Temp);
           .send(Agt, ask, request_certified(self, Temp));
       };
       // 3) Wait for all CR and WR replies
       !wait_for_all_ratings(Pairs, AllIT);
    .

/* Base case - empty list */
+!compute_scores([], _, _, _, Acc, Full)
  <- Full = Acc.

/* Recursive case - compute score for each agent/temperature pair */
+!compute_scores([[Ag,Temp]|Rest], AllIT, AllCR, AllWR, Acc, Full) 
  <-  // 1) Get IT rating (or 0 if none)
    .findall(R, .member([Ag,Temp,R], AllIT), IRates);
    if (.length(IRates) > 0) {
      .nth(0, IRates, IT);
    } else {
      IT = 0;
    };
    
    // 2) Get CR rating (or 0 if none)
    CR = 0;
    if (.member([Ag,temperature(Temp), Rval], AllCR)) {
      CR = Rval;
    };
    
    // 3) Get WR rating (or 0 if none)
    .findall(R, .member([Ag,temperature(Temp), R], AllWR), WRates);
    if (.length(WRates) > 0) {
      .nth(0, WRates, WR);
    } else {
      WR = 0;
    };
    
    // 4) Compute the simple average
    Score = (IT + CR + WR) / 3;
    
    // 5) Recurse with the new [Score,Agent,Temp]
    !compute_scores(Rest, AllIT, AllCR, AllWR, [[Score,Ag,Temp]|Acc], Full).



@wait_for_all_ratings_plan
+!wait_for_all_ratings(Pairs, AllIT)
  : true
  <- .print("Waiting for certified and witness reputations...");
     /* collect CR and WR with error handling */
     .findall([A2,M2,CR2], 
        certified_reputation(A2,A2,M2,CR2) & 
        M2 = temperature(_), 
        AllCR);
     .findall([A3,M3,WR3], 
        witness_reputation(_,A3,M3,WR3) & 
        M3 = temperature(_), 
        AllWR);
     
     .length(AllCR, NCR); .length(Pairs, N);
     if (NCR < N) {
       .wait(500); 
       !wait_for_all_ratings(Pairs, AllIT)
     } else {
       .print("All CRs: ", AllCR);
       .print("All WRs: ", AllWR);

       /* build the scored list */
       !compute_scores(Pairs, AllIT, AllCR, AllWR, [], FullScoredList);
       .print("Combined ITR_CR_WR scores: ", FullScoredList);

       .sort(FullScoredList, SortedAll);
       .length(SortedAll, L2);
       .nth(L2-1, SortedAll, [_,BestAg,BestTmp]);

       -+temperature(BestTmp);
       !manifest_temperature
     }.

@manifest_temperature_plan
+!manifest_temperature
    : temperature(Celsius) & robot_td(Location)
    <- .print("I will manifest the temperature: ", Celsius);
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
