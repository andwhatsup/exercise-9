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
+available_role(Role)
    : true
    <- .print("Adopting the role of ", Role);
       adoptRole(Role);
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
       findall([A,T,R], interaction_trust(acting_agent,A,temperature(T),R), AllIT);
       .print("All ITRatings: ", AllIT);
       // 2) Ask each reader for its certified reputation
       setof([A,T], member([A,T,_], AllIT), Pairs);
       for (.member([Agt,Temp], Pairs)) {
           .print("Asking ", Agt, " for CR of Temp=", Temp);
           .send(Agt, ask, request_certified(self, Temp));
       };
       // 3) Wait for all CR and WR replies
       !wait_for_all_ratings(Pairs, AllIT);
    .

@wait_for_all_ratings_plan
+!wait_for_all_ratings(Pairs, AllIT)
    : true
    <- .print("Waiting for certified and witness reputations...");
       findall([A2,temperature(T2),CR2], certified_reputation(A2,A2,temperature(T2),CR2), AllCR);
       findall([A3,temperature(T3),WR3], witness_reputation(_,A3,temperature(T3),WR3), AllWR);
       .length(AllCR, NCR); .length(Pairs, N);  
       if (NCR < N) {
           .wait(500);
           !wait_for_all_ratings(Pairs, AllIT)
       } else {
           .print("All CRs: ", AllCR);
           .print("All WRs: ", AllWR);
           
           // 4) Combine ITR, CR, WR into final score
           .findall([FinalScore,Ag,Tmp],
               member([Ag,Tmp,R], AllIT) &
               // average interaction trust
               .findall(Rx, interaction_trust(acting_agent,Ag,temperature(Tmp),Rx), IRates) &
               .sum(IRates, SI) & .length(IRates, CI) & AvgIT = SI/CI &
               // certified reputation
               member([Ag,temperature(Tmp),CR], AllCR) &
               // witness reputation average
               .findall(Wx, member([Ag,temperature(Tmp),Wx], AllWR), WRates) &
               .sum(WRates, SW) & .length(WRates, CW) & AvgWR = SW/CW &
               // final blend
               FinalScore = (AvgIT + CR + AvgWR) / 3,
               FullScoredList);
           .print("Combined ITR/CR/WR scores: ", FullScoredList);
           .sort(FullScoredList, SortedAll);
           .length(SortedAll, L2);
           .nth(L2-1, SortedAll, [_,BestAg,BestTmp]);
           // 5) Select and manifest
           -+temperature(BestTmp);
           !manifest_temperature
       };
    .

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
