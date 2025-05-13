package tools;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import cartago.*;

// Artifact for calculating the trustworthiness of agents based on various trust/reputation ratings.
public class Calculator extends Artifact {

    private class InteractionTrust {

        private String sourceAgent;
        private String targetAgent;
        private String messageContent;
        private double ITRating;

        public InteractionTrust(String sourceAgent, String targetAgent, String messageContent, double ITRating) {
            this.sourceAgent = sourceAgent;
            this.targetAgent = targetAgent;
            this.messageContent = messageContent;
            this.ITRating = ITRating;
        }

        public String getSourceAgent() {
            return sourceAgent;
        }

        public String getTargetAgent() {
            return targetAgent;
        }

        public String getMessageContent() {
            return messageContent;
        }

        public double getITRating() {
            return ITRating;
        }
    }

    private class CertificationReputation {

        private String certificationAgent;
        private String targetAgent;
        private String messageContent;
        private double CRRating;

        public CertificationReputation(String certificationAgent, String targetAgent, String messageContent, double CRRating) {
            this.certificationAgent = certificationAgent;
            this.targetAgent = targetAgent;
            this.messageContent = messageContent;
            this.CRRating = CRRating;
        }

        public String getCertificationAgent() {
            return certificationAgent;
        }

        public String getTargetAgent() {
            return targetAgent;
        }

        public String getMessageContent() {
            return messageContent;
        }

        public double getCRRating() {
            return CRRating;
        }
    }

    private class WitnessReputation {
        private String witnessAgent;
        private String targetAgent;
        private String messageContent;
        private double WRRating;

        public WitnessReputation(String witnessAgent, String targetAgent, String messageContent, double WRRating) {
            this.witnessAgent = witnessAgent;
            this.targetAgent = targetAgent;
            this.messageContent = messageContent;
            this.WRRating = WRRating;
        }

        public String getWitnessAgent() {
            return witnessAgent;
        }

        public String getTargetAgent() {
            return targetAgent;
        }

        public String getMessageContent() {
            return messageContent;
        }

        public double getWRRating() {
            return WRRating;
        }
    }

    // Creats an array of InteractionTrust objects from the given ITList
    private ArrayList<InteractionTrust> parseITList(Object[] ITList) {
        ArrayList<InteractionTrust> ITArrayList = new ArrayList<InteractionTrust>();
        for (Object entry : ITList) {
            Object[] IT = (Object[]) entry;
            String sourceAgent = (String) IT[0];
            String targetAgent = (String) IT[1];
            String messageContent = (String) IT[2];
            Double ITRating = ((Number) IT[3]).doubleValue();
            InteractionTrust ITObj = new InteractionTrust(sourceAgent, targetAgent, messageContent,
                    ITRating);
            ITArrayList.add(ITObj);
        }
        return ITArrayList;
    }

    private ArrayList<CertificationReputation> parseCRList(Object[] CRList) {
        ArrayList<CertificationReputation> CRArrayList = new ArrayList<CertificationReputation>();
        for (Object entry : CRList) {
            Object[] CR = (Object[]) entry;
            String certificationAgent = (String) CR[0];
            String targetAgent = (String) CR[1];
            String messageContent = (String) CR[2];
            Double CRRating = ((Number) CR[3]).doubleValue();
            CertificationReputation CRObj = new CertificationReputation(certificationAgent, targetAgent,
                    messageContent, CRRating);
            CRArrayList.add(CRObj);
        }
        return CRArrayList;
    }

    private ArrayList<WitnessReputation> parseWRList(Object[] WRList) {
        ArrayList<WitnessReputation> WRArrayList = new ArrayList<WitnessReputation>();
        for (Object entry : WRList) {
            Object[] WR = (Object[]) entry;
            String witnessAgent = (String) WR[0];
            String targetAgent = (String) WR[1];
            String messageContent = (String) WR[2];
            Double WRRating = ((Number) WR[3]).doubleValue();
            WitnessReputation WRObj = new WitnessReputation(witnessAgent, targetAgent, messageContent,
                    WRRating);
            WRArrayList.add(WRObj);
        }
        return WRArrayList;
    }

    private HashMap<String, Double> getAvgITRatingMap(ArrayList<InteractionTrust> ITArrayList) {
        HashMap<String, Double> avgITRatingMap = new HashMap<String, Double>();
        for (InteractionTrust ITObj : ITArrayList) {
            String targetAgent = ITObj.getTargetAgent();
            double ITRating = ITObj.getITRating();
            double sumITRating = ITRating;
            int countITRating = 1;
            for (InteractionTrust ITObj2 : ITArrayList) {
                if (ITObj2.getTargetAgent().equals(targetAgent) && !ITObj2.equals(ITObj)) {
                    sumITRating += ITObj2.getITRating();
                    countITRating++;
                }
            }
            double avgITRating = sumITRating / countITRating;
            avgITRatingMap.put(targetAgent, avgITRating);
        }
        return avgITRatingMap;
    }

    // Compute the average certification reputation rating for each target agent
    // NOTE: This implementation is not needed if only 1 certification_reputation
    // per target_agent is allowed...but it is here for completeness (and still
    // works)
    private HashMap<String, Double> getAvgCRRatingMap(ArrayList<CertificationReputation> CRArrayList) {
        HashMap<String, Double> avgCRRatingMap = new HashMap<String, Double>();
        for (CertificationReputation CRObj : CRArrayList) {
            String targetAgent = CRObj.getTargetAgent();
            double CRRating = CRObj.getCRRating();
            double sumCRRating = CRRating;
            int countCRRating = 1;
            for (CertificationReputation CRObj2 : CRArrayList) {
                if (CRObj2.getTargetAgent().equals(targetAgent) && !CRObj2.equals(CRObj)) {
                    sumCRRating += CRObj2.getCRRating();
                    countCRRating++;
                }
            }
            double avgCRRating = sumCRRating / countCRRating;
            avgCRRatingMap.put(targetAgent, avgCRRating);
        }
        return avgCRRatingMap;
    }

    // Compute the average witness reputation rating for each target agent
    private HashMap<String, Double> getAvgWRRatingMap(ArrayList<WitnessReputation> wrList) {
        // 1) Sum up all ratings and count them per targetAgent
        HashMap<String, Double> sumMap   = new HashMap<>();
        HashMap<String, Integer> countMap = new HashMap<>();

        for (WitnessReputation wr : wrList) {
            String  target = wr.getTargetAgent();
            double  rating = wr.getWRRating();
            sumMap.put(target, sumMap.getOrDefault(target, 0.0) + rating);
            countMap.put(target, countMap.getOrDefault(target, 0) + 1);
        }

        // 2) Divide sum by count to get the true average
        HashMap<String, Double> avgMap = new HashMap<>();
        for (String agent : sumMap.keySet()) {
            double total = sumMap.get(agent);
            int    cnt   = countMap.get(agent);
            avgMap.put(agent, total / cnt);
        }
        return avgMap;
    }
    // Method for obtaining the agent with the highest average interaction trust
    // rating
    @OPERATION
    public void findMaxITR(Object[] ITList, OpFeedbackParam<Object> mostTrustworthyAgent) {
        ArrayList<InteractionTrust> ITArrayList = parseITList(ITList);
        HashMap<String, Double> avgITRatingMap = getAvgITRatingMap(ITArrayList);
        // Find the targetAgent with the highest average ITRating
        double maxAvgITRating = 0;
        String mostTrustworthyAgentName = "";
        for (String targetAgent : avgITRatingMap.keySet()) {
            double avgITRating = avgITRatingMap.get(targetAgent);
            System.out.println("Average IT rating for agent " + targetAgent + ": " + avgITRating);
            if (avgITRating > maxAvgITRating) {
                maxAvgITRating = avgITRating;
                mostTrustworthyAgentName = targetAgent;
            }
        }
        mostTrustworthyAgent.set(mostTrustworthyAgentName);
    }

    // Method for obtaining the agent with the highest IT_CR rating
    @OPERATION
    public void findMaxCR(Object[] ITList, Object[] CRList, OpFeedbackParam<String> mostTrustworthyAgent) {
        ArrayList<InteractionTrust> ITArrayList = parseITList(ITList);
        ArrayList<CertificationReputation> CRArrayList = parseCRList(CRList);

        HashMap<String, Double> avgITRatingMap = getAvgITRatingMap(ITArrayList);
        HashMap<String, Double> avgCRRatingMap = getAvgCRRatingMap(CRArrayList);

        System.out.println("avgITRatingMap: " + avgITRatingMap.toString());
        System.out.println("avgCRRatingMap: " + avgCRRatingMap.toString());

        // Find the targetAgent with the highest IT_CR rating
        double maxIT_CRRating = 0;
        String mostTrustworthyAgentName = "";
        for (String targetAgent : avgITRatingMap.keySet()) {
            double avgITRating = avgITRatingMap.get(targetAgent);
            double avgCRRating = avgCRRatingMap.get(targetAgent);
            double IT_CRRating = 0.5 * avgITRating + 0.5 * avgCRRating;
            System.out.println("IT_CR rating for agent " + targetAgent + ": " + IT_CRRating);
            if (IT_CRRating > maxIT_CRRating) {
                maxIT_CRRating = IT_CRRating;
                mostTrustworthyAgentName = targetAgent;
            }
        }

        mostTrustworthyAgent.set(mostTrustworthyAgentName);
    }

    // Method for obtaining the agent with the highest IT_CR_WR rating
    @OPERATION
    public void findMaxWR(Object[] ITList, Object[] CRList, Object[] WRList, OpFeedbackParam<String> mostTrustworthyAgent) {

        ArrayList<InteractionTrust> ITArrayList = parseITList(ITList);
        ArrayList<CertificationReputation> CRArrayList = parseCRList(CRList);
        ArrayList<WitnessReputation> WRArrayList = parseWRList(WRList);

        HashMap<String, Double> avgITRatingMap = getAvgITRatingMap(ITArrayList);
        HashMap<String, Double> avgCRRatingMap = getAvgCRRatingMap(CRArrayList);
        HashMap<String, Double> avgWRRatingMap = getAvgWRRatingMap(WRArrayList);

        System.out.println("avgITRatingMap: " + avgITRatingMap.toString());
        System.out.println("avgCRRatingMap: " + avgCRRatingMap.toString());
        System.out.println("avgWRRatingMap: " + avgWRRatingMap.toString());

         // build the set of all agents we need to consider
        Set<String> allAgents = new HashSet<>();
        allAgents.addAll(avgITRatingMap.keySet());
        allAgents.addAll(avgCRRatingMap.keySet());
        allAgents.addAll(avgWRRatingMap.keySet());

        // seed with NEGATIVE_INFINITY so that even all-negative scores get picked
        double   maxCombinedRating     = Double.NEGATIVE_INFINITY;
        String   bestAgent             = null;

        // weight each component equally (1/3)
        final double w = 1.0 / 3.0;

        for (String agent : allAgents) {
            double it = avgITRatingMap.getOrDefault(agent, 0.0);
            double cr = avgCRRatingMap.getOrDefault(agent, 0.0);
            double wr = avgWRRatingMap.getOrDefault(agent, 0.0);

            double combined = w * it + w * cr + w * wr;
            System.out.println(
            String.format("Combined rating for %s: %.3f (IT=%.3f, CR=%.3f, WR=%.3f)",
                            agent, combined, it, cr, wr)
            );

            if (combined > maxCombinedRating) {
                maxCombinedRating = combined;
                bestAgent         = agent;
            }
        }

        // finally, return the winner
        mostTrustworthyAgent.set(bestAgent);
    }

    @OPERATION
    public void getTempReadingByAgent(String agentName, Object[] tempReadings, OpFeedbackParam<Double> tempReading) {
        ArrayList<Object> tempReadingList = new ArrayList<Object>(Arrays.asList(tempReadings));
        boolean found = false;

        System.out.println("DEBUG - Looking for temperature from agent: " + agentName);
        System.out.println("DEBUG - Available readings: " + Arrays.deepToString(tempReadings));

        // Find the temperature reading for the given agent
        for (Object entry : tempReadingList) {
            Object[] tempReadingEntry = (Object[]) entry;
            double temp = ((Number) tempReadingEntry[0]).doubleValue();
            String agent = (String) tempReadingEntry[1];
            System.out.println("DEBUG - Checking " + agent + " with temp " + temp);
            
            if (agent.equals(agentName)) {
                tempReading.set(temp);
                found = true;
                System.out.println("DEBUG - Found temperature reading for agent " + agentName + ": " + temp);
                break;
            }
        }

        if (!found) {
            System.out.println("DEBUG - No temperature reading found for agent " + agentName);
            tempReading.set(0.0); // Set default value if no reading found
        }
    }

}