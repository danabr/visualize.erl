%% Parse transform to analyze which processes communicate with each other.
-compile({parse_transform, process_communication_analysis_transform}).
