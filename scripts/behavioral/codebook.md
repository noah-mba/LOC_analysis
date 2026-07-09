# Final Merged File Codebook

This codebook documents the variables included in the final merged file created by [1_merge_data_subs3-22_reviewed.py].

`participant_id`: Numeric participant identifier added by the script before saving.

`cue_file`: File name or identifier for the cue stimulus used to align encoding and retrieval trials.

`enc_trial_count`: Encoding trial number, recoded to be 1-indexed.

`enc_condition_id`: Trial condition at encoding (1 = , 2 = , 3 = , etc).

`enc_low_prediction`: Encoding-trial low-level prediction/incongruency (0 = , 1 = ).

`enc_high_prediction`: Encoding-trial high-level prediction/incongruency (0 = , 1 = ).

`enc_direction`: Encoding-trial saturation direction (0 = , 1 = ).

`enc_scn_type`: Encoding-trial scene type (0 = , 1 = ).

`enc_target_file`: File name for the target stimulus from the encoding trial.

`enc_target_sat`: Saturation code for the encoding target stimulus.

`enc_acc` : Accuracy for the encoding task.

`ret_trial_count`: Retrieval trial number, recoded to be 1-indexed.

`ret_trial_type`: Retrieval trial type (`old` or `new`).

`ret_trial_type_code`: Retrieval trial type (0 = , 1 = ).

`cue_recog_response`: Participant's old/new cue-recognition response.

`cue_recog_acc`: Cue-recognition accuracy for the old/new judgment (0 = incorrect, 1 = correct).

`outcome_response`: Participant's outcome-memory response ("yes", "no"); empty responses in bug-affected rows are replaced with `9999`.

`outcome_memory_acc`: Outcome-memory accuracy for old trials matched to encoding; coded `1` for correct, `0` for incorrect or missing response, and `9999` in bug-affected rows where the value is missing.

`afc_response_num`: Numeric response option selected in the 4-alternative forced-choice task; coded `1` to `4`, with missing values in bug-affected rows replaced by `9999`.

`afc_response_story`: Story file chosen in the 4-alternative forced-choice task; missing values in bug-affected rows are replaced with `9999`.

`afc_response_sat`: Saturation value associated with the chosen AFC option; missing values in bug-affected rows are replaced with `9999`.

`afc_acc`: Overall AFC accuracy on old trials; coded `1` only when both story and saturation are correct, `0` for incorrect or missing response, and `9999` in bug-affected rows where the value is missing.

`afc_story_acc`: AFC story-identity accuracy on old trials; coded `1` for correct, `0` for incorrect or missing response, and `9999` in bug-affected rows where the value is missing.

`afc_sat_acc`: AFC saturation accuracy on old trials; coded `1` for correct, `0` for incorrect or missing response, and `9999` in bug-affected rows where the value is missing.

