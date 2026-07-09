# Script to recode retrieval behavioural measures for LoC participants with the early retrieval-task bug.
# Fixes: cue old/new accuracy, ending-memory accuracy, AFC accuracy, and basic QC flags.

from pathlib import Path

import numpy as np
import pandas as pd

# -------------------------
# Configuration
# -------------------------
DATA_DIR = Path('C:/Users/noahm/projects/loc_analysis/data/raw')

OUTPUT_ROOT = Path('C:/Users/noahm/projects/loc_analysis/data/bids')


# Python ranges exclude the stop value; this processes 3..22 inclusive.
PARTICIPANTS = range(2, 45)


# Decide what "¿Ocurrió algo inesperado?" meant in your task.
# Use "high_only" if unexpected = semantic/action incongruency only.
# Use "high_or_low" if participants were supposed to answer yes for either high-level or low-level violations.
ENDING_RULE = "high_or_low"

# Missing responses: for accuracy variables, 0 is usually safest for behavioural accuracy.
# The original response is still retained, so you can later recode missing as NaN if desired.
MISSING_RESPONSE_ACC = 0


def first_csv(folder: Path) -> Path | None:
    """Return the only/latest CSV in a folder, with a warning if there is more than one."""
    files = sorted(folder.glob('*.csv'), key=lambda p: p.stat().st_mtime)
    if not files:
        return None
    if len(files) > 1:
        print(f'WARNING: {folder} contains {len(files)} CSV files. Using latest: {files[-1].name}')
    return files[-1]


def keep_existing(df: pd.DataFrame, cols: list[str], label: str) -> pd.DataFrame:
    """Keep requested columns that exist, warning about absent ones."""
    cols_unique = list(dict.fromkeys(cols))
    missing = [c for c in cols_unique if c not in df.columns]
    if missing:
        print(f'WARNING: missing {label} columns: {missing}')
    present = [c for c in cols_unique if c in df.columns]
    return df[present].copy()


def clean_key(series: pd.Series) -> pd.Series:
    """Normalize PsychoPy key strings while preserving missing values."""
    return series.astype('string').str.strip().str.lower()


def to_num(series: pd.Series) -> pd.Series:
    return pd.to_numeric(series, errors='coerce')


for participant in PARTICIPANTS:
    print('\n' + '=' * 72)
    print(f'Participant {participant:02d}')

    try:
        enc_folder = DATA_DIR / f's{participant:02d}' / 'loc_label-encoding'
        ret_folder = DATA_DIR / f's{participant:02d}' / 'loc_label-retrieval'

        enc_file = first_csv(enc_folder)
        ret_file = first_csv(ret_folder)

        if enc_file is None:
            print(f'No encoding CSV found in {enc_folder}. Skipping.')
            continue
        if ret_file is None:
            print(f'No retrieval CSV found in {ret_folder}. Skipping.')
            continue

        enc_data = pd.read_csv(enc_file)
        ret_data = pd.read_csv(ret_file)

        # Keep trial rows only.
        enc_data = enc_data[enc_data['cue_file'].notna()].copy()
        ret_data = ret_data[ret_data['cue_file'].notna()].copy()

        # Remove practice rows from encoding if the practice column exists.
        if 'practice_trials.target_resp.keys' in enc_data.columns:
            enc_data = enc_data[enc_data['practice_trials.target_resp.keys'].isna()].copy()

        enc_data = enc_data.dropna(axis=1, how='all')
        ret_data = ret_data.dropna(axis=1, how='all')

        enc_cols = [
            'cue_file', 'condition_id', 'low_prediction', 'high_prediction', 'direction',
            'scn_type', 'object', 'schema', 'relationship', 'target_file', 'target_sat',
            'corr_ans_side', 'corr_label', 'question_type', 'cue_sat',
            'target_resp.keys', 'target_resp.corr', 'target_resp.rt', 'list_id', 'story_set_id',
            'comic_name', 'trials.thisTrialN'
        ]
        ret_cols = [
            'cue_file', 'OvsN', 'OvsN_code', 'cue_rec_resp.keys', 'cue_rec_resp.corr',
            'cue_rec_resp.rt', 'cue_rec_corr',
            'ending_rec_resp.keys', 'ending_rec_resp.corr', 'ending_rec_resp.rt',
            'afc_resp.keys', 'afc_resp.corr', 'afc_resp.rt',
            'target_file', 'target_sat',
            'choice_one', 'choice_one_sat', 'choice_two', 'choice_two_sat',
            'choice_three', 'choice_three_sat', 'choice_four', 'choice_four_sat',
            'afc_corr', 'high_prediction', 'low_prediction', 'direction', 'thisTrialN'
        ]

        enc_data = keep_existing(enc_data, enc_cols, 'encoding')
        ret_data = keep_existing(ret_data, ret_cols, 'retrieval')

        # -------------------------
        # Cue recognition: old/new
        # -------------------------
        # In the buggy task, cue_rec_corr means "side where Sí/old was displayed",
        # not the correct key for new trials.
        yes_side = clean_key(ret_data['cue_rec_corr'])
        cue_key = clean_key(ret_data['cue_rec_resp.keys'])
        ovsn = clean_key(ret_data['OvsN'])

        ret_data['cue_yes_side'] = yes_side
        ret_data['cue_recoded_response'] = pd.Series(pd.NA, index=ret_data.index, dtype='string')
        responded_cue = cue_key.notna()
        ret_data.loc[responded_cue & (cue_key == yes_side), 'cue_recoded_response'] = 'old'
        ret_data.loc[responded_cue & (cue_key != yes_side), 'cue_recoded_response'] = 'new'

        ret_data['cue_recog_acc'] = MISSING_RESPONSE_ACC
        valid_cue = responded_cue & ovsn.isin(['old', 'new'])
        ret_data.loc[valid_cue, 'cue_recog_acc'] = (
            ret_data.loc[valid_cue, 'cue_recoded_response'].astype('string') == ovsn.loc[valid_cue]
        ).astype(int)

        # -------------------------
        # Merge encoding and retrieval
        # -------------------------
        # Prefix encoding columns so we never accidentally use retrieval-list variables as ground truth.
        enc_unique = enc_data.drop_duplicates(subset=['cue_file'], keep='first').copy()
        enc_unique = enc_unique.rename(columns={c: f'enc_{c}' for c in enc_unique.columns if c != 'cue_file'})
        
        # Rename trial counts to avoid confusion
        enc_unique = enc_unique.rename(columns={'enc_trials.thisTrialN': 'enc_trial_count'})
        ret_data = ret_data.rename(columns={'thisTrialN': 'ret_trial_count'})
        
        # Add +1 to trial counts to make them 1-indexed instead of 0-indexed.
        enc_unique['enc_trial_count'] = enc_unique['enc_trial_count'] + 1
        ret_data['ret_trial_count'] = ret_data['ret_trial_count'] + 1

        merged = ret_data.merge(enc_unique, on='cue_file', how='left', indicator='encoding_match')
        merged['encoding_match'] = merged['encoding_match'].astype(str)

        old_mask = clean_key(merged['OvsN']) == 'old'
        n_old = int(old_mask.sum())
        n_old_matched = int((old_mask & (merged['encoding_match'] == 'both')).sum())
        print(f'Old retrieval trials matched to encoding: {n_old_matched}/{n_old}')

        # -------------------------
        # Ending memory
        # -------------------------
        end_key = clean_key(merged.get('ending_rec_resp.keys', pd.Series(pd.NA, index=merged.index)))
        enc_high = to_num(merged.get('enc_high_prediction', pd.Series(np.nan, index=merged.index)))
        enc_low = to_num(merged.get('enc_low_prediction', pd.Series(np.nan, index=merged.index)))

        if ENDING_RULE == 'high_only':
            unexpected = enc_high.eq(1)
        elif ENDING_RULE == 'high_or_low':
            unexpected = enc_high.eq(1) | enc_low.eq(1)
        else:
            raise ValueError('ENDING_RULE must be "high_only" or "high_or_low"')

        merged['ending_correct_key_recomputed'] = np.where(unexpected, 'left', 'right')
        # np.select() needs plain boolean ndarrays. Pandas StringDtype comparisons
        # can return nullable BooleanArrays, which trigger:
        # "invalid entry 0 in condlist: should be boolean ndarray".
        end_is_left = end_key.eq('left').fillna(False).to_numpy(dtype=bool)
        end_is_right = end_key.eq('right').fillna(False).to_numpy(dtype=bool)
        merged['ending_recoded_response'] = np.select(
            [end_is_left, end_is_right],
            ['yes', 'no'],
            default=pd.NA
        )
        merged['outcome_memory_acc'] = np.nan
        valid_end = old_mask & end_key.isin(['left', 'right']) & merged['encoding_match'].eq('both')
        merged.loc[valid_end, 'outcome_memory_acc'] = (
            end_key.loc[valid_end] == merged.loc[valid_end, 'ending_correct_key_recomputed'].astype('string')
        ).astype(int)
        # No response on an old matched trial counts as incorrect unless you later choose to treat it as missing.
        no_end_response = old_mask & end_key.isna() & merged['encoding_match'].eq('both')
        merged.loc[no_end_response, 'outcome_memory_acc'] = MISSING_RESPONSE_ACC

        # -------------------------
        # AFC memory
        # -------------------------
        afc_key = to_num(merged.get('afc_resp.keys', pd.Series(np.nan, index=merged.index)))
        merged['afc_key_num'] = afc_key

        for col in ['choice_one_sat', 'choice_two_sat', 'choice_three_sat', 'choice_four_sat', 'enc_target_sat']:
            if col in merged.columns:
                merged[col] = to_num(merged[col])

        merged['afc_chosen_file'] = pd.NA
        merged['afc_chosen_sat'] = np.nan
        choice_map = {
            1: ('choice_one', 'choice_one_sat'),
            2: ('choice_two', 'choice_two_sat'),
            3: ('choice_three', 'choice_three_sat'),
            4: ('choice_four', 'choice_four_sat'),
        }
        for key, (file_col, sat_col) in choice_map.items():
            if file_col in merged.columns and sat_col in merged.columns:
                m = afc_key.eq(key)
                merged.loc[m, 'afc_chosen_file'] = merged.loc[m, file_col]
                merged.loc[m, 'afc_chosen_sat'] = merged.loc[m, sat_col]

        # QC: was the true encoded target actually present among the 4 displayed options?
        target_file = merged.get('enc_target_file', pd.Series(pd.NA, index=merged.index)).astype('string')
        target_sat = to_num(merged.get('enc_target_sat', pd.Series(np.nan, index=merged.index)))
        option_matches = []
        for file_col, sat_col in choice_map.values():
            if file_col in merged.columns and sat_col in merged.columns:
                option_matches.append(
                    (merged[file_col].astype('string').eq(target_file) & to_num(merged[sat_col]).eq(target_sat))
                    .fillna(False)
                    .to_numpy(dtype=bool)
                )
        if option_matches:
            merged['afc_target_present'] = np.logical_or.reduce(option_matches)
        else:
            merged['afc_target_present'] = False

        merged['afc_story_acc'] = np.nan
        merged['afc_sat_acc'] = np.nan
        merged['afc_acc'] = np.nan
        valid_afc = old_mask & afc_key.isin([1, 2, 3, 4]) & merged['encoding_match'].eq('both') & merged['afc_target_present']
        merged.loc[valid_afc, 'afc_story_acc'] = (
            merged.loc[valid_afc, 'afc_chosen_file'].astype('string') == target_file.loc[valid_afc]
        ).astype(int)
        merged.loc[valid_afc, 'afc_sat_acc'] = (
            to_num(merged.loc[valid_afc, 'afc_chosen_sat']) == target_sat.loc[valid_afc]
        ).astype(int)
        merged.loc[valid_afc, 'afc_acc'] = (
            merged.loc[valid_afc, 'afc_story_acc'].eq(1) & merged.loc[valid_afc, 'afc_sat_acc'].eq(1)
        ).astype(int)
        no_afc_response = old_mask & afc_key.isna() & merged['encoding_match'].eq('both') & merged['afc_target_present']
        merged.loc[no_afc_response, ['afc_story_acc', 'afc_sat_acc', 'afc_acc']] = MISSING_RESPONSE_ACC

        n_afc_present = int((old_mask & merged['afc_target_present']).sum())
        print(f'Old AFC trials where encoded target was present: {n_afc_present}/{n_old}')

        # -------------------------
        # Summary
        # -------------------------
        for label, mask in [('old', ovsn.eq('old')), ('new', ovsn.eq('new'))]:
            if mask.sum() > 0:
                print(f'Cue recognition acc ({label}): {ret_data.loc[mask, "cue_recog_acc"].mean():.3f}')

        old_merged = merged[old_mask]
        print(f'Outcome memory acc: {old_merged["outcome_memory_acc"].mean(skipna=True):.3f}')
        print(f'AFC full acc:        {old_merged["afc_acc"].mean(skipna=True):.3f}')
        print(f'AFC story acc:       {old_merged["afc_story_acc"].mean(skipna=True):.3f}')
        print(f'AFC saturation acc:  {old_merged["afc_sat_acc"].mean(skipna=True):.3f}')

        # -------------------------
        # Save
        # -------------------------
        output_dir = OUTPUT_ROOT / f'sub-{participant:02d}' / 'beh'
        output_dir.mkdir(parents=True, exist_ok=True)
        updated_file = output_dir / f'sub-{participant:02d}_task-loc_label-merged_beh.csv'
        
        # Before saving, we'll rename a few columns for the sake of clarity
        merged = merged.rename(columns={
            'cue_recoded_response': 'cue_recog_response',
            'enc_target_resp.corr': 'enc_acc',
            'ending_recoded_response': 'outcome_response',
            'afc_key_num': 'afc_response_num',
            'afc_chosen_file': 'afc_response_story',
            'afc_chosen_sat': 'afc_response_sat', 
            'OvsN_code': 'ret_trial_type_code',
            'OvsN': 'ret_trial_type'
        })
        
        # Create a new column called 'participant_id' to keep track of which participant the data belongs to
        merged['participant_id'] = participant
        
        # Now, we'll keep only the columns that are relevant for analysis
        # Also, we will reorder the columns to a more logical order for analysis
        columns_to_keep = [
            'participant_id', 'cue_file', 'enc_trial_count', 'enc_condition_id', 'enc_low_prediction', 'enc_high_prediction', 'enc_direction',
            'enc_scn_type', 'enc_target_file', 'enc_target_sat', 'enc_acc', 'ret_trial_count',
            'ret_trial_type', 'ret_trial_type_code', 'cue_recog_response', 'cue_recog_acc', 'outcome_response', 'outcome_memory_acc',
            'afc_response_num', 'afc_response_story', 'afc_response_sat', 'afc_acc', 'afc_story_acc', 'afc_sat_acc'
        ]
        merged = merged[[col for col in columns_to_keep if col in merged.columns]]
        
        # Sort the dataframe by the encoding trial count for easier analysis
        merged = merged.sort_values('enc_trial_count')
        
        # To label the rows where the bug caused missing responses for outcome and afc, we must first locate the rows where enc_target_file is not null
        bug_rows = merged['enc_target_file'].notnull()
        
        # Now, we will fill only empty entries in those bug rows with "9999".
        # Do this separately for string and numeric columns to avoid type errors.
        for col in ['outcome_response', 'afc_response_story']:
            empty_mask = merged[col].isna() | (merged[col] == '')
            merged.loc[bug_rows & empty_mask, col] = '9999'

        for col in ['outcome_memory_acc', 'afc_response_num', 'afc_response_sat', 'afc_acc', 'afc_story_acc', 'afc_sat_acc']:
            empty_mask = merged[col].isna() | (merged[col] == '')
            merged.loc[bug_rows & empty_mask, col] = 9999

        # Save
        merged.to_csv(updated_file, index=False)
        print(f'Saved: {updated_file}')

    except Exception as e:
        print(f'ERROR for participant {participant:02d}: {e}')
        continue
