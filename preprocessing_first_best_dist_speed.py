import numpy as np
import pandas as pd
import dec_util
from pprint import pprint


# Load data.
df = pd.read_csv("DecData.csv")
df_original = df.copy(deep=True)

# ================ Preprocessing ================
df = dec_util.add_year_column(df)
df["Date"] = pd.to_datetime(df["Date"])
df["Men 1500"] = df["Men 1500"].apply(dec_util.time_seconds_str)

# Check to make sure the date/year conversion worked.
print(f"{df_original.loc[0, 'Date']} should translate into {2018}.")
print(df_original.loc[0, 'Date'].split('/')[-1] == "2018")
print(f"{df_original.loc[100, 'Date']} should translate into {2002}.")
print(df_original.loc[100, 'Date'].split('/')[-1] == "2002")

# Check to make sure that all worked.
print(f"{df_original.loc[0, 'Men 1500']} should translate into {240 + 36.1}")
print(df.loc[0, "Men 1500"] == 240 + 36.1)
print(f"{df_original.loc[245, 'Men 1500']} should translate into {240 + 36.7}")
print(df.loc[245, 'Men 1500'] == 240 + 36.7)

# ================ Convert Times to Speeds ================

def convert_time_to_speed(df, event, dist):
    """
    Converts a time in seconds to a distance in
    m/s for the specified column.
    """
    df[event] = dist / df[event]

[convert_time_to_speed(df, event.replace("_", " "), dist)
 for event, dist in dec_util.events_lower_is_better_und_dists.items()]

# Tests.
print(f"{df_original.loc[367, 'Men 100']} should translate into {9.541984732824426} which is also {df.loc[367, 'Men 100']}")
print(100/df_original.loc[367, 'Men 100'] == 9.541984732824426 == df.loc[367, 'Men 100'])
print(f"{df_original.loc[222, 'Men 110h']} should translate into {7.8125} which is also {df.loc[222, 'Men 110h']}")
print(110/df_original.loc[222, 'Men 110h'] == 7.8125 == df.loc[222, 'Men 110h'])
print(f"{df_original.loc[1026, 'Men 400']} should translate into {8.176614881439084} which is also {df.loc[1026, 'Men 400']}")
print(400/df_original.loc[1026, 'Men 400'] == 8.176614881439084 == df.loc[1026, 'Men 400'])
print(f"{df_original.loc[2222, 'Men 1500']} should translate into {5.466472303206998} which is also {df.loc[2222, 'Men 1500']}")
print(1500/(float(df_original.loc[2222, 'Men 1500'].split(":")[0]) * 60 + float(df_original.loc[2222, 'Men 1500'].split(":")[1])) == 5.466472303206998 == df.loc[2222, 'Men 1500'])

# ================ Filter athletes ================
# Filter the athletes so that we only grab those that competed for at least four distinct years
# doesn't have to be consecutive.
df_filtered = dec_util.filter_by_first_season(
    df,
    min_years_active=4,
    first_year_pr_bounds=(6000, 100000),
    v=1
)

# Check to make sure this all worked.
print(df[df.Competitor=="Tim NOWAK"]["Year"].unique())
print(df[df.Competitor=="Tim NOWAK"][["Year", "Overall Score"]])
# This guy competed for 7 unique years and the first time he came on the scene
# he got 7827 overall. He should be in the filtered DataFrame.
print("Tim NOWAK" in df_filtered.Competitor.values)
# Ensure that ALL of his performances were collected from the original DataFrame
# and put in the new one.
print(df[df.Competitor == "Tim NOWAK"].shape[0] == df_filtered[df_filtered.Competitor == "Tim NOWAK"].shape[0])
print(df_original[df_original.Competitor == "Tim NOWAK"].shape[0] == df_filtered[df_filtered.Competitor == "Tim NOWAK"].shape[0])

print(df[df.Competitor == "Mingyu SONG"]["Year"].unique())
print(df[df.Competitor == "Mingyu SONG"][["Year", "Overall Score"]])
# This guy should also be in.
print("Mingyu SONG" in df_filtered.Competitor.values)

# Ensure that ALL of Mingyu's marks were kept.
print(df[df.Competitor == "Mingyu SONG"].shape[0] == df_filtered[df_filtered.Competitor == "Mingyu SONG"].shape[0])
print(df_original[df_original.Competitor == "Mingyu SONG"].shape[0] == df_filtered[df_filtered.Competitor == "Mingyu SONG"].shape[0])

print(df[df.Competitor == "Anthony HOGG"]["Year"].unique())
# This guy should not be in because he only competed for one year.
print("Anthony HOGG" not in df_filtered.Competitor.values)

# Check to make sure it all works.
df[df.Competitor == "Tim NOWAK"][dec_util.event_groups["sprints"]]
# Best velocities are 100m: 9.017133 m/s (achieved twice). Best 110h: 7.534247 m/s. Best 400m: 8.1799591002045 m/s.
df[df.Competitor=="Tim NOWAK"][dec_util.event_groups["jumps"]]
df[df.Competitor=="Tim NOWAK"][dec_util.event_groups["throws"]]
df[df.Competitor=="Tim NOWAK"][dec_util.event_groups["endurance"]]

tim_nowak_bests_manual = {
    "Men 100": 9.017132551848512,  # Achieved twice.
    "Men 110h": 7.534246575342466,
    "Men 400": 8.1799591002045,  # I mismarked this as 49.29.
    "Men LJ": 7.56,
    "Men HJ": 2.07,
    "Men PV": 5.00,  # Achieved twice.
    "Men SP": 15.17,
    "Men DT": 47.27,  # I mismarked this as 45.32.
    "Men JT": 64.00,
    "Men 1500": 5.753739930955121
}

# Leave this; this is important. Why don't his PRs found in df_original match df_filtered?
# Note: fixed.
tim_nowak_bests_match = [df_filtered[df_filtered.Competitor=="Tim NOWAK"][col].max() == val
                         # if col in dec_util.events_higher_is_better
                         # else df_filtered[df_filtered.Competitor=="Tim NOWAK"][col].min() == val
                         for col, val in tim_nowak_bests_manual.items()]
print(np.array(tim_nowak_bests_match).all())

# ================ First-Season Event PRs ================
# Find first-season PRs for all athletes.
df_first = pd.DataFrame([dec_util.athlete_performance_profile_relative(
    df_filtered,
    ath,
    max_relative_year=0,
    events_higher_is_better=dec_util.events)
                         for ath in list(df_filtered.Competitor.unique())])

# Check to make sure first-season bests are collected correctly.
df_original[df_original.Competitor == "Tom PAPPAS"]
df[df.Competitor == "Tom PAPPAS"]["Year"].min()
# Tom first competed in 2001. Let's see what his best marks were in 2001 and compare to
# what was recorded in df_first.
df[(df.Competitor == "Tom PAPPAS") & (df.Year == 2001)][dec_util.events]

tom_pappas_fsb_manual = {
    "Men 100": 9.22509225092251,
    "Men 110h": 7.829181494661921,
    "Men 400": 7.957032027053908,
    "Men LJ": 7.24,
    "Men HJ": 2.18,
    "Men PV": 4.9,
    "Men SP": 15.3,
    "Men DT": 45.43,
    "Men JT": 62.19,
    "Men 1500": 5.048805116122518
}

tom_pappas_fsb_match = [df_first[df_first.Competitor == "Tom PAPPAS"][f"year_0_{col}"].iloc[0] == val
                        for col, val in tom_pappas_fsb_manual.items()]
print(np.all(tom_pappas_fsb_match))

# So we can do the same for an athlete that competed multiple times during their first
# season, let's take a look at Roman Šebrle.
df[df.Competitor == "Roman ŠEBRLE"]["Year"].min()
df[(df.Competitor == "Roman ŠEBRLE") & (df.Year == 2001)][dec_util.events]

roman_sebrle_fsb_manual = {
    "Men 100": 9.398496240601503,  # Came from observation 3.
    "Men 110h": 7.902298850574713,  # Came from obs. 3.
    "Men 400": 8.369951872776731,  # Came from obs. 3.
    "Men LJ": 8.11,  # Came from obs. 3.
    "Men HJ": 2.12,  # Came from obs. 3.
    "Men PV": 4.8,  # Came from obs. 3.
    "Men SP": 15.43,  # Came from obs. 583.
    "Men DT": 47.92,  # Came from obs. 3.
    "Men JT": 70.16,  # Came from obs. 3.
    "Men 1500": 5.7251908396946565  # Came from obs. 3.
}

roman_sebrle_fsb_match = [df_first[df_first.Competitor == "Roman ŠEBRLE"][f"year_0_{col}"].iloc[0] == val
                          for col, val in roman_sebrle_fsb_manual.items()]
print(np.all(roman_sebrle_fsb_match))

# Let's try someone who got their first-season best marks spread more between two different
# competitions. Damian Warner is a good example.
df[df.Competitor == "Damian WARNER"]["Year"].min()
df[(df.Competitor == "Damian WARNER") & (df.Year == 2011)][dec_util.events]

damian_warner_fsb_manual = {
    "Men 100": 9.606147934678194,  # From obs. 791.
    "Men 110h": 7.930785868781543,  # From obs. 791.
    "Men 400": 8.173273395995096,  # From obs. 791.
    "Men LJ": 7.43,  # From obs. 2143.
    "Men HJ": 2.03,  # From obs. 791.
    "Men PV": 4.5,  # From obs. 1820.
    "Men SP": 13.60,  # From obs. 791.
    "Men DT": 45.67,  # From obs. 791.
    "Men JT": 62.04,  # From obs. 2857.
    "Men 1500": 5.270555165144062  # From obs. 791.
}

damian_warner_fsb_match = [df_first[df_first.Competitor == "Damian WARNER"][f"year_0_{col}"].iloc[0] == val
                           for col, val in damian_warner_fsb_manual.items()]
print(np.all(damian_warner_fsb_match))

# I call that good.

# One more new comparison:
fsb_check_df = [
    df[(df.Competitor == ath) & (df.Year == df[df.Competitor == ath]["Year"].min())][dec_util.events].max(axis=0)
    for ath in df_filtered.Competitor.unique()
]
fsb_check_df_first = [
    df_first[df_first.Competitor == ath].iloc[0, 1:]
    for ath in df_filtered.Competitor.unique()
]
fsb_check_a = [fsb_check_df[i].values == fsb_check_df_first[i].values for i in range(len(fsb_check_df))]
fsb_check_b = [np.all(fsb_check_df[i].values == fsb_check_df_first[i].values) for i in range(len(fsb_check_df))]
print(fsb_check_a)
print(np.all(fsb_check_a))
print(fsb_check_b)
print(np.all(fsb_check_b))

# ================ All-Time Event PRs ================
# Find all-time PRs for all athletes.
df_best = pd.DataFrame(
        [dec_util.get_all_time_bests(
            df_filtered,
            ath,
            # invert_cols=dec_util.events_lower_is_better,
            invert_cols=[],
            event_name_suf="",
            include_relative_years=False)
         for ath in list(df_filtered.Competitor.unique())]
)

# Check to make sure everything worked out. Let's pick Payton Lewis as one of our
# test cases.
df[df.Competitor == "Payton LEWIS"][dec_util.events]

payton_lewis_ateb = {
    "Men 100": 8.936550491510278,  # Occurred in 9153 and 9154.
    "Men 110h": 7.348029392117568,  # Occurred in 9153 and 9154.
    "Men 400": 7.493443237167478,  # Occurred in 10890.
    "Men LJ": 6.71,  # Occurred in 10890.
    "Men HJ": 1.86,  # Occurred in 9153 and 9154.
    "Men PV": 5.00,  # Occurred in 12761.
    "Men SP": 13.34,  # Occurred in 11029.
    "Men DT": 41.19,  # Occurred in 11029.
    "Men JT": 44.30,  # Occurred in 12408.
    "Men 1500": 4.863813229571985,  # Occurred in 308.4 seconds
}

payton_lewis_match = [df_best[df_best.Competitor == "Payton LEWIS"][f"{col} all time best"].iloc[0] == val
                      for col, val in payton_lewis_ateb.items()]
print(np.all(payton_lewis_match))

ateb_check_df = [
    df[df.Competitor == ath][dec_util.events].max(axis=0)
    for ath in df_filtered.Competitor.unique()
]
ateb_check_df_best = [
    df_best[df_best.Competitor == ath].iloc[0, 1:]
    for ath in df_filtered.Competitor.unique()
]
ateb_check_a = [ateb_check_df[i].values == ateb_check_df_best[i].values for i in range(len(ateb_check_df_best))]
ateb_check_b = [np.all(ateb_check_df[i].values == ateb_check_df_best[i].values) for i in range(len(ateb_check_df_best))]
print(np.all(ateb_check_a))
print(np.all(ateb_check_b))

# ================ Combine DataFrames ================
# First, copy the DataFrames so we can double-check that everything is correct later.
df_first_old = df_first.copy(deep=True)
df_best_old = df_best.copy(deep=True)

# Reindex the dataframes by athlete.
df_first.set_index("Competitor", inplace=True)
df_best.set_index("Competitor", inplace=True)

# Let's make sure that everything went well there.
df_first_old[df_first_old.Competitor == "Robin COOPS"].values[0, 1:] == df_first.loc["Robin COOPS"].values

# Join the dataframes.
df_first_best = pd.concat([df_first, df_best], axis=1)

# Make sure that the data for firsts and bests were matched correctly by
# athlete.
print(np.all(df_first.index == df_best.index))
print(np.all(df_first_best == df_first.join(df_best)))

# Now that we've joined the dataframes using the athlete as the index, we can be
# pretty darn certain that nothing went wrong. We're going to reestablish the
# original index again to make our lives easier during subsequent analysis.
df_first_best.reset_index(inplace=True)

# Fix column names.
new_col_names = {
        col: col.replace("year_0_", "").replace(" ", "_") + "_first"
        if "year_0" in col
        else col.replace("all time best", "best").replace(" ", "_")
        for col in df_first_best.columns
        }
df_first_best.rename(mapper=new_col_names, inplace=True, axis=1)

# Check to make sure values still line up.
robin_coops_first_best = df_first_best[df_first_best.Competitor == "Robin COOPS"].values[0, 1:]
robin_coops_first_best_via_append = np.append(df_first.loc["Robin COOPS"].values, df_best.loc["Robin COOPS"].values)

print(np.all(robin_coops_first_best == robin_coops_first_best_via_append)) 

mingyu_song_first_best = df_first_best[df_first_best.Competitor == "Mingyu SONG"].values[0, 1:]
mingyu_song_first_best_via_append = np.append(df_first.loc["Mingyu SONG"].values, df_best.loc["Mingyu SONG"].values)

print(np.all(mingyu_song_first_best == mingyu_song_first_best_via_append))

# Looks good.

# ================ Compute Deltas ================
# Compute deltas.
[df_first_best.insert(df_first_best.shape[1], f"{event}_delta", df_first_best[event + "_best"] - df_first_best[event + '_first'])
 for event in dec_util.events_und]

# Check some of the deltas.
damian_warner_100_delta = df_first_best[df_first_best.Competitor == "Damian WARNER"]["Men_100_delta"].iloc[0]
damian_warner_100_fsb = df_first_best[df_first_best.Competitor == "Damian WARNER"]["Men_100_first"].iloc[0]
damian_warner_100_best = df_first_best[df_first_best.Competitor == "Damian WARNER"]["Men_100_best"].iloc[0]
print(damian_warner_100_delta == damian_warner_100_best - damian_warner_100_fsb)

erki_nool_LJ_delta = df_first_best[df_first_best.Competitor == "Erki NOOL"]["Men_LJ_delta"].iloc[0]
erki_nool_LJ_fsb = df_first_best[df_first_best.Competitor == "Erki NOOL"]["Men_LJ_first"].iloc[0]
erki_nool_LJ_best = df_first_best[df_first_best.Competitor == "Erki NOOL"]["Men_LJ_best"].iloc[0]
print(erki_nool_LJ_delta == erki_nool_LJ_best - erki_nool_LJ_fsb)

# Drop all the 'best' columns now that we have the deltas.
df_first_delta = df_first_best.drop(columns=[col for col in df_first_best.columns
                                             if "best" in col])
# df_first_best.drop(columns=[col for col in df_first_best.columns if "best" in col], inplace=True)

# Make sure that all worked.
# Let's take a look at Aaron Page, the very last athlete in the dataset.
df_first_old[df_first_old.Competitor == "Aaron PAGE"].iloc[0]
df_best_old[df_best_old.Competitor == "Aaron PAGE"].iloc[0]
df_first_best[df_first_best.Competitor == "Aaron PAGE"]["Men_JT_delta"].values[0] == 45.91 - 45.91
# Looks good. Let's take a different athlete and look at an event in which having a
# smaller mark is better.
df_first_old[df_first_old.Competitor == "Akihiko NAKAMURA"].iloc[0]
df_best_old[df_best_old.Competitor == "Akihiko NAKAMURA"].iloc[0]
print(df_first_best[df_first_best.Competitor == "Akihiko NAKAMURA"]["Men_400_delta"].values[0] == 8.479966080135679 - 8.271298593879239)
print(df_first_best[df_first_best.Competitor == "Akihiko NAKAMURA"]["Men_400_delta"].values[0])

# That appears to be working correctly.

# Make sure columns are correct in df_first_delta.
print(np.all(["best" not in col for col in df_first_delta.columns]))

# ================ Fetch All-Time Overall PBs ================
# Get the all-Time overall score PBs for each athlete.
df_first_best["All_Time_PB"] = [
        dec_util.get_all_time_overall_PR(df, ath)
        for ath in df_first_best.Competitor
        ]
df_first_delta["All_Time_PB"] = [
        dec_util.get_all_time_overall_PR(df, ath)
        for ath in df_first_delta.Competitor
        ]

# Make sure that all worked.
df[df.Competitor == "Akihiko NAKAMURA"][["Competitor", "Year", "Overall Score"]]
df[df.Competitor == "Akihiko NAKAMURA"]["Overall Score"].max()
df_first_best[df_first_best.Competitor == "Akihiko NAKAMURA"]["All_Time_PB"].values[0] == 8180

# Let's test another guy.
df[df.Competitor == "Rudy BOURGUIGNON"]["Overall Score"].max()
df_first_best[df_first_best.Competitor == "Rudy BOURGUIGNON"]["All_Time_PB"].values[0] == 8025

# Check maxes for both.
atpb_check_first_best = [
    (df_first_best[df_first_best.Competitor == ath]["All_Time_PB"].iloc[0]) == (df[df.Competitor == ath]["Overall Score"].max())
    for ath in df_first_best.Competitor
]
atpb_check_first_delta = [
    (df_first_delta[df_first_delta.Competitor == ath]["All_Time_PB"].iloc[0]) == (df[df.Competitor == ath]["Overall Score"].max())
    for ath in df_first_delta.Competitor
]
print(np.all(atpb_check_first_best))
print(np.all(atpb_check_first_delta))

# That looks like it's working very well.

# ================ Compare DataFrame with all data with first/delta DataFrame ================

print(np.all(df_first_delta == df_first_best[df_first_delta.columns]))

# ================ Save Data to Disk ================
# Write the data to disk.
# df_first_best.to_csv("data_first_best_updated.csv")
df_first_best.to_csv("data_first_best_delta_dist_speed.csv")
df_first_delta.to_csv("data_first_best_updated_dist_speed.csv")

# That concludes all of my data preprocessing/wrangling/extraction and my
# verification thereof. I checked each major piece against specific examples to
# make sure that everything was working, and everything appears to be in order.

# Second full review concluded on 7/22/2024 12:48pm.

