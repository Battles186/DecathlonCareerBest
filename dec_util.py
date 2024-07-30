"""
This module stores helpful functions for analyzing the decathlon.

Sources:
https://matplotlib.org/stable/gallery/color/colormap_reference.html
https://matplotlib.org/stable/tutorials/colors/colormap-manipulation.html
https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.table.html
https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.bar.html
https://matplotlib.org/stable/gallery/misc/table_demo.html#sphx-glr-gallery-misc-table-demo-py
https://www.sportcalculators.com/decathlon-calculator
https://www.geeksforgeeks.org/how-to-sort-a-pandas-dataframe-by-date/#
https://pythonfordatascienceorg.wordpress.com/welch-t-test-python-pandas/
https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.ttest_ind.html
https://www.google.com/search?q=How+to+perform+Welch%27s+t+test+in+Python&client=firefox-b-1-d&sca_esv=574165402&sxsrf=AM9HkKlwrIf5rlJa2O0SnFCpCPAnnhJ_jQ%3A1697561770681&ei=qrwuZcChKZj8ptQP6ZiqmAo&ved=0ahUKEwjAns2yxv2BAxUYvokEHWmMCqMQ4dUDCBA&uact=5&oq=How+to+perform+Welch%27s+t+test+in+Python&gs_lp=Egxnd3Mtd2l6LXNlcnAiJ0hvdyB0byBwZXJmb3JtIFdlbGNoJ3MgdCB0ZXN0IGluIFB5dGhvbjIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwA0igBVAAWABwAXgBkAEAmAEAoAEAqgEAuAEDyAEA4gMEGAAgQYgGAZAGCA&sclient=gws-wiz-serp
https://docs.scipy.org/doc/scipy/reference/stats.html
https://www.google.com/search?q=scipy+welch+t+test&client=firefox-b-1-d&sca_esv=574165402&sxsrf=AM9HkKmsoVtTQaCVkL6giNdt8r3lQcGVVg%3A1697561467018&ei=e7suZdFh9Imm1A_kj4-QCw&ved=0ahUKEwiRi-ehxf2BAxX0hIkEHeTHA7IQ4dUDCBA&uact=5&oq=scipy+welch+t+test&gs_lp=Egxnd3Mtd2l6LXNlcnAiEnNjaXB5IHdlbGNoIHQgdGVzdDIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwAzIKEAAYRxjWBBiwA0joB1AAWABwAXgBkAEAmAEAoAEAqgEAuAEDyAEA4gMEGAAgQYgGAZAGCA&sclient=gws-wiz-serp
https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.ttest_ind.html
"""


import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap
from scipy import stats
from pprint import pprint


# Define events.
events = [
        # Sprints.
        "Men 100", "Men 110h", "Men 400",
        # Jumps.
        "Men LJ", "Men HJ", "Men PV",
        # Throws.
        "Men SP", "Men DT", "Men JT",
        # Distance.
        "Men 1500"
        ]


# Event names with underscores.
events_und = [
        "Men_100", "Men_110h", "Men_400",
        "Men_LJ", "Men_HJ", "Men_PV",
        "Men_SP", "Men_DT", "Men_JT",
        "Men_1500"
]


# The units in which each event is typically given in the DataFrame as provided by Tyler.
# Note that 1500m times need to be converted to second for this dictionary to correspond
# exactly with the typical feature values.
event_units = {
        "Men 100": "Time (s)",
        "Men 110h": "Time (s)",
        "Men 400": "Time (s)",
        "Men LJ": "Distance (m)",
        "Men HJ": "Height (m)",
        "Men PV": "Height (m)",
        "Men SP": "Distance (m)",
        "Men DT": "Distance (m)",
        "Men JT": "Distance (m)",
        "Men 1500": "Time (s)"
        }


event_units_short = {
        "Men 100": "s",
        "Men 110h": "s",
        "Men 400": "s",
        "Men LJ": "m",
        "Men HJ": "m",
        "Men PV": "m",
        "Men SP": "m",
        "Men DT": "m",
        "Men JT": "m",
        "Men 1500": "s"
        }


events_lower_is_better_und_dists = {
    "Men_100": 100,
    "Men_110h": 110,
    "Men_400": 400,
    "Men_1500": 1500
}


# For these events, higher is better.
events_higher_is_better = ["Men LJ", "Men HJ", "Men PV", "Men SP", "Men DT", "Men JT"]


# For these events, lower marks are better.
events_lower_is_better = ["Men 100", "Men 110h", "Men 400", "Men 1500"]


# Events by group.
event_groups = {
        "sprints": ["Men 100", "Men 110h", "Men 400"],
        "jumps": ["Men LJ", "Men HJ", "Men PV"],
        "throws": ["Men SP", "Men DT", "Men JT"],
        "endurance": ["Men 1500"]
        }


# Define the names of the columns containing scores derived in some way from the
# IAAF equations.
scores = [
        "Men 100 Score", "Men 110h Score", "Men 400 Score",
        "Men LJ Score", "Men HJ Score", "Men PV Score",
        "Men SP Score", "Men DT Score", "Men JT Score",
        "Men 1500 Score", "Overall Score"
        ]


# Double-checked these. Results match the calculators provided online.
coefficients = {
        "Men 100": {"A": 25.4347, "B": 18, "C": 1.81},
        "Men LJ": {"A": 0.14354, "B": 220, "C": 1.4},
        "Men SP": {"A": 51.39, "B": 1.5, "C": 1.05},
        "Men HJ": {"A": 0.8465, "B": 75, "C": 1.42},
        "Men 400": {"A": 1.53775, "B": 82, "C": 1.81},
        "Men 110h": {"A": 5.74352, "B": 28.5, "C": 1.92},
        "Men DT": {"A": 12.91, "B": 4, "C": 1.1},
        "Men PV": {"A": 0.2797, "B": 100, "C": 1.35},
        "Men JT": {"A": 10.14, "B": 7, "C": 1.08},
        "Men 1500": {"A": 0.03768, "B": 480, "C": 1.85}
        }


# Plotting utility variables -- sprints are blue, jumps are green, throws are red.
# The 400m and the 1500m will be a sort of teal.
event_colors = {"Men 100": (0.0, 0.0, 1.0, 0.7),
                "Men 110h": (0.0, 0.0, 0.7, 0.7),
                "Men 400": (0.0, 0.0, 0.4, 0.7),
                "Men LJ": (0.0, 1.0, 0.0, 0.7),
                "Men HJ": (0.0, 0.7, 0.0, 0.7),
                "Men PV": (0.0, 0.4, 0.0, 0.7),
                "Men SP": (1.0, 0.0, 0.0, 0.7),
                "Men DT": (0.7, 0.0, 0.0, 0.7),
                "Men JT": (0.4, 0.0, 0.0, 0.7),
                "Men 1500": (0.8, 0.0, 0.7, 0.7)}


colormap_event = ListedColormap(colors=list(event_colors.values()), name="decathlon_events")


# ======== Conversions, Date/Time Utilities, etc. ========

# Define a helper to allow us to convert Men's 1500 times into seconds.
def time_seconds(time_obj):
    """
    Converts a time datetime object giving minutes and seconds into a single value for total seconds.
    """
    # Split the time string into minutes and seconds.
    return time_obj.minute * 60 + time_obj.second


def time_seconds_str(time_str):
    """
    Converts a string giving time in minutes and seconds into a value giving total elapsed seconds.
    """
    try:
        minute_str, second_str = time_str.split(":")
    except:
        raise Exception(time_str)
    return (int(minute_str)) * 60 + float(second_str)


def time_seconds_to_speed(time_s, distance_m):
    """
    Converts time in seconds to speed in m/s, given a race of a certain distance.
    """
    return distance_m / time_s


def get_year(row, as_str=True):
    """
    Given a row in a DataFrame that contains a "Date" column, extracts the year from the date in
    the given row.
    :param as_str: if True, the year will be returned as a string. If False, it will be returned as
    an integer.
    """
    if as_str:
        return str(row["Date"].split("/")[-1])
    else:
        return int(row["Date"].split("/")[-1])


def add_year_column(df):
    """
    Given a DataFrame that contains a "Date" column, adds a column that contains the corresponding year
    for each row.
    """
    years = df.apply(axis=1, func=get_year)
    df_out = df.copy()
    df_out["Year"] = years
    df_out["Year"] = df_out["Year"].astype("int")
    return df_out


# ======== Statistical Utilities ========

def compare_groups_t_test(data):
    """
    Given a DataFrame of athlete profiles as generated by calling athlete_performance_profile_relative()
    on each individual athlete in a dataset that also contains a feature indicating elite status
    (or lack thereof), performs a Welch's t-test of statistical independence between groups
    for each event/year in the profile.
    """
    feats = data.drop(columns=["Competitor", "elite"])
    
    results = {feat: stats.ttest_ind(data[data.elite == 0][feat].dropna(),
                                     data[data.elite == 1][feat].dropna(),
                                     equal_var=False)[1]
               for feat in feats}
    return results


# ======== Descriptive Utilities ========

def generate_sample_comparison_df(data, event=None, grouping_feat=None, descriptive_feat=None):
    """
    Using data as produced by athlete_performance_profile_relative() (for an example, see
    performance_profile_analysis()), groups athletes by a feature of interest and creates a
    table that shows a feature of the marks were recorded during each year. In other words,
    if we pass it a dataset, specify "elite" as the grouping variable and "count" as the
    descriptive feature, it might show that, in a given year relative to career start, there
    might be 146 marks/athletes in the "non-elite" category and 58 in the "elite" category, for example.
    :param data: the Pandas DataFrame containing the information concerning the marks for each athlete
    in each year.
    :param grouping_feat: the name of the feature by which to group the data.
    :param descriptive_feat: the name of the feature to obtain for each relative year both all the
    bins defined by the grouping feature.
    :return: a DataFrame giving the descriptive feature of interest for each bin described by the grouping
    feature for each year in the performance profile.
    """
    # First, group data by the grouping feature and then generate a descriptive DataFrame.
    # This will give us a column for "year_0_Men 100" and "year_1_Men 100", etc. Then,
    # transpose it so those columns are part of the index and the new columns are things
    # like "count", "mean", etc.
    df_sample_desc = data.groupby(grouping_feat).describe().transpose()

    # Grab only the indeces of interest; those that describe counts for the event of
    # interest.
    index_of_interest = [
        tup
        for tup in df_sample_desc.index
        if event in tup[0] and tup[1] == descriptive_feat
        ]

    # Extract the counts for the relative years for the event of interest.
    df_counts = df_sample_desc.loc[index_of_interest]

    # Unstack that so the index is in terms of 'year_0_Men 100' instead of
    # ('year_0_Men 100', 'count').
    df_out = df_counts.reset_index(level=1, drop=True)

    return df_out


# ======== Event Sorting/Ranking Utilities ========

# Double-checked results for this by hand; looks rock-solid.
def rank_events(row, v=0, col_label="Score"):
    """
    For a given performance, ranks events from highest to lowest.
    :param row: a row of a DataFrame detailing the performance in question, represented as a
    Series.
    :param v: verbosity level. 0 and 1 are acceptable inputs.
    :param col_label: This tells the algorithm which columns to pick to indicate values for the
    events to sort. For example, if "Score" is passed, columns called "Men 100 Score" and
    "Men SP Score" will be used in the sorting process. If "Z" is passed, columns called
    "Men 100 Z" and "Men SP Z" will be used in the sorting process.
    """
    # scores = {col: row[col] for col in row.index if "Men" in col and "Score" in col}
    scores = {col: row[col] for col in row.index if "Men" in col and col_label in col}

    # Rank-order the scores based on points.
    scores_ranked = sorted(scores.items(), key=lambda tup: tup[1], reverse=True)

    if v == 1:
        pprint.pprint(scores_ranked)

    # Get a list of the names of the events in order of points scored.
    # scores_ordered = [tup[0].replace(" Score", "") for tup in scores_ranked]
    scores_ordered = [tup[0].replace(col_label, "").strip(" ") for tup in scores_ranked]

    if v == 1:
        pprint.pprint(scores_ordered)

    # Rank each event for this athlete.
    event_ranks = {event: scores_ordered.index(event) + 1
                   for event in events}

    # Add the other information from the row and combine with the ranks.
    data_final = {"Competitor": row["Competitor"],
                  "Date": row["Date"],
                  "Overall Score": row["Overall Score"]}

    data_final.update(event_ranks)

    return data_final


def z_score_cols(df, columns: list, invert_cols: list):
    """
    For each listed column, generates an accompanying column that gives a z score for each value
    in the corresponding original column.
    :param df: the Pandas DataFrame for which these columns will be generated.
    :param columns: the columns for which we are going to generate z scores in a standard fashion (no inversion).
    :param abs_cols: for these columns, z scores will be multiplied by -1. This allows us to
    compare apples to apples between, say, the 100m and the shot put. In the 100m, a negative
    Z score is good (indicates less time to complete the race than average). The smaller
    the value becomes, the faster the athlete was. In the shot put, the opposite is true; the
    higher the Z score, the further the athlete threw above average. Conversely, the smaller the
    Z score, the less distance they threw.
    """
    df_copy = df.copy()

    # # First, calculate z scores for the columns that will not be inverted.
    # for col in columns:
        # df_copy[col + " Z"] = df[col].apply(func=lambda val: (val - df[col].mean()) / df[col].std(ddof=0))

    # # Then, calculate z scores for the columns that will be inverted.
    # for col in invert_cols:
        # df_copy[col + " Z"] = df[col].apply(func=lambda val: -(val - df[col].mean()) / df[col].std(ddof=0))

    # Convert to standard scores.
    for col in columns:
        df_copy[col + " Z"] = (df[col] - df[col].mean()) / df[col].std(ddof=0)

    for col in invert_cols:
        df_copy[col + " Z"] = -(df[col] - df[col].mean()) / df[col].std(ddof=0)

    return df_copy


# ======== Scoring Group Utilities ========

def find_means_by_scoring_group(df, bounds: tuple = None):
    """
    Averages numeric columns within a dataframe, including only rows for which the
    "Overall Score" column is between the given bounds.
    """
    # Consider only data within the given bounds for the overall score.
    df_relevant = df[(df["Overall Score"] >= bounds[0]) & (df["Overall Score"] <= bounds[1])]

    # Construct output data.
    data_out = {"Score Upper Bound": bounds[1]}

    # Average every rank in the relevant portion of the dataset.
    avg_ranks = {col: df_relevant[col].mean() for col in df_relevant.columns
                 if "Men" in col}

    # Combine dictionaries.
    data_out.update(avg_ranks)

    return data_out


def find_means_for_all_scoring_groups(df):
    """
    Calculates mean rank for each event by scoring group.
    """
    data_out = []
    for score_max in range(6500, 9500, 500):
        if score_max == 6000:
            data_out.append(find_means_by_scoring_group(df, bounds=(0, score_max))) 
        else:
            data_out.append(find_means_by_scoring_group(df, bounds=(score_max - 499, score_max)))
    return pd.DataFrame(data_out)


# ======== Season's Best ========

def get_season_best_performance(data, athlete, year):
    """
    Gets the season's best performance from the given data for a given athlete during a given year.
    :return: the row(s) of the given DataFrame representing performances that equaled the athlete's
    season's best performance during the given year.
    """
    # Grab all data for that athlete in that year.
    df_athlete = data[(data["Competitor"] == athlete) & (data["Year"] == year)]

    # Find their best performance during the year.
    season_best = df_athlete["Overall Score"].max()

    # Return all performances whose scores equaled the athlete's season's best for the year.
    return df_athlete[df_athlete["Overall Score"] == season_best]


def get_season_best_performance_int(data, athlete, year):
    """
    Returns the athlete's season's best performance for the given year as an integer.
    """
    df_athlete = data[(data["Competitor"] == athlete) & (data["Year"] == year)]

    # Find their best performance during the year.
    return df_athlete["Overall Score"].max()


# ======== Athlete Utilities ========

def map_athletes(data, func):
    """
    This applies a given function to all the collected rows of data pertaining to each athlete individually.
    It returns the output as a dictionary in which each key is the name of an athlete and each value is the
    output of the given function.
    """
    return {athlete: func(data[data["Competitor"] == athlete])
            for athlete in list(data["Competitor"].unique())}


def best_marks(data, athlete, invert_cols, seasons=None, event_name_suf="", out_suffix=""):
    """
    Finds the best marks for an athlete in each discipline. These marks do not need to belong to
    the same performance, but may be gathered from different performances. If seasons are specified,
    marks will only be collected for those seasons; otherwise, marks will be collected for all
    seasons in which the athlete has records in the given DataFrame.
    :param data: the DataFrame containing event performances.
    :param athlete: the name of the athlete for which to collect the performances.
    :param invert_cols: for these columns/disciplines, the lowest mark will be taken
    instead of the highest. This is to account for events in which lower marks are better.
    In some applications, however, standard scores for these events are "flipped" so that
    higher is better for standard scores in all events and they can be compared "apples to
    apples", so flexibility was left open here to flip these events or not.
    :param seasons: a list of integers giving the seasons for which marks will be collected.
    Alternatively, "first" or "last" can be given as values to get only the best marks for
    the athlete's first or last season as given in the DataFrame.
    :param event_name_suf: often, event columns have their names modified
    (e.g., "HJ delta mean"). To allow for some flexibility of naming conventions, this
    parameter's value will be suffixed onto the name of each variable when both indexing the
    DataFrame and constructing the output dictionary.
    :param out_suffix: the event columns will have this suffix added after the regular event
    name suffix in the output dictionary.
    :return: a dictionary in which each key is a string representing the year/season and each
    value is a smaller dictionary with key/value pairs connecting the name of each discipline and
    its performance.

    Vetting: has been tested, looks good. Also works on Z-scored event columns as long as the
    proper suffix is provided for the event names and the invert_cols==(). Tested for Men DT
    and Men 1500 math-wise and in terms of picking the true best marks for each season.
    """
    # Get only data pertaining to the athlete.
    df_ath = data[data["Competitor"] == athlete]

    # If no specific seasons were given, use all of them.
    if not seasons:
        seasons = tuple(df_ath["Year"].unique())
    elif seasons == "first":
        seasons = [df_ath["Year"].min()]
    elif seasons == "last":
        seasons = [df_ath["Year"].max()]

    # Construct a dictionary in which each event's name is paired to either the maximum or
    # minimum value for that event during that year. Then, construct a bigger dictionary that
    # pairs each season with each of the corresponding nested dictionaries that were generated as
    # described above.
    out = {season: {event + event_name_suf + out_suffix: df_ath[df_ath["Year"] == season][event + event_name_suf].max()
                    if event not in invert_cols
                    else df_ath[df_ath["Year"] == season][event + event_name_suf].min()
                    for event in events}
           for season in seasons}

    # Add an entry for the competitor to each season's dictionary.
    [out[year].update({"Competitor": athlete}) for year, event_values in out.items()]

    return out


def get_all_time_pr_with_year(df_ath, event, invert_cols, include_relative_years=False):
    """
    Given a DataFrame containing only data for one athlete, find the athlete's all-time best
    mark in that event and the year during which that event occurred.
    """
    # Get only the rows in which the athlete performed at their all-time best in that event.
    # That could be their lowest or highest mark depending on the event.
    if event not in invert_cols:
        df_best = df_ath[df_ath[event] == df_ath[event].max()]
    else:
        df_best = df_ath[df_ath[event] == df_ath[event].min()]
    
    # Keep track of the PR.
    pr = df_best[event].values[0]

    # Return the mark and the (potentially relative) year in which it was achieved.
    # if not include_relative_years:
        # year = df_best["Year"].values[0]
    # else:
        # # If we're supposed to give the year relative to the first year of competition, do so.
        # year = df_best["Year"].values[0] - df_ath["Year"].min()

    # Return the requested information.
    if include_relative_years:
        return {event + " all time best": pr, event + " year of all time best": year}
    else:
        return {event + " all time best": pr}


def get_all_time_bests(data, athlete, invert_cols, event_name_suf, include_relative_years=False):
    """
    Gets all-time bests in each event for a given athlete.
    :param data: the DataFrame containing event performances.
    :param athlete: the name of the athlete for which to collect the performances.
    :param invert_cols: for these columns/disciplines, the lowest mark will be taken
    instead of the highest. This is to account for events in which lower marks are better.
    In some applications, however, standard scores for these events are "flipped" so that
    higher is better for standard scores in all events and they can be compared "apples to
    apples", so flexibility was left open here to flip these events or not.
    :param event_name_suf: often, event columns have their names modified
    (e.g., "HJ delta mean"). To allow for some flexibility of naming conventions, this
    parameter's value will be suffixed onto the name of each variable when both indexing the
    DataFrame and constructing the output dictionary.
    :param include_relative_years: if True, not only will the best mark for each event be included,
    but also when the best mark for each event occurred relative to the first year of competition
    for that athlete. So, if the athlete first competed in 2013 and they hit their all-time PR for
    the long jump in 2016, the relative year value for that event is 3.
    :returns: a dictionary with one or two keys for each event; one key is simply the name of the
    event plus the given suffix, which gives the best mark for that event. The other key, which
    is the same key as the previous one plus "_rel_year" gives the number of years relative to
    the first year of competition that the all-time PR for that event occurred.
    """
    # Get the data that pertains to the given athlete.
    df_ath = data[data["Competitor"] == athlete]

    results = {"Competitor": athlete}

    # Grab PRs by event.
    # prs_by_event = {event + event_name_suf + " all-time best": get_all_time_pr_with_year(
        # df_ath,
        # event=event + event_name_suf,
        # invert_cols=invert_cols,
        # include_relative_years=include_relative_years)
        # for event in events}

    # Combine results for all events.
    [results.update(get_all_time_pr_with_year(
        df_ath,
        event = event + event_name_suf,
        invert_cols=invert_cols,
        include_relative_years=include_relative_years))
     for event in events
     ]

    return results


def performance_profile(df, athlete: str, events: list, event_name_suffix, vis=False) -> pd.DataFrame:
    """
    Constructs a performance profile of an athlete in the given events.
    :param df: the DataFrame to mine for athlete performances.
    :param athlete: the name of the athlete.
    :param events: a list of events for which to construct a performance profile.
    :param event_name_suffix: this suffix will be added to the given event names when searching in the
    DataFrame. This allows for columns with alternate names that represent events to be used.
    :param vis: if True, a plot will be provided.
    """
    # Collect data of interest.
    event_names = [event + event_name_suffix for event in events]
    df_ath = df[df["Competitor"] == athlete][event_names + ["Date"]]
    df_ath["Date"] = pd.to_datetime(df_ath["Date"])
    df_sorted = df_ath.sort_values(by="Date")

    if vis:
        for event in event_names:
            df_sorted.plot(x="Date", y=event)
            plt.ylabel("Mark")
            plt.title(f"Performance profile for {athlete} in the {event}")
            plt.show()

    return df_sorted


# I verified that this is working in the same way it was when it was not a formal
# function yet in the decathlete_profile_analysis.py script by looking at graphs
# for the 100m and the JT with the same elite threshold (>7500 overall score).
# The graphs looked identical.
def athlete_performance_profile_relative(df, athlete, max_relative_year, dec_events=events, events_higher_is_better=None):
    """
    Builds a profile for the athlete in which, for every year y in range [0, max_relative_year]
    relative to the first year in which they competed, a new column is created for each
    event. For example, year_0_LJ, gives their long jump PR during their first year of
    competition. year_3_SP would give their shot put PR during their fourth year of
    competition.
    :param df: the DataFrame to search for records of the athlete's performance.
    :param athlete: the name of the athlete.
    :param max_relative_year: the highest year relative career start for which we will collect
    data.
    :param dec_events: the decathlon events for which best marks will be collected. This can
    technically include any numeric column in the DataFrame.
    """
    # Get all the data pertaining to the given athlete.
    df_ath = df[df.Competitor == athlete]

    # Sort the athlete's performances by date.
    df_ath = df_ath.sort_values(by="Date")

    # Collect all the years in which the athlete was active and the first year in which he
    # was active.
    years = list(df_ath.Year.unique())
    first_year = min(years)

    def get_best_mark(year, event):
        if event in events_higher_is_better:
            return df_ath[df_ath.Year == year][event].max()
        else:
            return df_ath[df_ath.Year == year][event].min()

    # This is the element we want as the first column in the DataFrame, so we're putting it here.
    data_out = {"Competitor": athlete} 

    # Get the athlete's best mark in each year relative to the start of his career in each
    # event.
    data_marks = {f"year_{rel_year}_{event}": get_best_mark(first_year + rel_year, event)
                  for rel_year in range(max_relative_year + 1)
                  for event in dec_events}

    data_out.update(data_marks)

    return data_out


def year_event_combinations(years_rel, events,
                            include_competitor_col=False, include_elite_col=False):
    """
    This generates a list of strings that correspond to feature names
    representing performances for all combinations of the given years
    relative to career start and events. For example, if we pass [0, 1, 2]
    and ['Men 400'], we'll get ['year_0_Men 400', 'year_1_Men 400', 'year_2_Men 400'].
    The two flags include_competitor_col and include_elite_col allow for the items
    'Competitor' and 'elite' to be added to the beginning and end of the list,
    respectively.
    """
    cols = [
            f"year_{year}_{event}"
            for year in years_rel
            for event in events
            ]

    if include_competitor_col:
        cols.insert(0, 'Competitor')

    if include_elite_col:
        cols.append('elite')

    return cols


def get_first_season_overall_PR(df, athlete):
    """
    Returns the highest overall score achieved by an athlete during the first
    season recorded in the given DataFrame.
    """
    df_ath = df[df.Competitor == athlete]
    comp_years = years_active(df, athlete)
    return df_ath[df_ath.Year == min(comp_years)]["Overall Score"].max()


def get_all_time_overall_PR(df, athlete):
    """
    Returns the highest score an athlete in achieved in the given DataFrame.
    """
    return df[df.Competitor == athlete]["Overall Score"].max()


def years_active(df, athlete):
    """
    Returns a list giving the years during which an athlete was active.
    """
    return df[df.Competitor == athlete]["Year"].unique()


def n_years_active(df, athlete):
    """
    Returns the number of years between an athlete's first and last years of activity.
    """
    yrs = years_active(df, athlete)
    return max(yrs) - min(yrs) + 1


def get_first_season_overall_PR(df, athlete):
    """
    Returns the highest overall score achieved by an athlete during the first
    season recorded in the given DataFrame.
    """
    df_ath = df[df.Competitor == athlete]
    comp_years = years_active(df, athlete)
    return df_ath[df_ath["Year"] == min(comp_years)]["Overall Score"].max()


# I looked over the results of Damian Warner, Kevin Mayer, and Zachary Haskin to
# make sure they weren't included. The first two had first year PRs over 7500 and
# Zach had a first year PR of 6402 but only one year of competition. Finally, I also
# checked to make sure that Sinval Souza DE OLIVEIRA, who had competed for four years
# with first year PR of 7243, was not in the final dataset (he wasn't). I also checked
# Tarmo RIITMURU (5 years of competition, first year PR of 7626) was not in the dataset
# and he wasn't. Finally, I checked to make sure that Esteban SALGADO (6607 first year PR,
# 5 years of competition) was in the dataset, and he was. Then, I checked to make sure that
# his first year PR was calculated correctly. His first year of competition, 2011, yielded
# a singel performance of 6607, which matched the listed first year PR).
#
# Hence, I believe the function is working correctly! Keep in mind, PR bounds were set at
# (6000, 7000) for filtering.
def filter_by_first_season(data, min_years_active=4, first_year_pr_bounds=(7500, 8500), v=1):
    """
    Finds athletes in the given data whose season's best performance during their first active season was
    within a given range of scores. Then, all performances for each of those athletes is included in the final
    DataFrame while all others are dropped.
    :param min_years_active: athletes that competed in this many distinct seasons or more will be kept in the
    returned dataset.
    :param v: verbosity level. 0 (quiet) or 1 (includes all output) are available as options.
    :return: a DataFrame containing only performances by the athlete meeting the specified parameters.
    """
    # Get a list of all the athletes represented in the data.
    athletes = list(data["Competitor"].unique())

    # Grab all rows representing each athlete and then get the unique values from the year column.
    years_active = {athlete: sorted(list(data[data["Competitor"] == athlete]["Year"].unique()))
                    for athlete in athletes}

    # Then, we count those to find the number of year during which the athlete competed.
    years_active_count = {athlete: len(years) for athlete, years in years_active.items()}

    # Find the best overall score for each athlete during their first recorded season of competition.
    first_sb_perfs = {athlete: get_season_best_performance_int(data, athlete, year=years_active[athlete][0])
                      for athlete in athletes}

    if v:
        print("Years in which athlete competed:")
        pprint(years_active)

        print("Number of distinct years in which each athlete competed:")
        pprint(years_active_count)

        print("First season PRs:")
        pprint(first_sb_perfs)

    # We only want to include athletes that were active for a certain number of years or more.
    # Additionally, we only want to include athletes that score within a certain range during their
    # first season of competition.
    # Hence, we're going to drop athletes with a number of active years below the specified value
    # or a first season PR that is out of the specified bounds.
    [athletes.remove(athlete)
     for athlete, year_count in years_active_count.items()
     if year_count < min_years_active or not first_year_pr_bounds[0] <= first_sb_perfs[athlete] <= first_year_pr_bounds[1]]

    if v:
        print(f"Final Athletes Included n={len(athletes)}:")
        pprint(athletes)

    # Mark each row based on whether the athlete in it made the final cut or not.
    df_out = data.copy()
    df_out["valid_athlete"] = df_out.apply(func=lambda row: int(row["Competitor"] in athletes), axis=1)

    # Keep only those rows for which we determined the athlete in question should be included in the dataset.
    df_out = df_out[df_out["valid_athlete"] == 1]

    # Get rid of the valid_athlete column; we can clean up our DataFrame a bit now that we don't need it anymore.
    df_out.drop(columns=["valid_athlete"], inplace=True)

    return df_out


# ==== Decathlon Scoring/Equation Utilities ====

def score_perf(perf, event: str, return_nan=False, typecast_func=int) -> int:
    """
    Scores a performance in a given event. Note that performances in the 100m, 400m, 110m hurdles, and 1500m should
    be given in seconds, the long jump, high jump, and pole vault should have values given in m, and the
    shot put, discus, and javelin throw should have performances given in m.
    :param perf: the value of the performance.
    :param event: a string giving the name of the event. Should correspond the value of a key in "events" variable above.
    :param return_nan: if True, the function will return NaN if that is the result of the scoring
    calculation. If False, an integer conversion will be attempted.
    :param typecast_func: this function will be used to cast the scores to the desired output type.
    :return: an integer indicating the point score for that performance.
    """
    # Gather coefficients.
    a, b, c = list(coefficients[event].values())

    # Convert values in the jumps from meters to centimeters.
    if event == "Men PV" or event == "Men LJ" or event == "Men HJ":
        perf_final = perf * 100
    else:
        perf_final = perf

    if event in events_lower_is_better:
        # If we would have to take a power of a negative number, the
        # score is Nan.
        if b < perf_final:
            out = np.NaN
        else:
            out = a * (b - perf_final)**c
        # return int(a * (b - perf_final)**c)
    else:
        # if we we would have to take a power of a negative number,
        # the score is NaN.
        if perf_final < b:
            out = np.NaN
        else:
            out = a * (perf_final - b)**c
        # return int(a * (perf_final - b)**c)

    if type(out) == pd.Series:
        out: pd.Series
        return out.astype(typecast_func)
    else:
        if np.isnan(out) or pd.isna(out):
            if return_nan:
                return out
            else:
                raise Exception(f"Value {out} is NaN but return_nan == False.")
        else:
            return typecast_func(out)


def plot_equations_z(df, event_name_suffix="", output_graph_suffix="", save_fig=True):
    """
    Plots the values of decathlon scoring equations by first z scoring the performances and then showing how going several
    standard deviations upon and down produces different scores.
    :param df: a DataFrame containing performances of interest. These performances will be used to compute mean and standard
    deviation for displaying points awarded by z score for each event.
    :param event_name_suffix: this suffix will be added onto the name of each event when searching for a given event in the
    DataFrame.
    :param output_graph_suffix: this string will be inserted at the end of the name of the output graph. This way,
    if the suffix is " first year PR", then the full output relative directory/name will be
    "Images/Z Score Point Reward first year PR.png".
    :param save_fig: if True, the generated figure will be saved in "Images/Z Score Point Rewards.png". Another plot,
    "Images/Z Score Point Ranges.png" is produced, which visualizes the ranges as bars rather than as lines.
    :return: a DataFrame containing the scoring data for each event. The index consists of the z scores, the columns are
    the event names, and the individual values are the points awarded for the corresponding event and z score.
    """
    # Calculate mean and standard deviation of performances in each event.
    means = {event: df[event + event_name_suffix].mean() for event in events}
    sds = {event: df[event + event_name_suffix].std(ddof=0) for event in events}

    def z_to_perf(z, event):
        """
        Helper to convert a z score to a performance.
        """
        if event in events_lower_is_better:
            return (z / -1) * sds[event] + means[event]
        else:
            return z * sds[event] + means[event]

    # Create an x dimension of standard scores going from -2 to 2.
    z_scores = np.linspace(-2, 2, 200)

    # Create our dataset by calculating a performance for each Z score.
    scores = {
            event: {z: score_perf(perf=z_to_perf(z, event), event=event) for z in z_scores}
            for event in events
            }

    # Create a DataFrame.
    df_scores = pd.DataFrame(scores)

    # First, show the progression of each event across the standard score range.
    df_scores.plot(kind="line", colormap=colormap_event, linewidth=2.0)
    plt.title("Point Reward for Each Event by Standard Score")
    plt.ylabel("Points")
    plt.xlabel("Z Score")
    plt.savefig(f"Images/Z Score Point Rewards{output_graph_suffix}.png") if save_fig else None
    plt.show()

    # Then, show the difference between the upper and lower bounds.
    df_score_range =  pd.DataFrame([{"event": col, "range": df_scores[col].max() - df_scores[col].min()}
                                    for col in df_scores.columns])
    score_mins = {col: df_scores[col].min() for col in df_scores.columns}
    score_ranges = {col: df_scores[col].max() - df_scores[col].min() for col in df_scores.columns}

    plt.figure(figsize=(6, 6))
    plt_bar_x = [x for x in range(1, 11)]
    [plt.bar(plt_bar_x,
             width=0.5,
             height=list(score_ranges.values()),
             bottom=list(score_mins.values()),
             color=list(event_colors.values()))]
    # df_score_range.plot(kind="bar", x="event", y="range", color=list(event_colors.values()), figsize=(6, 6))
    plt.xticks(plt_bar_x, labels=df_scores.columns, rotation=45)
    plt.ylim((300, 1000))
    plt.ylabel("Score")
    plt.title("Scoring Profile from -2σ to 2σ Performances for each Discipline")
    plt.savefig(f"Images/Z Score Point Ranges{output_graph_suffix}.png") if save_fig else None
    plt.show()

    # Show events in order of range.
    pprint(df_score_range.sort_values(by="range", ascending=False))

    return df_scores


def plot_performance_profiles(data, event, max_relative_year, p_values, alpha=0.05,
                              output_path=None):
    """
    Given a DataFrame of athlete profiles as generated by calling athlete_performance_profile_relative()
    on each individual athlete in a dataset that also contains a feature indicating elite status
    (or lack thereof), creates plots for a given event displaying the trajectory of the
    elite and non-elite athletes across their careers.
    :param data: the DataFrame containing the information specified above.
    :param event: a string detailing the event for which comparisons should be performed.
    :param max_relative_year: this value indicates the highest relative year (indicating
    a number of years after an athlete's first year of competition, 0 being their first
    year) for which the differences between groups will be plotted.
    :param p_values: a dictionary in which each key specifies a year relative to career
    start and an event (e.g., "year_1_Men JT") and each value is a p-value that was the result
    of a test to see if the two groups (elite and non-elite, or some other classification criteria
    that may have been used) were meaningfully different in that event and in that year.
    :param alpha: this is the value at or below which results are considered statistically
    significant.
    :param output_path: if provided, the graph that is produced will be saved at the given location.
    If a path that ends in ".png" is provided, the path will be used as-is. If not, it will be
    treated as a path to a folder in which a new image will be created with a name equal to the
    name in question with ".png" at the end.
    """
    # Calculate group means.
    df_means = data.drop(columns=["Competitor"]).groupby(by="elite").mean()  # Originally used df_prof.

    pprint(df_means)

    # Compute, in an ordered fashion, the names of the columns in the DataFrame
    # that represent the athletes' trajectory in the event of interest across
    # time.
    years_rel = list(range(max_relative_year + 1))
    event_cols = [f"year_{i}_{event}" for i in years_rel]

    pprint(df_means[event_cols])

    # Get values for both elite and non-elite athletes.
    values_non_elite = df_means.iloc[0][event_cols].values
    values_elite = df_means.iloc[1][event_cols].values

    print("Non-Elite Means:")
    pprint(values_non_elite)
    print("\nElite Means:")
    pprint(values_elite)

    # Plot.
    # df_means[event_cols].plot(kind="bar")
    plt.figure(figsize=(6, 4))
    plt.plot(years_rel, values_non_elite, c="blue", marker="s", label="Non-Elite")
    plt.plot(years_rel, values_elite, c="red", marker="^", label="Elite")

    # Show significance in the plot.
    # Add each year to the list for which results between the elite group and the non-elite
    # group were significantly different.
    years_significant = [
            year
            for year in range(max_relative_year)
            if p_values[f"year_{year}_{event}"] <= alpha
            ]

    # Grab only performance values at which there was a statistically significant difference.
    # Then, increase all those values by a small amount so the values end up above the actual
    # performance marks.
    plt.scatter(
            # X axis values: years for which there were significant differences.
            years_significant,
            # Y axis: performance values for those years, increased by a small amount to move
            # the markers upward.
            values_elite[years_significant] + values_elite.std() / 4,
            c="red", marker="*", label="Significantly different from non-elite")

    # Title and show the plot.
    plt.xlabel("Year Relative to Career Start")
    plt.ylabel(event_units[event])
    plt.title(f"Comparing Elite and Non-Elite Athletes in the event \"{event}\"")
    plt.legend()

    if output_path:
        if output_path[:-3] == ".png":
            plt.savefig(output_path)
        else:
            plt.savefig(output_path + f"{event}.png")

    plt.show()


# ======== Full Analyses ========

def performance_profile_analysis(df, max_relative_year=10, category_name="elite", categorizer=None,
                                 categorizer_params: list = None, vis=True):
    """
    Constructs a performance profile for each given athlete in the DataFrame consisting
    of their PR in each event during each year they competed relative to their first,
    up to a given maximum relative year. Then, compartmentalizes the athletes into two
    groups based on a categorizing function (elite/non-elite, medal/no medal, etc.).
    Finally, comparisons between the season's bests of the two groups are made.
    :param df: the Pandas DataFrame containing athlete performance data.
    :param max_relative_year: the maximum number of years relative to the first year of
    competition that will be included in the comparison between groups.
    :param category_name: the name of the variable that states the category of each athlete.
    :param categorizer: the function used to categorize the athlete. This function will be
    given the DataFrame (df) and the name of the athlete in question and will need to draw
    conclusions based upon that (and the categorizer_params).
    :param categorizer_params: extra parameters that can be passed to the categorizer if the
    DataFrame mentioned above and the name of the athlete are not enough.
    :param vis: if True, visualizations will be provided for each event.
    :return: the DataFrame of athlete profiles that was generated, the results of the t-tests
    testing differences between elite and sub-elite samples, and the results of the Shapiro-Wilk
    tests of normality.
    """
    # Construct athlete profiles.
    df_prof = pd.DataFrame([athlete_performance_profile_relative(df, ath, max_relative_year=max_relative_year)
                            for ath in list(df.Competitor.unique())])

    # Tag athletes based upon the decision function.
    df_prof[category_name] = [categorizer(df, ath, categorizer_params)
                              for ath in df_prof.Competitor.values]

    # Perform t-tests to determine if there are statistically significant differences between groups
    # for any event during any year.
    t_test_results = compare_groups_t_test(df_prof)

    # Perform Shapiro-wilk tests to test the null hypothesis that the performances are
    # realizations of normally distributed variables.
    normality_results = {
            feat: stats.shapiro(df_prof[feat].dropna())
            for feat in df_prof.drop(columns=["Competitor", "elite"])
            }

    # Plot the events.
    for event in events:
        plot_performance_profiles(data=df_prof,
                                  event=event,
                                  max_relative_year=max_relative_year,
                                  p_values=t_test_results,
                                  alpha=0.05)

    # Return the DataFrame of profiles and the t-test results.
    return df_prof, t_test_results, normality_results

