#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Data cleaning function to be imported and used for all modeling. Allows for reproducibility
@author: saraokun, kpant, prestonlharry
"""

# ===============
# LIBRARIES
# ============
import json
import numpy as np
import pandas as pd

from sklearn.preprocessing import LabelEncoder, OneHotEncoder
from sklearn.model_selection import train_test_split


# ===============
# HELPER FUNCTIONS
# ============

def set_nulls(data):
    """
    Replace -99s (used in the raw data to designate null) of all forms with NaN values
    @param data: DataFrame of values
    @return: DF with NaN instead of -99
    """
    # Run three separate replaces, each for a different data type.
    data.replace(to_replace=-99, value=np.nan, inplace=True)
    data.replace(to_replace=-99.0, value=np.nan, inplace=True)
    data.replace(to_replace='-99', value=np.nan, inplace=True)
    return data


def map_cpt(data, column, replace, name):
    """
    Manually map principle CPT codes to predefined categories.
    @param data: DataFrame of values
    @param column: Column to apply changes to
    @param replace: Values to replace
    @param name: New value to replace with
    @return: DataFrame with replaced values
    """
    # Iterate through values to replace, replacing with name provided.
    for r in replace:
        idx = np.where(data[column] == r)[0]
        data[column].loc[idx] = name
    return data


def ensure_before_readmission(df, day_col, binary_col, cols_to_drop=[]):
    """
    Function used to ensure any event occurs before readmission, preventing leakage.
    @param df: DataFrame of values
    @param day_col: Column containing days to event occurrence
    @param binary_col: Column containing binary value (whether event occurred)
    @param cols_to_drop: Additional related columns not needed for analysis (optional)
    @return: DataFrame with all events occurring before readmission
    """
    # Create a variable to check if readmission is before the particular column by setting null values extremely high
    days_to_readmission = df['READMPODAYS1'].fillna(999)
    # If an event occurs after readmission, set binary value to 0
    df.loc[(days_to_readmission - df[day_col]) <= 0, binary_col] = 0
    # Create list of columns to drop, combining provided list with now unneeded date column
    dropcols = cols_to_drop + [day_col]
    df.drop(columns=dropcols, inplace=True)
    return df


def replace_col_values(df, column, replace_dict, nan_vals=[], new_name=None):
    """
    Replace specified values in column with new values provided, set NaN and rename column where necessary
    @param df: DataFrame containing data
    @param column: Column to modify
    @param replace_dict: Dictionary old, new pairs for replacement
    @param nan_vals: Values to replace with NaN (optional)
    @param new_name: New column name for interpretability (optional)
    @return: Modified DataFrame
    """
    # Replace all values in column using dictionary provided
    df[column].replace(replace_dict, inplace=True)
    # Replace specific values with NaN
    for val in nan_vals:
        df[column].replace(val, np.nan, inplace=True)
    # Rename column if appropriate
    if new_name is not None:
        df.rename(columns={column: new_name}, inplace=True)
    return df


def le_ohe(data, features):
    """
    Label encoding and one-hot encoding for categorical variables.
    @param data: DataFrame of data to modify
    @param features: Features to encode
    @return: Encoded DataFrame
    """
    # Use SKLearn's LabelEncoder to create encodings for data. Store in dictionary
    le = LabelEncoder()
    encodings = {}
    for c in features:
        data[c] = le.fit_transform(data[c].astype(str))
        encodings[c] = dict(zip(le.classes_, le.transform(le.classes_)))

    # Write encodings to txt file for record-keeping + result interpretability
    with open('encodings.txt', 'w') as mappings:
        for key, value in encodings.items():
            mappings.write('%s:%s\n' % (key, value))

    # Use SKLearn's OneHotEncoder to encode categorical variables. Store in new DataFrame
    ohe = OneHotEncoder(drop='first')
    ohe.fit(data[features])
    ohe_labels = ohe.transform(data[features]).toarray()
    df_cat = pd.DataFrame(ohe_labels, columns=ohe.get_feature_names(features))

    # Drop old features, concatenate data with new encoded features
    data.drop(features, axis=1, inplace=True)
    clean_data = pd.concat([data, df_cat], axis=1)
    return clean_data


# ===============
# MAIN FUNCTION
# ============

def load_and_clean_data():
    """
    Function which loads in data and applies a series of cleaning operations.
    @return: Cleaned dataframe
    """
    # Read in data and cleaning JSON
    f = open('../data/data_cleaning.json', )
    clean_dict = json.load(f)
    df = pd.read_csv("../data/monet_output.csv")
    df.drop(['Unnamed: 0', 'X'], axis=1, inplace=True)

    # Deal with null values: replace -99s with NaN and fill NaN for certain columns where imputation is possible
    df['COL_APPROACH'].fillna("other", inplace=True)
    df['REOPERATION2'].fillna(0, inplace=True)

    # Categorize approach using predefined categories
    options = [clean_dict["approach"]['MIS'], clean_dict["approach"]['open'], clean_dict["approach"]['other']]
    names = ['MIS', 'open', 'other']
    for i in range(len(options)):
        df = map_cpt(df, 'COL_APPROACH', options[i], names[i])
    nulls = np.where(df.COL_APPROACH == 'Unknown')[0]
    df.COL_APPROACH.loc[nulls] = np.nan  # Set unknown values to NaN

    # Create new BMI column using height and weight
    df["bmi"] = (df.WEIGHT / 2.205) / ((df.HEIGHT / 39.37) ** 2)

    # Process target variable
    unplanned = [c for c in df if "UNPLANNEDREADMISSION" in c]
    df['target'] = [1 if x > 0 else 0 for x in df[unplanned].sum(axis=1)]

    # Process other procedures and concurrent procedures (subject to change)
    othercpt = [c for c in df if "OTHERCPT" in c]
    df['num_other_procs'] = df[othercpt].count(axis=1)
    concurrcpt = [c for c in df if "CONCURR" in c]
    df['num_concurr_procs'] = df[concurrcpt].count(axis=1)

    df['insulin'] = df.DIABETES  # Duplicate diabetes column for one-hot encoding
    idx = np.where(df.BLEEDDIS.isnull() is False)[0]
    replace = df.BLEEDDIS.iloc[idx]
    df.BLEEDIS.iloc[idx] = replace  # Deal with split column BLEEDIS AND BLEEDDIS

    # Make sure that any relevant event occurs before readmission
    ensure_dict = clean_dict['ensure_before_readmission']
    for binary_col in ensure_dict.keys():
        df = ensure_before_readmission(df,
                                       ensure_dict[binary_col]['day_col'],
                                       binary_col,
                                       ensure_dict[binary_col]['cols_to_drop'])

    # Drop unneeded columns (DO THIS AFTER CREATING NEW COLUMNS AS DROP LIST MAY INCLUDE COLUMNS NEEDED)
    cols_to_drop = clean_dict['cols_to_drop']
    df.drop(columns=cols_to_drop, inplace=True)

    # Replace and rename columns for simplicity/interpretability/ML processing
    replace_dict = clean_dict['replace_col_vals']
    for col in replace_dict.keys():
        df = replace_col_values(df, col, **replace_dict[col])

    # Drop NaN values for age and sex and reset index before OHE before one-hot encoding
    df.dropna(subset=['AGE', 'female'], inplace=True)
    df.reset_index(inplace=True, drop=True)

    # Apply one hot encoding to categorical variables
    cat_feat = [col for col in df.columns if (df[col].dtype == 'O') or (df[col].isnull().sum() != 0)]
    df = le_ohe(df, cat_feat)

    # Set -99 to NaN (MAKE SURE THIS IS AFTER OHE)
    df = set_nulls(df)

    # Remove rows with null values in specific columns
    dropna_cols = clean_dict['dropna_cols']
    df.dropna(axis=0, subset=dropna_cols, inplace=True)

    X = df.drop('target', axis=1)
    y = df['target']

    # 70 20 10 split
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.1, random_state=42)

    return X_train, X_test, y_train, y_test


# ===============
# MAIN EXECUTION
# ============

if __name__ == "__main__":
    # If executing from this script directly, run and print shape of cleaned DataFrame for debugging
    X_train, X_test, y_train, y_test = load_and_clean_data()
    print(X_train.shape)
    print(X_test.shape)
    print(y_train.shape)
    print(y_test.shape)
