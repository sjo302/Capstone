import numpy as np
import pandas as pd
import json


def set_nulls(data):
    """
   @param data: dataframe

   @return dataframe with -99 replaced with NaN
    """
    data.replace(to_replace=-99, value=np.nan)

    return data


def map_cpt(data, column, replace, name):
    """
    @param data: dataframe
    @param column: string, column name
    @param replace: list of variables holding the values to be replaced by that particular variable name
    @param name: string or integer of what will replace the values in replacements

    """
    for r in replace:
        idx = np.where(data[column] == r)[0]
        data[column].loc[idx] = name
    
    return data


def ensure_before_readmission(df, day_col, binary_col, cols_to_drop=[]):
    # Create a variable to check if readmission is before the particular column by setting null values extremely high
    days_to_readmission = df['READMPODAYS1'].fillna(999)
    df.loc[(days_to_readmission - df[day_col]) <= 0, binary_col] = 0
    dropcols = cols_to_drop + [day_col]
    df.drop(columns=dropcols, inplace=True)
    return df


def replace_col_values(df, column, replace_dict, nan_vals=[], new_name=None):
    df[column].replace(replace_dict, inplace=True)
    for val in nan_vals:
        df[column].replace(val, np.nan, inplace=True)
    if new_name is not None:
        df.rename(columns={column: new_name})
    return df


def load_and_clean_data():
    """
    Function which loads in data and applies a series of cleaning operations.
    @return: Cleaned dataframe
    """
    # Read in data and cleaning JSON
    f = open('../data/data_cleaning.json', )
    clean_dict = json.load(f)
    df = pd.read_csv("../data/monet_output.csv")
    data.drop(['Unnamed: 0', 'X'], axis=1, inplace=True)

    # Deal with null values: replace -99s with NaN and fill NaN for certain columns where imputation is possible.
    df['COL_APPROACH'].fillna("other", inplace=True)
    df['REOPERATION2'].fillna(0, inplace=True)

    # Categorize approach using predefined categories.
    options = [clean_dict["approach"]['MIS'], clean_dict["approach"]['open'], clean_dict["approach"]['other']]
    names = ['MIS', 'open', 'other']
    for i in range(len(options)):
        df = map_cpt(df, 'COL_APPROACH', options[i], names[i])
    nulls = np.where(df.COL_APPROACH == 'Unknown')[0]
    df.COL_APPROACH.loc[nulls] = np.nan  # Set unknown values to NaN

    # Create new BMI column using height and weight.
    df["bmi"] = (df.WEIGHT / 2.205) / ((df.HEIGHT / 39.37) ** 2)

    # Process target variable.
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

    # Drop unneeded columns (DO THIS AFTER CREATING NEW COLUMNS AS DROP LIST MAY INCLUDE COLUMNS NEEDED)
    cols_to_drop = clean_dict['cols_to_drop']
    df.drop(columns=cols_to_drop, inplace=True)

    # Make sure that any relevant event occurs before readmission.
    ensure_dict = clean_dict['ensure_before_readmission']
    for binary_col in ensure_dict.keys():
        df = ensure_before_readmission(df, ensure_dict[binary_col]['day_col'], binary_col,
                                             ensure_dict[binary_col]['cols_to_drop'])

    # Replace and rename columns for simplicity/interpretability/ML processing
    replace_dict = clean_dict['replace_col_vals']
    for col in replace_dict.keys():
        df = replace_col_values(df, col, **replace_dict[col])

    # Remove rows with null values in specific columns
    df.dropna(subset=['AGE', 'female'], inplace=True)

    # Reset index before OHE
    df.reset_index(inplace=True, drop=True)

    df = set_nulls(df)

    return df


if __name__ == "__main__":
    data = load_and_clean_data()
    print(data)
