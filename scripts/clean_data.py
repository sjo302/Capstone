import numpy as np
import pandas as pd
import json
from sklearn.preprocessing import LabelEncoder, OneHotEncoder


def set_nulls(data):
    """
   @param data: dataframe

   @return dataframe with -99 replaced with NaN
    """
    data.replace(to_replace=-99, value=np.nan, inplace=True)
    data.replace(to_replace=-99.0, value=np.nan, inplace=True)
    data.replace(to_replace='-99', value=np.nan, inplace=True)
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
        df.rename(columns={column: new_name}, inplace=True)
    return df


def le_ohe(data, features):
    le = LabelEncoder()
    encodings = {}
    for c in features:
        data[c] = le.fit_transform(data[c].astype(str))
        encodings[c] = dict(zip(le.classes_, le.transform(le.classes_)))

    with open('encodings.txt', 'w') as mappings:
        for key, value in encodings.items():
            mappings.write('%s:%s\n' % (key, value))

    ohe = OneHotEncoder(drop='first')
    ohe.fit(data[features])
    ohe_labels = ohe.transform(data[features]).toarray()
    df_cat = pd.DataFrame(ohe_labels, columns=ohe.get_feature_names(features))

    data.drop(features, axis=1, inplace=True)
    clean_data = pd.concat([data, df_cat], axis=1)
    return clean_data


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

    # Make sure that any relevant event occurs before readmission.
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

    # Remove rows with null values in specific columns
    nulls = [col for col in df.columns if (df[col].isnull().sum() != 0)]
    nulls_le30k = [c for c in nulls if df[c].isnull().sum() <= 30000]  # Find all cols w <30k blank rows.
    df.dropna(subset=['AGE', 'OPTIME', 'female'] + nulls_le30k, inplace=True)

    # Reset index before OHE
    df.reset_index(inplace=True, drop=True)

    # Apply one hot encoding to categorical variables
    cat_feat = [col for col in df.columns if (df[col].dtype == 'O') or (df[col].isnull().sum() != 0)]
    df = le_ohe(df, cat_feat)

    # Set -99 to NaN DO AFTER OHE
    df = set_nulls(df)

    return df


if __name__ == "__main__":
    data = load_and_clean_data()
    print(data.shape)
