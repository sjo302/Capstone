import pandas as pd
import numpy as np
import json
from argparse import ArgumentParser

from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, auc
from sklearn.feature_selection import mutual_info_classif

from clean_data import load_and_clean_data


def run_baseline_dt(X_train, y_train, X_val, y_val):
    dt = DecisionTreeClassifier()
    dt.fit(X_train, y_train)
    y_pred = dt.predict_proba(X_val)[:, 1]

    fpr, tpr, thresholds = roc_curve(y_val, y_pred)
    roc_auc = auc(fpr, tpr)

    importances = {}
    for name, importance in zip(X_val.columns, dt.feature_importances_):
        importances[name] = importance

    imp = pd.DataFrame.from_dict(importances, orient='index', columns=['importance'])
    imp.sort_values(by='importance', ascending=False, inplace=True)

    return fpr, tpr, roc_auc, imp


def mutual_information_ranking(X, y):
    """
    Use mutual information to rank features by importance
    """

    mi_scores = mutual_info_classif(X, y)

    mi_features = pd.Series(dict(zip(X.columns, mi_scores)))

    return mi_features.sort_values(ascending=False)


def pairwise_correlation_ranking(X, threshold):
    """
    Returns pairwise correlation of features ranked by absolute value, if above a certain threshold
    """

    corr = X.corr()

    corrdict = {}
    for i in range(len(corr)):
        for j in range(len(corr.columns)):
            if i != j and np.abs(corr.iloc[i, j] > threshold):
                corrdict[tuple(sorted([corr.columns[i], corr.columns[j]]))] = corr.iloc[i, j]
    return np.array(sorted(corrdict.items(), key=lambda x: np.abs(x[1]), reverse=True), dtype=object)


def filter_by_correlation(X, y, threshold):
    """
    Greedy filtering of features by correlation
    """
    correlations = pairwise_correlation_ranking(X, threshold)
    while len(correlations) > 0:
        worst_feature = mutual_information_ranking(X[list(correlations[0][0])], y).idxmin()
        X.drop(worst_feature, axis=1, inplace=True)
        correlations = pairwise_correlation_ranking(X, threshold)
    return X


if __name__ == '__main__':

    X_train, X_test, y_train, y_test = load_and_clean_data()
    X_train, X_val, y_train, y_val = train_test_split(X_train, y_train, test_size=.25, random_state=42)

    fpr, tpr, roc_auc, imp = run_baseline_dt(X_train, y_train, X_val, y_val)
    imp.reset_index(inplace=True)
    imp.drop(np.where(imp.importance == 0)[0], inplace=True)
    cols_to_keep = np.array(imp['index'])
    X_train = X_train[cols_to_keep]
    X_val = X_val[cols_to_keep]

    try:
        f = open("../data/feature_select.json", "r")
        feat_dict = dict(json.load(f))
        ## Close the json after reading it in so no shenanigans delete data
        f.close()
    except json.decoder.JSONDecodeError:
        print("Could not load JSON, restarting!")
        feat_dict = {}

    ## =====================##
    ## Determine what features to start with
    ## =====================##
    ## For simplicity's sake, I'm just going to do this with 'rfe'
    ## I think the same logic should apply for 'correlation'

    ## May be nice to have a simple way to start from scratch if need be
    start_over = False
    if start_over:
        feat_dict["correlation"] = {}

    if "correlation" not in feat_dict.keys():
        feat_dict["correlation"] = {}
        ## This will put the full feature set in the dictionary to start
        ## The AUC doesn't matter here, so I made it -1
        start_thresh = 1.0

    else:
        start_thresh = min([float(x) for x in feat_dict["correlation"].keys()])

    thresholds = np.arange(start_thresh, 0, -0.01)

    for t in thresholds:
        X_train_corr = filter_by_correlation(X_train, y_train, t)
        X_val_corr = X_val[np.array(X_train_corr.columns)]
        fpr, tpr, auc_corr, imp = run_baseline_dt(X_train_corr, y_train, X_val_corr, y_val)
        feat_dict['correlation'][t] = {"auc": auc_corr, "features": list(X_train_corr.columns)}
        f = open("../data/feature_select.json", "w+")
        f.write(json.dumps(feat_dict))
        f.close()
        print(f'Done with {t}!')
