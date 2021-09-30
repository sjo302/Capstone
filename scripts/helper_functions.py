#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 30 15:58:38 2021

@author: saraokun, keshavpant, prestonlharry
"""
#imports 
import numpy as np
import pandas as pd


#helper functions
def set_nulls(data, cols):
    """
   @param data: dataframe
   @param cols: list of column names that have -99
   
   @return dataframe with -99 replaced with NaN
    """
    for c in cols:
        idx = np.where(data[c] == -99)[0]
        if len(idx) > 0:
            data[c].loc[idx] = np.nan
            
    return data

def map_cpt(data, column, replace, name):
    """
    @param data: dataframe
    @param column: string, column name
    @param replace: list of variables holding the values to be replaced by that particular variable name
    @ param name: string or integer of what will replace the values in replacements

    """
    for r in replace:
        idx = np.where(data[column] == r)[0]
        data[column].loc[idx] = name
    
    return data