#!/usr/bin/env python3

from openTSNE import TSNE
import pandas as pd
import numpy as np

train_features = pd.read_csv('./lish-moa/train_features.csv')
test_features  = pd.read_csv('./lish-moa/test_features.csv')

# sample_submission.csv
# system.Rmd
# test_features.csv
# train_drug.csv
# train_features.csv
# train_features_describe_all.tsv
# train_targets_nonscored.csv
# train_targets_scored.csv


tsne = TSNE(
    n_components=3, # https://github.com/pavlin-policar/openTSNE/issues/121
    negative_gradient_method='bh',
    perplexity=30,
    metric='euclidean',
    verbose=True,
    n_jobs=10,
    random_state=42
    )

embedding = tsne.fit(np.array(train_features.drop(columns=[
  'sig_id',
  'cp_type',
  'cp_dose',
  'cp_time' 
  ])))


# can embed new data:
# embedded_test = embedding.transform(np.array(test_features.drop(columns=[...])))

np.savetxt("tsne.csv", embedding, delimiter=',', header=",".join([f'X{i}' for i in range(embedding.shape[1])]))

