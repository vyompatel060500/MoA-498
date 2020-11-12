#!/usr/bin/env python3

from openTSNE import TSNE
from openTSNE.affinity import PerplexityBasedNN
from openTSNE import initialization
from openTSNE import TSNEEmbedding
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


x_train = np.array(train_features.drop(columns=[
    'sig_id',
    'cp_type',
    'cp_dose',
    'cp_time'
  ]))

n_components = 4
tsne = TSNE(
    n_components=n_components, # https://github.com/pavlin-policar/openTSNE/issues/121
    negative_gradient_method='bh',
    perplexity=30,
    metric='euclidean',
    verbose=True,
    n_jobs=10,
    random_state=42
    )

embedding = tsne.fit(x_train)


# can embed new data:
# embedded_test = embedding.transform(np.array(test_features.drop(columns=[...])))

np.savetxt(f"tsne{n_components}dims.csv", embedding, delimiter=',', header=",".join([f'X{i}' for i in range(embedding.shape[1])]))



# ## Advanced embedding. https://opentsne.readthedocs.io/en/latest/examples/02_advanced_usage/02_advanced_usage.html

# affinities_train = PerplexityBasedNN(
#       x_train,
#       perplexity=30,
#       metric='euclidean',
#       n_jobs=10,
#       random_state=42
#     )

# affinity_init = initialization.pca(x_train, random_state=42)

# affinity_embedding = TSNEEmbedding(
#       affinity_init,
#       affinities_train,
#       n_components=3, # NOTE: DOESN'T DO ANYTHING!!
#       negative_gradient_method='bh',
#       n_jobs=10,
#       verbose=True
#     )


# np.savetxt("affinity_tsne.csv", affinity_embedding, delimiter=',', header=",".join([f'X{i}' for i in range(affinity_embedding.shape[1])]))

