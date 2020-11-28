CREATE TABLE IF NOT EXISTS JiangResults(
    ID INTEGER PRIMARY KEY, -- TODO: Does an index get created automatically?
    testlogloss FLOAT,
    trainlogloss FLOAT,
    eta INTEGER,
    nrounds INTEGER,
    num_parallel_tree INTEGER,
    max_depth INTEGER,
    max_delta_step INTEGER,
    colsample_bynode FLOAT,
    colsample_bylevel FLOAT,
    colsample_bytree FLOAT,
    subsample FLOAT,
    grow_policy TEXT,
    tree_method TEXT,
    booster TEXT,
    alpha FLOAT,
    lambda FLOAT,
    objective TEXT,
    with_pca INTEGER, -- bool
    with_important_only INTEGER, --bool
    drop_ctl INTEGER, -- bool
    DirectoryPath TEXT NOT NULL -- Directory path where all objects are stored.
);


-- TODO: Write R script to load all our datasets and add them as tables to the database??
--       Not worried about it yet, especially since we can not load them as is.















