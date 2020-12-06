import visuals
import pandas as pd
import numpy as np

t_data = pd.read_csv("../REPORT_DATA_FILES/tsne3dims.csv")
t_data = np.array(t_data)

labels = pd.read_csv('../lish-moa/train_targets_scored.csv')
drugs = pd.read_csv('../lish-moa/train_drug.csv')
number_o_ones = pd.read_csv('../REPORT_DATA_FILES/number_o_ones.csv')

drug_freqs = drugs.drug_id.value_counts()
top_drugs = drug_freqs[drug_freqs > 12]
# thresh  count  (total is 3289)
# 5       3086
# 6       312
# 7       116
# 8       112
# 9       112
# 10      112
# 11      108
# 12      44
# 13      19
# 14      13
# 18      10

import asyncio






labs_temp = pd.DataFrame(data=dict(top_drugs=top_drugs, labs=range(1,len(top_drugs)+1)))
labs_map = {k:k for k,v in zip(labs_temp.top_drugs.keys(), labs_temp.top_drugs.keys())}
labs = np.array([labs_map.get(k, 'default') for k in drugs.drug_id])
print("Proportion of samples belonging to top drugs:", sum(labs != 'default')/len(labs))

#asyncio.run(visuals.plot_GM_assignments_in_3d_new(t_data, labs))


#labs = drugs.drug_id
print(labs)

t_data = t_data[list(i for i,x in enumerate(labs) if x != 'default')]
labs = np.array([x for x in labs if x != 'default'])

#asyncio.run(visuals.plot_GM_assignments_in_3d_new(t_data, labs))





ones = number_o_ones
ll_thresh = sorted(ones.loglosses)[int(0.9 * len(ones.loglosses))]
hard_moas = [(idx,ones) for idx,ones,ll in zip(ones.Index, ones.ones, ones.loglosses) if ll > ll_thresh]
#drugs_by_ll =

t_data = t_data[list(i for i,x in enumerate(labs) if x != 'default')]
labs = np.array([x for x in labs if x != 'default'])


asyncio.run(visuals.plot_GM_assignments_in_3d_new(t_data, labs))

