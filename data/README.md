## We will post the necessary data files to use with notebooks in this directory

Course datasets that are stored in the `data/` directory of this repository.  

## Accessing Course Data
When working in Google Colab or another Python environment, datasets can be loaded directly from GitHub without downloading them manually by adding the following code cell to your notebook

### Python / Pandas

Define the course data location once:

```python
import pandas as pd

DATA_ROOT = (
    "https://raw.githubusercontent.com/"
    "acesillinois/cpsc-ese-practicum/main/data/"
)
```
Then load any dataset by filename:

```python
df = pd.read_csv(DATA_ROOT + "example.csv")
```

For tab-separated (.tsv) files:

```python
df = pd.read_csv(
    DATA_ROOT + "plant_traits.tsv",
    sep="\t"
)
```
