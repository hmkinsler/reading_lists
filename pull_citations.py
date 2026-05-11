import pyalex
import pandas as pd

pyalex.config.api_key = "<YOUR_API_KEY>" # CHANGE TO PULL FROM ENV FILE FOR SECURITY

df = pd.read_csv("doi.csv")

results = {}

for _, row in df.iterrows():
    doi = row["doi"]
    title = row["title"]

    print(f"Processing: {title}")

    work_results = Works().filter(doi=doi).get()

    if not work_results:
        Print(f"{title} not found in OpenAlex")
        continue

    work = work_results[0]
    ref_ids = [r.split("/")[-1] for r in work.get("referenced_works", [])]

    if not ref_ids:
        print(f"No references found for {title}")
        continue

    refs = Works().filter(openalex="|".join(ref_ids)).get()
    results[doi] = {
        "title": title
        "references": refs
    }

    print(f"A total of {len(refs)} references were found in {title}")
